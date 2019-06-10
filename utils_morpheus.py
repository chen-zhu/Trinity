#!/usr/bin/env python

import numpy
import random
import argparse
from collections import defaultdict
from tyrell import spec as S
from tyrell.interpreter import Interpreter, PostOrderInterpreter, GeneralError
from tyrell.enumerator import Enumerator, SmtEnumerator, RandomEnumerator, DesignatedEnumerator, RandomEnumeratorS, RandomEnumeratorFD
from tyrell.decider import Example, ExampleConstraintPruningDecider, ExampleDecider, TestDecider
from tyrell.synthesizer import Synthesizer
from tyrell.logger import get_logger
import rpy2.robjects as robjects
from sexpdata import Symbol
from tyrell import dsl as D
from typing import Callable, NamedTuple, List, Any

logger = get_logger('tyrell')

counter_ = 1

# load all words as candidate strings
with open("./words.txt","r") as f:
    TMP_WORDS = f.readlines()
TMP_WORDS = [i.strip() for i in TMP_WORDS]

robjects.r('''
    library(compare)
    library(dplyr)
    library(tidyr)
   ''')

## Common utils.
def get_collist(sel):
    sel_str = ",".join(sel)
    return "c(" + sel_str + ")"

def get_fresh_name():
    global counter_ 
    counter_ = counter_ + 1

    fresh_str = 'RET_DF' + str(counter_)
    return fresh_str

def get_fresh_col():
    global counter_ 
    counter_ = counter_ + 1

    fresh_str = 'COL' + str(counter_)
    return fresh_str

def get_type(df, index):
    _rscript = 'sapply({df_name}, class)[{pos}]'.format(df_name=df, pos=index)
    ret_val = robjects.r(_rscript)
    return ret_val[0]

def eq_r(actual, expect):
    _rscript = '''
    tmp1 <- sapply({lhs}, as.character)
    tmp2 <- sapply({rhs}, as.character)
    compare(tmp1, tmp2, ignoreOrder = TRUE, ignoreNames = TRUE)
    '''.format(lhs=actual, rhs=expect)
    # ignoreNames:TRUE, work for benchmark 23
    # logger.info(robjects.r(actual))
    # logger.info(robjects.r(expect))
    ret_val = robjects.r(_rscript)
    return True == ret_val[0][0]

def get_head(df):
    head = set()
    for h in df.colnames:
        head.add(h)

    return head

def get_content(df):
    content = set()
    for vec in df:
        for elem in vec:
            e_val = str(elem)
            content.add(e_val)

    return content

    
class MorpheusInterpreter(PostOrderInterpreter):
    def __init__(self):
        self.init_settings = {
            "MAX_VALUE": 100,
            "MIN_VALUE": -100,
            "MAX_ROW": 10, 
            "MAX_COL": 6,
        }
        self.random_dicts = {
            # the first 3 ms are dumb
            "string": lambda n,m: random.choices(TMP_WORDS, k=n),
            "float": lambda n,m: [random.uniform(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]) for _ in range(n)],
            "int": lambda n,m: random.choices(range(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]+1), k=n),
            # m cats, n instances
            "string_cat": lambda n,m: random.choices(
                random.choices(TMP_WORDS, k=m), 
                k=n,
            ),
            "int_cat": lambda n,m: random.choices(
                random.choices(range(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]+1), k=m),
                k=n,
            )
        }

    # method that generate random input table
    def random_table(self):
        dr = random.randint(5,self.init_settings["MAX_ROW"])
        dc = random.randint(3,self.init_settings["MAX_COL"])

        vlist = [
            self.random_dicts[
                random.choices(
                    ["string","float","int","string_cat","int_cat"],
                    weights=[1,2,4,2,2],
                    k=1,
                )[0]
            ](dr, random.choice([2,3,4,5]))
            for i in range(dc)
        ]

        # print(vlist)

        tmp_c = []
        for i in range(len(vlist)):
            tmp_c.append("OCOL{}=c(".format(i+1) + ",".join(["'{}'".format(j) if isinstance(j,str) else "{:.2f}".format(j) for j in vlist[i]]) + ")")

        ref_df_name = get_fresh_name()
        mr_script = "{} <- data.frame({}, stringsAsFactors=FALSE)".format(
            ref_df_name ,",".join(tmp_c),
        )
        # print("CODE:")
        # print(mr_script)
        try:
            ret_val = robjects.r(mr_script)
            return ref_df_name
        except:
            # logger.error('Error in generating random table...')
            raise GeneralError()

    def load_data_into_var(self, pdata, pvar):
        robjects.r("{} <- {}".format(pvar,pdata))

    '''
    perform a check on intermediate output
    if fail, the trainer can directly assign negative reward and terminate the current episode
    '''
    def outv_check(self, p_obj):
        try:
            # deal with
            # "data frame with 0 columns and 10 rows"
            dr = robjects.r('nrow({})'.format(p_obj))[0]
            dc = robjects.r('ncol({})'.format(p_obj))[0]
            if dr==0 or dc==0:
                return False
            else:
                np_obj = numpy.asarray(robjects.r(p_obj),dtype=numpy.object).T
        except Exception:
            return False

        try:
            ac = robjects.r("colnames({table})".format(table=p_obj))
        except Exception:
            return False

        for p in np_obj.flatten():
            if isinstance(p,str):
                if len(p)==0:
                    return False
                if "NA" in p:
                    return False
            elif isinstance(p,numpy.float):
                if numpy.isnan(p) or numpy.isinf(p):
                    return False
            else:
                return False

        return True


    # Single Input, Single Output
    def sanity_check(self, p_prog, p_example):
        # 0) no do nothing
        if p_example.input[0]==p_example.output:
            return False
        def rec_check(p_current):
            for i in range(len(p_current.children)):
                if isinstance(p_current.children[i], D.node.ApplyNode):
                    if p_current.name==p_current.children[i].name:
                        # print("GOT")
                        # print("{},{}".format(p_current.name,p_current.children[i].name))
                        return True
                    elif rec_check(p_current.children[i])==True:
                        return True
            return False
        # 1) no two consecutive same components
        if isinstance(p_prog, D.node.ApplyNode):
            ret_val = rec_check(p_prog)
            if ret_val==True:
                return False
        # 1.1) no group_by in the last call
        if isinstance(p_prog, D.node.ApplyNode):
            if p_prog.name=="group_by" or p_prog.name=="neg_group_by":
                return False
        # 2) result should have at least 1x1 cell
        mr_script = '''
            ncol({})<=0
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            return False
        mr_script = '''
            nrow({})<=0
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            return False
        # 3) no numeric NA in any cell
        # mr_script = '''
        #     any(apply({},1,function(x) any(is.na(x))))
        mr_script = '''
            any(sapply({},function(x) is.na(x)))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has <NA> or empty string
            return False

        # 3.1) no infinity in any cell
        # mr_script = '''
        #     any(apply({},1,function(x) any(is.infinite(x))))
        mr_script = '''
            any(sapply({},function(x) is.infinite(x)))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has infinity or empty string
            return False

        # 4) no empty string in any cell, require: no <NA> first
        mr_script = '''
            any(sapply({},function(x) x==''))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has empty string
            return False
        # 5) no NA as substring in any cell
        mr_script = '''
            any(sapply({},function(x) grepl("NA",x)))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has empty string
            return False
        # 6) no "COL" as substring in any cell
        # This is to prevent gather->spread pattern that
        # compares COL value in cell
        mr_script = '''
            any(sapply({},function(x) grepl("COL",x)))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has empty string
            return False
        return True

    def print_obj(self,obj):
        print(robjects.r(obj))

    def print_cmp(self,obj):
        print(robjects.r("tmp1 <- sapply({}, as.character)".format(obj)))

    ## Concrete interpreter
    def eval_ColInt(self, v):
        return int(v)

    def eval_ColList(self, v):
        return v

    def eval_const(self, node, args):
        return args[0]

    def eval_select(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting select...')
            raise GeneralError()

    def eval_neg_select(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting select...')
            raise GeneralError()

    def eval_unite(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        first_idx = int(args[1])
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols and x != first_idx,
                capture_indices=[0, 1])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- unite({table}, {TMP}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(), col1=str(args[1]), col2=str(args[2]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting unite...')
            raise GeneralError()

    def eval_filter(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: get_type(args[0], str(x)) != 'factor',
                capture_indices=[0])

        ret_df_name = get_fresh_name()

        _script = '{ret_df} <- {table} %>% filter(.[[{col}]] {op} {const})'.format(
                  ret_df=ret_df_name, table=args[0], op=args[1], col=str(args[2]), const=str(args[3]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting filter...')
            raise GeneralError()

    def eval_separate(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- separate({table}, {col1}, c("{TMP1}", "{TMP2}"))'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), TMP1=get_fresh_col(), TMP2=get_fresh_col())
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting separate...')
            raise GeneralError()

    def eval_spread(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        first_idx = int(args[1])
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols and x > first_idx,
                capture_indices=[0, 1])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- spread({table}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), col2=str(args[2]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting spread...')
            raise GeneralError()

    def eval_gather(self, node, args):
        # input("PAUSE")
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- gather({table}, KEY, VALUE, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting gather...')
            raise GeneralError()

    def eval_neg_gather(self, node, args):
        # input("PAUSE")
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- gather({table}, KEY, VALUE, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting gather...')
            raise GeneralError()

    # NOTICE: use the scoped version: group_by_at to support column index
    def eval_group_by(self, node, args):

        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])
        
        # removing this assertion for benchmark#6
        # self.assertArg(node, args,
        #         index=1,
        #                cond=lambda x: len(x) == 1,
        #         capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting group_by...')
            raise GeneralError()

    # NOTICE: use the scoped version: group_by_at to support column index
    def eval_neg_group_by(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])
        self.assertArg(node, args,
                index=1,
                       cond=lambda x: len(x) == 1,
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting group_by...')
            raise GeneralError()

    def eval_summarise(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: get_type(args[0], str(x)) == 'integer' or get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])

        # get column names
        colname = robjects.r("colnames({table})".format(table=args[0]))[args[2]-1]

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- {table} %>% summarise({TMP} = {aggr} (`{col}`))'.format(
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(), aggr=str(args[1]), col=colname)
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting summarise...')
            raise GeneralError()

    def eval_mutate(self, node, args):
        n_cols = robjects.r('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=3,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])
        self.assertArg(node, args,
                index=3,
                cond=lambda x: get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- {table} %>% mutate({TMP}=.[[{col1}]] {op} .[[{col2}]])'.format(
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(), op=args[1], col1=str(args[2]), col2=str(args[3]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting mutate...')
            raise GeneralError()


    def eval_inner_join(self, node, args):
        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- inner_join({t1}, {t2})'.format(
                  ret_df=ret_df_name, t1=args[0], t2=args[1])
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # logger.error('Error in interpreting innerjoin...')
            raise GeneralError()

    ## Abstract interpreter
    def apply_row(self, val):
        df = robjects.r(val)
        return df.nrow

    def apply_col(self, val):
        df = robjects.r(val)
        return df.ncol

    def apply_head(self, val):
        input_df = robjects.r('input0')
        curr_df = robjects.r(val)

        head_input = get_head(input_df)
        content_input = get_content(input_df)
        head_curr = get_head(curr_df)
        return len(head_curr - head_input - content_input)

    def apply_content(self, val):
        input_df = robjects.r('input0')
        curr_df = robjects.r(val)

        content_input = get_content(input_df)
        content_curr = get_content(curr_df)
        return len(content_curr - content_input)

'''
Chain Execution Single Input No Branch
'''
class MorpheusGenerator(object):
    _spec: S.TyrellSpec
    _interpreter: Interpreter
    _sfn: Callable[[Any,Any], bool]

    def __init__(self,
                 spec: S.TyrellSpec,
                 interpreter: Interpreter,
                 sfn: Callable[[Any,Any], bool] = lambda pr,ex:True):
        self._interpreter = interpreter
        self._spec = spec
        self._sfn = sfn

    def generate(self, fixed_depth, example, probs=(1,5)):
        tmp_enumerator = RandomEnumeratorFD(self._spec, fixed_depth = fixed_depth)
        while True:
            try:
                tmp_prog = tmp_enumerator.next()
                # print("CAND:{}".format(tmp_prog))
                tmp_eval = self._interpreter.eval(
                    tmp_prog,
                    example.input,
                )
            except Exception:
                # print("EXCEPT")
                continue
            tmp_example = Example(input=example.input, output=tmp_eval)
            if self._sfn(tmp_prog, tmp_example):
                    # print("YES")
                    return (
                        tmp_prog, 
                        tmp_example
                    )
            else:
                continue

def init_tbl(df_name, csv_loc):
    cmd = '''
    tbl_name <- read.csv(csv_location, check.names = FALSE)
    fctr.cols <- sapply(tbl_name, is.factor)
    int.cols <- sapply(tbl_name, is.integer)
    tbl_name[, fctr.cols] <- sapply(tbl_name[, fctr.cols], as.character)
    tbl_name[, int.cols] <- sapply(tbl_name[, int.cols], as.numeric)
    '''
    cmd = cmd.replace('tbl_name', df_name).replace('csv_location', '"'+ csv_loc + '"')
    robjects.r(cmd)
    return None

'''
collect abstraction vector (col/row wise) and abstraction map (cell) for a given table
'''
def morpheus_abstraction(p_obj, verbose=False):
    '''
    string hashing function hash(s)= ( g(s[0])*5+g(s[-1])+1 ) / 25.
    Notice: to lower first
    '''
    def hash_string(s):
        hash_gp = ["","abcde","fghij","klmno","pqrst","uvwxyz","0123456789"]
        hash_dt = {
            hash_gp[i][j]:i for i in range(5) for j in range(len(hash_gp[i]))
        }
        ls = s.lower()
        hash_val = hash_dt.setdefault(ls[0],0)*7 + hash_dt.setdefault(ls[-1],0)
        return round( float(hash_val + 1)/49., 4)

    '''
    float hashing function hash(x)= ( dLeft(x)*10+dRight+1 ) / 100.
    '''
    def hash_float(x):
        dRight = int(x*10)-int(x)*10
        dLeft = int(x)-int(x/10)*10
        hash_val = abs( dLeft*10 + dRight )
        return round( float(hash_val + 1)/100., 4)

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr = robjects.r('nrow({})'.format(p_obj))[0]
        dc = robjects.r('ncol({})'.format(p_obj))[0]
        if dr==0 or dc==0:
            np_obj = numpy.asarray([[]])
            dr = 0
            dc = 0
        else:
            np_obj = numpy.asarray(robjects.r(p_obj),dtype=numpy.object).T
    except Exception:
        np_obj = numpy.asarray([[]])
        dr = 0
        dc = 0

    # abstraction settings
    abs_setting = {
        "MAX_ROW": 15,
        "MAX_COL": 15,
    }

    # the generated code to return
    abs_code = {
        "existence_row": None,
        "existence_col": None,
        "type_string_col": None,
        "type_float_col": None,
        "type_int_col": None,
        "categorical_col": None,
        "separator_col": None,
        "string_hashing_cell": None,
        "float_hashing_cell": None,
    }


    # 1) existence indicator (row/col wise)
    # 1: Yes
    # 0: No
    tmp_code = [1 if i<dr else 0 for i in range(abs_setting["MAX_ROW"])]
    abs_code["existence_row"] = tmp_code
    tmp_code = [1 if i<dc else 0 for i in range(abs_setting["MAX_COL"])]
    abs_code["existence_col"] = tmp_code

    # 2) string type indicator
    # 1: Yes (string)
    # 0: No (numeric or not applicable)
    tmp_code = [0 for _ in range(abs_setting["MAX_COL"])]
    for i in range(dc):
        # assume: every col shares a type
        if isinstance(np_obj[0,i],numpy.str):
            tmp_code[i] = 1
    abs_code["type_string_col"] = tmp_code

    # 3.1) float type indicator
    # 1: Yes (float)
    # 0: No (integer/string or not applicable)
    tmp_code = [0 for _ in range(abs_setting["MAX_COL"])]
    for i in range(dc):
        # assume every col shares a type
        if isinstance(np_obj[0,i],numpy.float):
            # try to determine equality to integer
            if int(np_obj[0,i])!=np_obj[0,i]:
                tmp_code[i] = 1
    abs_code["type_float_col"] = tmp_code

    # 3.2) integer type indicator
    # 1: Yes (integer)
    # 0: No (float/string or not applicable)
    tmp_code = [0 for _ in range(abs_setting["MAX_COL"])]
    for i in range(dc):
        # assume every col shares a type
        if isinstance(np_obj[0,i],numpy.float):
            # try to determine equality to integer
            if int(np_obj[0,i])==np_obj[0,i]:
                tmp_code[i] = 1
    abs_code["type_int_col"] = tmp_code

    # 4) categorical indicator
    # 1: Yes (categorical value)
    # 2: No (just value or not applicable)
    # if a specific value appears twice in the same column, then yes
    tmp_code = [0 for _ in range(abs_setting["MAX_COL"])]
    for i in range(dc):
        tmp_set = set()
        for j in range(dr):
            if np_obj[j,i] in tmp_set:
                tmp_code[i] = 1
                break
            else:
                tmp_set.add(np_obj[j,i])
    abs_code["categorical_col"] = tmp_code

    # 5) separator indicator
    # 1: Yes
    # 0: No
    # default separator: "_", should all cell in the same column contain it
    tmp_code = [0 for _ in range(abs_setting["MAX_COL"])]
    for i in range(dc):
        if isinstance(np_obj[0,i],numpy.str):
            tmp_is_sep = True
            for j in range(dr):
                tmp_sp = np_obj[j,i].split("_")
                if len(tmp_sp)<=1:
                    # fail
                    tmp_is_sep = False
                    break
            if tmp_is_sep:
                tmp_code[i] = 1
            # else: do nothing, remain 0
        else:
            # not applicable
            continue
    abs_code["separator_col"] = tmp_code

    # 6) string hashing
    tmp_code = numpy.zeros((abs_setting["MAX_ROW"],abs_setting["MAX_COL"]))
    for i in range(dr):
        for j in range(dc):
            if isinstance(np_obj[i,j],numpy.str):
                tmp_code[i,j] = hash_string(np_obj[i,j])
    abs_code["string_hashing_cell"] = tmp_code

    # 7) float hashing
    tmp_code = numpy.zeros((abs_setting["MAX_ROW"],abs_setting["MAX_COL"]))
    for i in range(dr):
        for j in range(dc):
            if isinstance(np_obj[i,j],numpy.float):
                tmp_code[i,j] = hash_float(np_obj[i,j])
    abs_code["float_hashing_cell"] = tmp_code

    if verbose:
        for dkey in abs_code:
            print("{}: {}".format(dkey,abs_code[dkey]))

    final_abs_code = []
    for dkey in abs_code:
        if isinstance(abs_code[dkey],numpy.ndarray):
            final_abs_code += abs_code[dkey].flatten().tolist()
        else:
            final_abs_code += abs_code[dkey]

    return final_abs_code


'''
cheating information for agent learning debugging
'''
def morpheus_cheating(p0_obj, p1_obj, verbose=False):

    # just need to get column names
    try:
        ac0 = robjects.r("colnames({table})".format(table=p0_obj))
    except Exception:
        # the same applies to this
        # then just create an empty list of column ignoreNames
        ac0 = []

    # just need to get column names
    try:
        ac1 = robjects.r("colnames({table})".format(table=p1_obj))
    except Exception:
        # the same applies to this
        # then just create an empty list of column ignoreNames
        ac1 = []

    abs_code = [0 for _ in range(10)]
    for i in range(len(ac0)):
        if ac0[i] in ac1:
            abs_code[i] = 1

    return abs_code



























    

