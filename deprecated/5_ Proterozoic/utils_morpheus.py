#!/usr/bin/env python

import re
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

from scipy.spatial.distance import cosine

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
            # "MAX_COL": 6,
            "MAX_COL": 4, # set 2
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
        # dc = random.randint(3,self.init_settings["MAX_COL"])
        dc = random.randint(2,self.init_settings["MAX_COL"]) # set2

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
            # print("==sanity violation #1==")
            return False

        # 0.1) don't be equal
        if eq_r(p_example.input[0],p_example.output):
            return False

        # 0.2) if mutate, make sure not dividing on the same columns
        # def rec_check_mutate(p_current):
        #     if p_current.name=="mutate" and p_current.children[2].data==p_current.children[3].data:
        #         return False
        #     for i in range(len(p_current.children)):
        #         if isinstance(p_current.children[i], D.node.ApplyNode):
        #             if rec_check_mutate(p_current.children[i])==False:
        #                 return False
        #     return True
        # if rec_check_mutate(p_prog)==False:
        #     return False

        
        # 1) no two consecutive same components
        def rec_check_con(p_current):
            for i in range(len(p_current.children)):
                if isinstance(p_current.children[i], D.node.ApplyNode):
                    if p_current.name==p_current.children[i].name:
                        # print("GOT")
                        # print("{},{}".format(p_current.name,p_current.children[i].name))
                        return True
                    elif rec_check_con(p_current.children[i])==True:
                        return True
            return False
        if isinstance(p_prog, D.node.ApplyNode):
            ret_val = rec_check_con(p_prog)
            if ret_val==True:
                # print("==sanity violation #2==")
                return False

        # 1.x) for testing select->gather sequence
        # enforce this sequence
        # the LAST component must be select
        # and the no duplicate rule will make sure the second is select
        # for set2 testing only
        if isinstance(p_prog, D.node.ApplyNode):
            if p_prog.name!="select":
                return False

        # 1.1) no group_by in the last call
        # if isinstance(p_prog, D.node.ApplyNode):
        #     if p_prog.name=="group_by" or p_prog.name=="neg_group_by":
        #         # print("==sanity violation #3==")
        #         return False

        # 2) result should have at least 1x1 cell
        mr_script = '''
            ncol({})<=0
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # print("==sanity violation #4==")
            return False
        mr_script = '''
            nrow({})<=0
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # print("==sanity violation #5==")
            return False

        # 3) no numeric NA in any cell
        # mr_script = '''
        #     any(sapply({},function(x) is.na(x)))
        # '''.format(p_example.output)
        # ret_val = robjects.r(mr_script)
        # if True==ret_val[0]:
        #     # has <NA> or empty string
        #     return False

        # 3.1) no infinity in any cell
        # mr_script = '''
        #     any(sapply({},function(x) is.infinite(x)))
        # '''.format(p_example.output)
        # ret_val = robjects.r(mr_script)
        # if True==ret_val[0]:
        #     # has infinity or empty string
        #     return False

        # 4) no empty string in any cell, require: no <NA> first
        # mr_script = '''
        #     any(sapply({},function(x) x==''))
        # '''.format(p_example.output)
        # ret_val = robjects.r(mr_script)
        # if True==ret_val[0]:
        #     # has empty string
        #     return False

        # # 5) no NA as substring in any cell
        # mr_script = '''
        #     any(sapply({},function(x) grepl("NA",x)))
        # '''.format(p_example.output)
        # ret_val = robjects.r(mr_script)
        # if True==ret_val[0]:
        #     # has empty string
        #     return False

        # 6) no "COL" as substring in any cell
        # This is to prevent gather->spread pattern that
        # compares COL value in cell
        # mr_script = '''
        #     any(sapply({},function(x) grepl("COL",x)))
        # '''.format(p_example.output)
        # ret_val = robjects.r(mr_script)
        # if True==ret_val[0]:
        #     # has empty string
        #     return False

        # print("==sanity check: True==")
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

        # print("PASS assertion.")
        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- spread({table}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), col2=str(args[2]))
        # print("CODE: {}".format(_script))
        try:
            ret_val = robjects.r(_script)
            return ret_df_name
        except:
            # pritn("ERROR")
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
        _exp_cnt = 0
        while True:
            # print("trying...")
            try:
                tmp_prog = tmp_enumerator.next()
                # print("CAND:{}".format(tmp_prog))
                tmp_eval = self._interpreter.eval(
                    tmp_prog,
                    example.input,
                )
            # except StopIteration:
            #     print("STOP")
            #     continue
            except Exception:
                # print("EXCEPT")
                _exp_cnt += 1
                if _exp_cnt >= 10:
                    # exceed the limit consider changing example
                    return (None, None)
                continue
            tmp_example = Example(input=example.input, output=tmp_eval)
            if self._sfn(tmp_prog, tmp_example):
                    # print("YES")
                    return (
                        tmp_prog, 
                        tmp_example
                    )
            else:
                # important, also prevents infinite loop
                _exp_cnt += 1
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
def morpheus_select(p0_obj, p1_obj, verbose=False):

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

    abs_code = [0 for _ in range(20)]
    for i in range(min(20,len(ac0))):
        if ac0[i] in ac1:
            abs_code[i] = 1

    return abs_code

'''
cheating information for agent learning debugging
'''
def morpheus_gather(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr = robjects.r('nrow({})'.format(p0_obj))[0]
        dc = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr==0 or dc==0:
            np0_obj = numpy.asarray([[]])
            dr = 0
            dc = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr = 0
        dc = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr = robjects.r('nrow({})'.format(p1_obj))[0]
        dc = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr==0 or dc==0:
            np1_obj = numpy.asarray([[]])
            dr = 0
            dc = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr = 0
        dc = 0

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

    abs_code = [0 for _ in range(20)]
    lp1_obj = np1_obj.flatten().tolist()
    for i in range(min(20,len(ac0))):
        if ac0[i] in lp1_obj:
            abs_code[i] = 1

    return abs_code


'''
cheating information for agent learning debugging
'''
def morpheus_spread(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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


    # any source cell value from ith col appear in target col
    abs_code_1 = [0 for _ in range(20)]
    tmp_rec_i = set()
    for i in range(min(20,dc0)):
        for j in range(dr0):
            # notice: should match as string
            # print("dbg:{}".format(str(np0_obj[j,i])))
            # float==int, use "4" instead of "4.0"
            tmp_tgt = None
            if isinstance(np0_obj[j,i],numpy.float) and (not numpy.isnan(np0_obj[j,i])) and (not numpy.isinf(np0_obj[j,i])) and np0_obj[j,i]==int(np0_obj[j,i]):
                tmp_tgt = str(int(np0_obj[j,i]))
            else:
                tmp_tgt = str(np0_obj[j,i])
            if tmp_tgt in ac1:
                # abs_code_1[i] = 1
                abs_code_1[i] += min(1.,1./dr0)
                # also should index using string
                tmp_rec_i.add(ac1.index(tmp_tgt))

    # test whether cell values apper in col form tmp_rec_i
    abs_code_2 = [0 for _ in range(20)]
    tmp_rec_i = list(tmp_rec_i)
    for i in range(min(20,dc0)):
        for j in range(dr0):
            for k in tmp_rec_i:
                if np0_obj[j,i] in np1_obj[:,k]:
                    # abs_code_2[i] = 1
                    abs_code_2[i] += min(1.,1./dr0)
                    break

    return abs_code_1+abs_code_2




'''
cheating information for agent learning debugging
'''
def morpheus_unite(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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

    # splitting pre-process np1_obj
    sp1_obj = [
        [None for j in range(dc1)] for i in range(dr1)
    ]
    for i in range(dr1):
        for j in range(dc1):
            if isinstance(np1_obj[i,j],numpy.str):
                tmp_sp = np1_obj[i,j].split("_")
                if len(tmp_sp)>1:
                    sp1_obj[i][j] = tmp_sp

    abs_code = [0 for _ in range(20)]
    for i in range(min(20,dc0)):
        for j in range(dr0):
            tmp_src = None
            if isinstance(np0_obj[j,i],numpy.float) and (not numpy.isnan(np0_obj[j,i])) and (not numpy.isinf(np0_obj[j,i])) and np0_obj[j,i]==int(np0_obj[j,i]):
                tmp_src = str(int(np0_obj[j,i]))
            else:
                tmp_src = str(np0_obj[j,i])
            # check the target table
            is_break = False
            for i1 in range(dr1):
                if is_break:
                    break
                for j1 in range(dc1):
                    if sp1_obj[i1][j1] is None:
                        continue
                    if tmp_src in sp1_obj[i1][j1]:
                        # exist, and then determine the position
                        tmp_idx = sp1_obj[i1][j1].index(tmp_src)
                        # weighted by index
                        abs_code[i] += (tmp_idx+1)*(1./dr0)
                        is_break = True
                        break

    # print(sp1_obj)
    return abs_code

'''
cheating information for agent learning debugging
'''
def morpheus_separate(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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

    # splitting pre-process np0_obj
    sp0_obj = [
        [None for j in range(dc0)] for i in range(dr0)
    ]
    for i in range(dr0):
        for j in range(min(20,dc0)):
            if isinstance(np0_obj[i,j],numpy.float) and (not numpy.isnan(np0_obj[i,j])) and (not numpy.isinf(np0_obj[i,j])) and np0_obj[i,j]==int(np0_obj[i,j]):
                # tmp_sp = str(int(np0_obj[i,j])).split("_")
                tmp_sp = re.split("-|_|,|\.", str(int(np0_obj[i,j])))
            else:
                tmp_sp = re.split("-|_|,|\.", str(np0_obj[i,j]))
            # only preserve the first 2 groups
            sp0_obj[i][j] = tmp_sp[:2]

    abs_code = [0 for _ in range(20)]
    for i in range(dc1):
        for j in range(dr1):
            tmp_src = None
            if isinstance(np1_obj[j,i],numpy.float) and (not numpy.isnan(np1_obj[j,i])) and (not numpy.isinf(np1_obj[j,i])) and np1_obj[j,i]==int(np1_obj[j,i]):
                tmp_src = str(int(np1_obj[j,i]))
            else:
                tmp_src = str(np1_obj[j,i])
            # do not compare special tokens
            if len(tmp_src)==0 or "NA" in tmp_src:
                # further detect if all column values are "<NA>"
                # special condition for unsplittable columns
                if len(set(np1_obj[:,i]))==1:
                    # all column values are "<NA>"
                    # all unsplittable <NA> go to the right 1st column
                    abs_code[i-1] += 1.
                    break
                else:
                    # some are not "<NA>"
                    continue
            # check the target table
            for i1 in range(dr0):
                for j1 in range(min(20,dc0)):
                    if tmp_src in sp0_obj[i1][j1]:
                        # weighted, 1 split does not count
                        abs_code[j1] += (1./dr0) * (len(sp0_obj[i1][j1])-1)

    return abs_code

def morpheus_filter(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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

    # find out shared comparable columns
    shared_col_set = set(ac0[:min(20,dc0)]).intersection(set(ac1[:min(20,dc1)]))
    shared_col_lst = list(shared_col_set)
    shared_colid0 = [ac0.index(p) for p in shared_col_lst]
    shared_colid1 = [ac1.index(p) for p in shared_col_lst]

    # find out the filtered rows
    row_fp0 = [
        set([np0_obj[i,j] for j in shared_colid0]) for i in range(dr0)
    ] # row figure print
    row_fp1 = [
        set([np1_obj[i,j] for j in shared_colid1]) for i in range(dr1)
    ] # row figure print
    filtered_rowid0 = []
    for i in range(len(row_fp0)):
        if row_fp0[i] not in row_fp1:
            filtered_rowid0.append(i)

    # construct pattern
    filtered_pattern = [
        0 if i in filtered_rowid0 else 1 for i in range(dr0)
    ]

    pattern_list = [
        lambda x:x==0, lambda x:x==1, lambda x:x==2, lambda x:x==3, lambda x:x==4, lambda x:x==5, lambda x:x==6,
        lambda x:x>0, lambda x:x>1, lambda x:x>2, lambda x:x>3, lambda x:x>4, lambda x:x>5, lambda x:x>6,
        lambda x:x<0, lambda x:x<1, lambda x:x<2, lambda x:x<3, lambda x:x<4, lambda x:x<5, lambda x:x<6,
    ]

    # construct filter map
    np_fmp = numpy.zeros((len(pattern_list),20))
    for i in range(len(shared_col_lst)):
        d0col = shared_colid0[i]
        # d1col = shared_colid1[i]
        for k in range(len(pattern_list)):
            col_pattern = []
            for j in range(dr0):
                try:
                    tmp_cp = pattern_list[k](np0_obj[j,i])
                except Exception:
                    tmp_cp = False
                col_pattern.append(int(tmp_cp))
            if col_pattern==filtered_pattern:
                np_fmp[k,i] = 1
            # else: do nothing, remain 0

    if verbose:
        print(np_fmp)
    return np_fmp.flatten().tolist()


def morpheus_mutate(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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

    # find out the newly added columns
    newcol_set = set(ac1)-set(ac0)
    newcol_lst = list(newcol_set)
    newcol_idx = [ac1.index(i) for i in newcol_lst]

    abs_code = [0 for _ in range(20)]
    # perform division as in mutate
    for j0 in range(min(20,dc0)):
        for j1 in range(min(20,dc0)):
            for i in range(dr0):
                try:
                    tmp_res = np0_obj[i,j0]/np0_obj[i,j1]
                except Exception:
                    continue
                for k in newcol_idx:
                    if tmp_res in np1_obj[:,k]:
                        abs_code[j0] += 1./dr0
                        abs_code[j1] -= 1./dr0

    return abs_code


def morpheus_summarise(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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

    # get "grouping" information
    gr0 = robjects.r("ncol(group_keys({}))".format(p0_obj))[0]
    gc0 = robjects.r("nrow(group_keys({}))".format(p0_obj))[0]
    # if any of them is 0, then it's not grouped
    if gr0==0 or gc0==0:
        # set to target, if not grouping, shrink to 1x1
        gr0 = 1
        gc0 = 1
    else:
        # always one more, with grouping, add one column
        gc0 += 1
    # 2 bits for shape comparison (trigger)
    is_r = (gr0==dr1)
    is_c = (gc0==dc1)

    # find out the newly added columns
    newcol_set = set(ac1)-set(ac0)
    newcol_lst = list(newcol_set)
    newcol_idx = [ac1.index(i) for i in newcol_lst]

    np_fmp = numpy.zeros((3,20)) # feature map
    mmn = [None for _ in range(20)] # max, min, new
    for i in range(min(20,dc0)):
        if isinstance(np0_obj[0,i],numpy.float):
            tmp_tup = (
                max(np0_obj[:,i]), min(np0_obj[:,i]), set(np0_obj[:,i].tolist())
            )
            mmn[i] = tmp_tup
        else:
            continue

    for i in newcol_idx:
        for j in range(dr1):
            for k in range(min(20,dc0)):
                if mmn[k] is None:
                    continue
                dmax, dmin, dset = mmn[k]
                if np1_obj[j,i]==dmax:
                    np_fmp[0,k] += 1./dr1
                if np1_obj[j,i]==dmin:
                    np_fmp[1,k] += 1./dr1
                if np1_obj[j,i] not in dset:
                    np_fmp[2,k] += 1./dr1

    if verbose:
        print(mmn)

    return [is_r] + [is_c] + np_fmp.flatten().tolist()


def morpheus_group_by(p0_obj, p1_obj, verbose=False):

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr0 = robjects.r('nrow({})'.format(p0_obj))[0]
        dc0 = robjects.r('ncol({})'.format(p0_obj))[0]
        if dr0==0 or dc0==0:
            np0_obj = numpy.asarray([[]])
            dr0 = 0
            dc0 = 0
        else:
            np0_obj = numpy.asarray(robjects.r(p0_obj),dtype=numpy.object).T
    except Exception:
        np0_obj = numpy.asarray([[]])
        dr0 = 0
        dc0 = 0

    # get the table in numpy format
    try:
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr1 = robjects.r('nrow({})'.format(p1_obj))[0]
        dc1 = robjects.r('ncol({})'.format(p1_obj))[0]
        if dr1==0 or dc1==0:
            np1_obj = numpy.asarray([[]])
            dr1 = 0
            dc1 = 0
        else:
            np1_obj = numpy.asarray(robjects.r(p1_obj),dtype=numpy.object).T
    except Exception:
        np1_obj = numpy.asarray([[]])
        dr1 = 0
        dc1 = 0

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


    # get group keys if there's any
    try:
        gk0 = robjects.r("colnames(group_keys({table}))".format(table=p0_obj))
    except Exception:
        gk0 = []

    try:
        gk1 = robjects.r("colnames(group_keys({table}))".format(table=p1_obj))
    except Exception:
        gk1 = []

    # match gk1 with ac0
    abs_code = [0 for _ in range(20)]
    for i in range(len(ac0)):
        if ac0[i] in gk1:
            abs_code[i] = 1

    return abs_code



'''
cheating information for agent learning debugging
'''
def morpheus_set1(p0_obj, p1_obj, verbose=False):
    select_code = morpheus_select(p0_obj,p1_obj)
    gather_code = morpheus_gather(p0_obj,p1_obj)
    spread_code = morpheus_spread(p0_obj,p1_obj)
    unite_code = morpheus_unite(p0_obj,p1_obj)
    separate_code = morpheus_separate(p0_obj,p1_obj)
    group_by_code = morpheus_group_by(p0_obj,p1_obj)
    mutate_code = morpheus_mutate(p0_obj,p1_obj)
    filter_code = morpheus_filter(p0_obj,p1_obj)
    summarise_code = morpheus_summarise(p0_obj,p1_obj)
    return select_code+gather_code+spread_code+unite_code+separate_code+group_by_code+mutate_code+filter_code+summarise_code

    
'''
helper for getting numpy object and simple metas (nrow, ncol)
'''
def abs_get_np_obj(p_obj):
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
    return (np_obj, dr, dc)

def abs_get_col_names(p_obj):
    # just need to get column names
    try:
        ac = robjects.r("colnames({table})".format(table=p_obj))
    except Exception:
        # the same applies to this
        # then just create an empty list of column ignoreNames
        ac = []
    return ac

'''
string hashing function hash(s)= ( g(s[0])*5+g(s[-1])+1 ) / 25.
Notice: to lower first
'''
def abs_hash_string(s):
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
def abs_hash_float(x):
    dRight = int(x*10)-int(x)*10
    dLeft = int(x)-int(x/10)*10
    hash_val = abs( dLeft*10 + dRight )
    return round( float(hash_val + 1)/100., 4)

'''
cheating information for agent learning debugging
'''
def morpheus_select_abs(p0_obj, p1_obj, verbose=False):

    np0_obj, dr0, dc0 = abs_get_np_obj(p0_obj)
    np1_obj, dr1, dc1 = abs_get_np_obj(p1_obj)
    ac0 = abs_get_col_names(p0_obj)
    ac1 = abs_get_col_names(p1_obj)

    drm = max(dr0, dr1)
    dcm = max(dc0, dc1)

    h0_obj = numpy.zeros((20,20))
    for i in range(dr0):
        for j in range(dc0):
            if isinstance(np0_obj[i,j],numpy.str):
                h0_obj[i,j] = abs_hash_string(np0_obj[i,j])
            elif isinstance(np0_obj[i,j],numpy.float):
                if (not numpy.isnan(np0_obj[i,j])) and (not numpy.isinf(np0_obj[i,j])):
                    h0_obj[i,j] = abs_hash_float(np0_obj[i,j])

    h1_obj = numpy.zeros((20,20))
    for i in range(dr1):
        for j in range(dc1):
            if isinstance(np1_obj[i,j],numpy.str):
                h1_obj[i,j] = abs_hash_string(np1_obj[i,j])
            elif isinstance(np1_obj[i,j],numpy.float):
                if (not numpy.isnan(np1_obj[i,j])) and (not numpy.isinf(np1_obj[i,j])):
                    h1_obj[i,j] = abs_hash_float(np1_obj[i,j])

    sim_obj = numpy.zeros((20,20))
    # masked out range
    for i in range(min(dc0,20)):
        for j in range(min(dc1,20)):
            sim_obj[i,j] = 1-cosine(h0_obj[:,i],h1_obj[:,j])

    if verbose:
        print("h0_obj:")
        print(h0_obj)
        print("h1_obj:")
        print(h1_obj)
        print("sim:")
        print(sim_obj>=0.99)
        # print("sim_sum:")
        # print(numpy.sum((sim_obj>=0.99),axis=1))


    return (sim_obj>=0.99).flatten().tolist()
    # return (numpy.sum((sim_obj>=0.99),axis=1)).flatten().tolist()


CMM_NCOL = 20
CMM_NROW = 50
CMM_LIST = ["<PAD>","<NEW>","<DUP>","<OUT>"]
CMM_LIST +=["<rNEW>","<rDUP>","<rOUT>"]
CMM_LIST += ["<COL_{}>".format(i) for i in range(CMM_NCOL)]
CMM_LIST += ["<rCOL_{}>".format(i) for i in range(CMM_NCOL)]
CMM_DICT = {CMM_LIST[i]:i for i in range(len(CMM_LIST))}
def out_check(p_val):
    # check if the value belongs to <OUT>
    # True: it's out
    # False: it's NOT out
    if isinstance(p_val,numpy.float):
        if numpy.isnan(p_val) or numpy.isinf(p_val):
            return True
    return False

def construct_column_dict(p_obj, p_r, p_c):
    # p_obj: a numpy array
    # construct the column dictionary
    # excluding some unknown/uncared values
    ret_dic = {}
    for j in range(p_c):
        tmp_set = set()
        for i in range(p_r):
            if out_check(p_obj[i,j]):
                continue
            tmp_set.add(p_obj[i,j])
        ret_dic[j] = tmp_set
    return ret_dic

def get_cell2x_value(p_val, p_cd, now_cell=True):
    # p_val is expected to be a numpy type
    # p_cd: column dict generated by the method construct_column_dict
    dfound = []
    for i in range(CMM_NCOL):
        if i not in p_cd:
            # monotonically, break since it does not exist
            break
        if p_val in p_cd[i]:
            dfound.append(i)

    dtoken = None
    if len(dfound)==0:
        # could be <NEW> or <CMB> or <OUT>
        # we treat it as <NEW>/<OUT> temporarily
        if out_check(p_val):
            if now_cell:
                dtoken = "<OUT>"
            else:
                dtoken = "<rOUT>"
        else:
            if now_cell:
                dtoken = "<NEW>"
            else:
                dtoken = "<rNEW>"
    elif len(dfound)==1:
        # if it's found, it can't be <OUT>
        if now_cell:
            dtoken = "<COL_{}>".format(dfound[0])
        else:
            dtoken = "<rCOL_{}>".format(dfound[0])
    else:
        # appear in more than 2
        if now_cell:
            dtoken = "<DUP>"
        else:
            dtoken = "<rDUP>"

    return CMM_DICT[dtoken]





'''
trying the new column markup method to create a
column markup map
'''
def morpheus_select_cmm(p0_obj, p1_obj, verbose=False):

    np0_obj, dr0, dc0 = abs_get_np_obj(p0_obj)
    np1_obj, dr1, dc1 = abs_get_np_obj(p1_obj)
    ac0 = abs_get_col_names(p0_obj)
    ac1 = abs_get_col_names(p1_obj)

    assert len(ac0)==dc0
    assert len(ac1)==dc1

    # cell to cell mapping
    dmap = numpy.zeros((CMM_NROW,CMM_NCOL)) # 0 is <PAD>
    cdic = construct_column_dict(np0_obj,dr0,dc0)
    for i in range(min(CMM_NROW,dr1)):
        for j in range(min(CMM_NCOL,dc1)):
            dmap[i,j] = get_cell2x_value(np1_obj[i,j],cdic,now_cell=True)

    # cell to col mapping
    cmap = numpy.zeros((1,CMM_NCOL)) # map for columns
    for i in range(min(CMM_NCOL, dc1)):
        cmap[0,i] = get_cell2x_value(ac1[i],cdic,now_cell=False)

    rmap = numpy.vstack((cmap,dmap))


    if verbose:
        print(rmap)

    return rmap
    # (row, col) -> (col, row) / (dim, maxlen)
    # return dmap.T
    # return dmap.flatten().tolist()
    # return dmap[0,:].flatten().tolist()
