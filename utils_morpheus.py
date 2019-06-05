#!/usr/bin/env python

import numpy
import random
import argparse
from tyrell import spec as S
from tyrell.interpreter import Interpreter, PostOrderInterpreter, GeneralError
from tyrell.enumerator import Enumerator, SmtEnumerator, RandomEnumerator, DesignatedEnumerator, RandomEnumeratorS
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
            logger.error('Error in generating random table...')
            raise GeneralError()

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
        mr_script = '''
            any(apply({},1,function(x) any(is.na(x))))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has <NA> or empty string
            return False
        # print("SANITY:{}".format(ret_val[0]))
        # 4) no empty string in any cell, require: no <NA> first
        mr_script = '''
            any(apply({},1,function(x) any(x=='')))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has empty string
            return False
        # 5) no NA as substring in any cell
        mr_script = '''
            any(apply({},1,function(x) any(grepl("NA",x))))
        '''.format(p_example.output)
        ret_val = robjects.r(mr_script)
        if True==ret_val[0]:
            # has empty string
            return False
        # 6) no "COL" as substring in any cell
        # This is to prevent gather->spread pattern that
        # compares COL value in cell
        mr_script = '''
            any(apply({},1,function(x) any(grepl("COL",x))))
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
            logger.error('Error in interpreting select...')
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
            logger.error('Error in interpreting select...')
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
            logger.error('Error in interpreting unite...')
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
            logger.error('Error in interpreting filter...')
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
            logger.error('Error in interpreting separate...')
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
            logger.error('Error in interpreting spread...')
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
            logger.error('Error in interpreting gather...')
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
            logger.error('Error in interpreting gather...')
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
            logger.error('Error in interpreting group_by...')
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
            logger.error('Error in interpreting group_by...')
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
            logger.error('Error in interpreting summarise...')
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
            logger.error('Error in interpreting mutate...')
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
            logger.error('Error in interpreting innerjoin...')
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

    def generate(self, max_depth, example, probs=(1,5)):
        tmp_enumerator = RandomEnumeratorS(self._spec, max_depth = max_depth, probs=probs)
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
collect 1d meta information into feature vector
'''
def morpheus_perspective1(p_obj, verbose=False):
    '''
    perspective collections for 1 object
    all YES(1)/NO(0) questions
    '''
    np_obj = numpy.asarray(robjects.r(p_obj)).T
    # np_obj = numpy.asarray(p_obj,dtype=numpy.object).T
    ac = robjects.r("colnames({table})".format(table=p_obj))
    # ac = self.r_colnames("table",p_obj)
    dr, dc = np_obj.shape

    pcode = []

    # (20 bits) What's the shape? (0~9,0~9), 0 for out of bound dimension
    tmp_code_r = [0 for _ in range(10)]
    tmp_code_c = [0 for _ in range(10)]
    if dr>=10:
        tmp_code_r[0] = 1
    else:
        tmp_code_r[dr] = 1
    if dc>=10:
        tmp_code_c[0] = 1
    else:
        tmp_code_c[dc] = 1
    pcode += tmp_code_r
    pcode += tmp_code_c
    if verbose:
        print("# #rows:{}, #cols:{}".format(dr, dc))

    
    tmp_STR = 0 # Does the table contain string?
    tmp_ESTR = 0 # Does the table contain empty string?
    tmp_NASTR = 0 # Does the table contain the string "NA"?
    tmp_NAN = 0 # Does the table contain np.nan?
    tmp_UNDERSCORE = 0 # Does the table contain underscore?
    tmp_SEP = 0 # Does the table contain separator?
    for i in np_obj.flatten():
        if isinstance(i,str):
            tmp_STR = 1
            if len(i)==0:
                tmp_ESTR = 1
            if "NA" in i:
                tmp_NASTR = 1
            if "_" in i:
                tmp_UNDERSCORE = 1
            if ("_" in i) or ("-" in i) or ("." in i) or ("," in i) or ("/" in i) or ("|" in i):
                tmp_SEP = 1
        if isinstance(i,numpy.float) and numpy.isnan(i):
            tmp_NAN = 1
    pcode += [tmp_STR, tmp_ESTR, tmp_NASTR, tmp_NAN, tmp_UNDERSCORE, tmp_SEP]
    if verbose:
        print("# STR:{}, ESTR:{}, NASTR:{}, NAN:{}, UND:{}, SEP:{}".format(
            tmp_STR, tmp_ESTR, tmp_NASTR, tmp_NAN, tmp_UNDERSCORE, tmp_SEP
        ))

    tmp_cUNDERSCORE = 0 # Does the table column contain underscore?
    for ds in ac:
        if "_" in ac:
            tmp_cUNDERSCORE = 1
            break
    pcode += [tmp_cUNDERSCORE]
    if verbose:
        print("# cUND:{}".format(
            tmp_cUNDERSCORE,
        ))

    return pcode
