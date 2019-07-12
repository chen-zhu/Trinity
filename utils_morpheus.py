#!/usr/bin/env python

'''
Morpheus Utils: ==== Cambrian Version ====
- with some redundant codes removed (see deprecated/Proterozonic for old version)
'''

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
from itertools import product

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

def get_shadow_name(p_df):
    # get a shadow name from a name of data frame
    return p_df.replace('RET_DF',"SHADOW")


def get_fresh_col(p_obj,p_ord):
    dstr = robjects.r(p_obj).r_repr()
    dhsh = hash(dstr)%1000000
    # fresh_str = 'COL' + str(counter_)
    fresh_str = 'COL{}'.format(dhsh+p_ord)
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
            # "MAX_COL": 4, # set 2
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

    def equal(self,actual, expect):
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

    # method that generate random input table
    # CAMB: should also generate a shadow table
    def random_table(self):
        dr = random.randint(5,self.init_settings["MAX_ROW"])
        dc = random.randint(3,self.init_settings["MAX_COL"])
        # dc = random.randint(2,self.init_settings["MAX_COL"]) # set2

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
        dr = 1 # [OVERRIDE] we don't care about row changes
        shadow_script = "{} <- data.frame(matrix(0L, {}, {}))".format(
            get_shadow_name(ref_df_name), dr, dc
        )

        try:
            ret_val = robjects.r(mr_script)
            _ = robjects.r(shadow_script) # create shadow value
            return ref_df_name
        except:
            # logger.error('Error in generating random table...')
            raise GeneralError()

    # CAMB: twin function for random_table
    # init a new shadow table
    # used in Trainer when loading from pworker file
    def create_shadow(self, obj):
        dr = robjects.r("nrow({})".format(obj))[0]
        dr = 1 # [OVERRIDE] we don't care about row changes
        dc = robjects.r("ncol({})".format(obj))[0]
        shadow_script = "{} <- data.frame(matrix(0L, {}, {}))".format(
            get_shadow_name(obj), dr, dc,
        )
        try:
            _ = robjects.r(shadow_script)
            # no return
        except:
            raise GeneralError()

    def load_data_into_var(self, pdata, pvar):
        robjects.r("{} <- {}".format(pvar,pdata))
        self.create_shadow(pvar) # CAMB: create shadow

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

        # CAMB.0) check for utilization
        prog_size = p_prog.__repr__().count("ApplyNode")
        shadow_max = robjects.r("max({})".format(get_shadow_name(p_example.output)))[0]
        if not shadow_max==prog_size:
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

        # 1.x) for testing gather->select sequence
        # enforce this sequence
        # the LAST component must be select
        # and the no duplicate rule will make sure the second is select
        # for set2 testing only
        # if isinstance(p_prog, D.node.ApplyNode):
        #     if p_prog.name!="select":
        #         return False

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

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise
        # to secure shadow mechanism
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError()

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        shadow_script = '{} <- select({}, {}) + 1'.format(
                         get_shadow_name(ret_df_name), 
                         get_shadow_name(args[0]), 
                         get_collist(args[1]))
        try:
            ret_val = robjects.r(_script)
            _ = robjects.r(shadow_script)
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

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise
        # to secure shadow mechanism
        if args[1]==args[2]:
            raise GeneralError()

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- unite({table}, {TMP}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(args[0],0), col1=str(args[1]), col2=str(args[2]))
        # print("CODE: {}".format(_script))
        shadow_mincol = min(args[1],args[2])
        shadow_maxcol = max(args[1],args[2])
        shadow_maxcnt = robjects.r('max({table}[,c({col1},{col2})])'.format(
                                    table=get_shadow_name(args[0]),
                                    col1=str(args[1]), 
                                    col2=str(args[2])))[0]
        # print("BEGIN")
        shadow_script_0 = '{ret_shadow} <- select({table},{cols})'.format(
            ret_shadow=get_shadow_name(ret_df_name),
            table=get_shadow_name(args[0]),
            cols="c(-{})".format(shadow_maxcol),
            )
        shadow_script_1 = '{ret_shadow}[,{col}]={smax}'.format(
            ret_shadow=get_shadow_name(ret_df_name),
            col=shadow_mincol,
            smax="c({})".format(shadow_maxcnt+1),
            )
        # print("SHADOW-0: {}".format(shadow_script_0))
        # print("SHADOW-1: {}".format(shadow_script_1))   
        try:
            ret_val = robjects.r(_script)
            _ = robjects.r(shadow_script_0)
            _ = robjects.r(shadow_script_1)
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
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), TMP1=get_fresh_col(args[0],0), TMP2=get_fresh_col(args[0],1))
        # print("CODE: {}".format(_script))
        shadow_maxcnt = robjects.r('max({table}[,{col}])'.format(
                                    table=get_shadow_name(args[0]),
                                    col=str(args[1]), 
                                    ))[0]
        # print("BEGIN")
        shadow_script_0 = '{ret_shadow} <- as.data.frame(append({table},c(-1),after={pos}))'.format(
            ret_shadow=get_shadow_name(ret_df_name),
            table=get_shadow_name(args[0]),
            pos=args[1],
            )
        shadow_script_1 = '{ret_shadow}[,{cols}]={val}'.format(
            ret_shadow=get_shadow_name(ret_df_name),
            cols="c({},{})".format(args[1],args[1]+1),
            val="c({})".format(shadow_maxcnt+1),
            )
        # print("SHADOW-0: {}".format(shadow_script_0))
        # print("SHADOW-1: {}".format(shadow_script_1))   
        try:
            ret_val = robjects.r(_script)
            _ = robjects.r(shadow_script_0)
            _ = robjects.r(shadow_script_1)
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

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise
        # to secure shadow mechanism
        if args[1]==args[2]:
            raise GeneralError()

        # print("PASS assertion.")
        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- spread({table}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), col2=str(args[2]))
        # print("CODE: {}".format(_script))
        shadow_maxcnt = robjects.r('max({table}[,c({col1},{col2})])'.format(
                                    table=get_shadow_name(args[0]),
                                    col1=str(args[1]), 
                                    col2=str(args[2])))[0]
        try:
            ret_val = robjects.r(_script)

            # shadow of spread needs to be computed after concrete execution
            dc0 = robjects.r("ncol({})".format(args[0]))[0]
            dc1 = robjects.r("ncol({})".format(ret_df_name))[0]
            dc2 = dc1-(dc0-2) # number of new columns
            # print("OK")
            shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix(select({table},{cols})),{smax}))'.format(
                             ret_shadow=get_shadow_name(ret_df_name), 
                             table=get_shadow_name(args[0]), 
                             cols="c(-{},-{})".format(args[1],args[2]),
                             smax=",".join(["c({})".format(shadow_maxcnt+1) for _ in range(dc2)]), )
            # print("SHADOW_SPREAD: {}".format(shadow_script))
            _ = robjects.r(shadow_script)

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

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise
        # to secure shadow mechanism
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError()

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- gather({table}, KEY, VALUE, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        # print("CODE: {}".format(_script))
        shadow_maxcnt = robjects.r('max({table}[,c({cols})])'.format(
                                    table=get_shadow_name(args[0]),
                                    cols=get_collist(args[1])))[0]
        # print("OH?")
        shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix(select({table},{cols})),{smax}))'.format(
                         ret_shadow=get_shadow_name(ret_df_name), 
                         table=get_shadow_name(args[0]), 
                         cols="c({})".format(",".join(["-{}".format(i) for i in args[1]])),
                         smax=",".join(["c({})".format(shadow_maxcnt+1) for _ in range(2)]), )
        # print("SHADOW_GATHER: {}".format(shadow_script))
        try:
            ret_val = robjects.r(_script)
            _ = robjects.r(shadow_script)
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
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(args[0],0), aggr=str(args[1]), col=colname)
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
                  ret_df=ret_df_name, table=args[0], TMP=get_fresh_col(args[0],0), op=args[1], col1=str(args[2]), col2=str(args[3]))
        # print("CODE: {}".format(_script))
        shadow_maxcnt = robjects.r('max({table}[,{cols}])'.format(
                                    table=get_shadow_name(args[0]),
                                    cols="c({},{})".format(args[2],args[3]),))[0]
        # print("BEGIN")
        shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix({table}),{smax}))'.format(
                         ret_shadow=get_shadow_name(ret_df_name), 
                         table=get_shadow_name(args[0]), 
                         smax="c({})".format(shadow_maxcnt+1), )
        # print("SHADOW_MUTATE: {}".format(shadow_script))
        try:
            ret_val = robjects.r(_script)
            _ = robjects.r(shadow_script)
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
========================================================================
========================================================================
==================== Starting Main Cambrian Version ====================
============================= abbr.: camb. =============================
========================================================================
========================================================================
'''

'''
helper for getting numpy object and simple metas (nrow, ncol)
'''
def camb_get_np_obj(p_obj):
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
            # np_obj = numpy.asarray(robjects.r(p_obj),dtype=numpy.str).T
    except Exception:
        np_obj = numpy.asarray([[]])
        dr = 0
        dc = 0
    return (np_obj, dr, dc)

'''
string hashing function hash(s)= g(s[0])*5+g(s[-1])+1
total: 49 tags, 0~48
Notice: to lower first
'''
def str2hash(s):
    hash_gp = ["","abcde","fghij","klmno","pqrst","uvwxyz","0123456789"]
    hash_dt = {
        hash_gp[i][j]:i for i in range(len(hash_gp)) for j in range(len(hash_gp[i]))
    }
    ls = s.lower()
    if len(ls)==0:
        return 0
    hash_val = hash_dt.setdefault(ls[0],0)*len(hash_gp) + hash_dt.setdefault(ls[-1],0)
    return hash_val

'''
float hashing function hash(x)= dLeft(x)*10+dRight
total: 100 tags, 0~99
'''
def num2hash(x):
    if numpy.isnan(x) or numpy.isinf(x):
        return 0
    dx = abs(x)
    dRight = int(dx*10)-int(dx)*10
    dLeft = int(dx)-int(dx/10)*10
    hash_val = dLeft*10 + dRight
    return hash_val

'''
<PAD>: padding token for all maps
<EXT>: existing token
<tNUM>: numeric type
<tSTR>: string type
<tUNK>: undefined type
'''
CAMB_NCOL = 15
CAMB_NROW = 15
# CAMB_LIST = ["<PAD>","<EXT>","<tNUM>","<tSTR>","<tUNK>"]
CAMB_LIST = ["<PAD>"]
CAMB_LIST += ["<STR_{}>".format(i) for i in range(49)]
CAMB_LIST += ["<NUM_{}>".format(i) for i in range(100)]


CAMB_DICT = {CAMB_LIST[i]:i for i in range(len(CAMB_LIST))}

'''
Cambrian/Charniodiscus Version
just returning all non-deterministic abstraction maps
packed in a numpy array
(n_maps, map_r, map_c) === (n_maps, CAMB_NROW, CAMB_NCOL)
'''
def camb_get_abs(p_obj):
    np_obj, dr, dc = camb_get_np_obj(p_obj)

    hash_map = numpy.zeros((CAMB_NROW,CAMB_NCOL),dtype=numpy.int)
    # existence_map = numpy.zeros((CAMB_NROW,CAMB_NCOL),dtype=numpy.int)
    # type_map = numpy.zeros((CAMB_NROW,CAMB_NCOL),dtype=numpy.int)
    for i in range(min(CAMB_NROW,dr)):
        for j in range(min(CAMB_NCOL,dc)):
            # existence_map[i,j] = CAMB_DICT["<EXT>"]
            # if isinstance(np_obj[i,j],numpy.str):
            #     type_map[i,j] = CAMB_DICT["<tSTR>"]
            # elif isinstance(np_obj[i,j],numpy.float):
            #     type_map[i,j] = CAMB_DICT["<tNUM>"]
            # else:
            #     type_map[i,j] = CAMB_DICT["<tUNK>"]
            if isinstance(np_obj[i,j],numpy.str):
                hash_map[i,j] = CAMB_DICT["<STR_{}>".format(str2hash(np_obj[i,j]))]
            elif isinstance(np_obj[i,j],numpy.float):
                hash_map[i,j] = CAMB_DICT["<NUM_{}>".format(num2hash(np_obj[i,j]))]

    # (n_maps, CAMB_NROW, CAMB_NCOL)
    # return numpy.asarray([existence_map,type_map])
    return hash_map


    












