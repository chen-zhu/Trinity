#!/usr/bin/env python

'''
==== Morpheus Interpreter ====
==== Version/0714: Cambrian/Spriggina ====
'''

import re
import numpy
import random
import argparse
import hashlib
from collections import defaultdict
from tyrell import spec as S
from tyrell.interpreter import Interpreter, PostOrderInterpreter, GeneralError
from tyrell.enumerator import Enumerator, SmtEnumerator, RandomEnumerator, DesignatedEnumerator, RandomEnumeratorS, RandomEnumeratorFD
from tyrell.decider import Example, ExampleConstraintPruningDecider, ExampleDecider, TestDecider
from tyrell.synthesizer import Synthesizer
from tyrell.logger import get_logger

# initialize the shared R environment
from rpy2.robjects import r as GLO_RENV
GLO_RENV('''
library(compare)
library(dplyr)
library(tidyr)
''')

from sexpdata import Symbol
from tyrell import dsl as D
from typing import Callable, NamedTuple, List, Any

from scipy.spatial.distance import cosine
from itertools import product

from ProgramSpace import *

# load all words as candidate strings
# to make it more efficient, make it global
# so that it won't be loaded too many times
with open("./words.txt","r") as f:
    GLO_WORDS = f.readlines()
GLO_WORDS = [i.strip() for i in GLO_WORDS]

    
class MorpheusInterpreter(PostOrderInterpreter):
    def __init__(self):
        self.ID = "MI0001"
        self.counter = -1
        self.words = GLO_WORDS
        self.renv = GLO_RENV
        self.init_settings = {
            "MAX_VALUE": 100,
            "MIN_VALUE": -100,
            "MAX_ROW": 10, 
            "MAX_COL": 6,
            "MIN_ROW": 5,
            "MIN_COL": 3,
        }
        self.random_dicts = {
            # the first 3 ms are dumb
            "string": lambda n,m: random.choices(self.words, k=n),
            "string_und": lambda n,m: ["{}_{}".format(random.choice(self.words), random.choice(self.words)) for _ in range(n)],
            "float": lambda n,m: [random.uniform(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]) for _ in range(n)],
            "int": lambda n,m: random.choices(range(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]+1), k=n),
            # m cats, n instances
            "string_cat": lambda n,m: random.choices(
                random.choices(self.words, k=m), 
                k=n,
            ),
            "int_cat": lambda n,m: random.choices(
                random.choices(range(self.init_settings["MIN_VALUE"],self.init_settings["MAX_VALUE"]+1), k=m),
                k=n,
            )
        }
        self.camb_init()

    '''
    Cambrian/Charniodiscus Version:
    changing ignoreOrder to ignoreColOrder to enforce a strict equivalent
    which means no row re-permutation will be allowed
    since ignoreOrder have False Positive
    '''
    def equal(self, actual, expect):
        _rscript = '''
        tmp1 <- sapply({lhs}, as.character)
        tmp2 <- sapply({rhs}, as.character)
        compare(tmp1, tmp2, ignoreColOrder = TRUE, ignoreNames = TRUE)
        '''.format(lhs=actual, rhs=expect)
        # ignoreNames:TRUE, work for benchmark 23
        ret_val = self.renv(_rscript)
        return True == ret_val[0][0]

    def get_collist(self, sel):
        sel_str = ",".join(sel)
        return "c(" + sel_str + ")"

    def get_fresh_name(self):
        self.counter += 1
        fresh_str = '{}_RET_DF_{}'.format(
            self.ID, self.counter
        )
        return fresh_str

    def get_shadow_name(self, p_df):
        # get a shadow name from a name of data frame
        return p_df.replace('RET_DF',"SHADOW")

    '''
    wrapped hash function to generate consistent results in every hash session
    '''
    def hash(self, p_val):
        dstr = str(p_val).encode('utf8')
        return int(hashlib.md5(dstr).hexdigest(),16)

    def get_fresh_col(self, p_obj, p_ord):
        # The r_repr behaviors are different across platform
        # F---
        # dstr = self.renv(p_obj).r_repr()
        dstr = str(self.renv(p_obj))
        dhsh = (self.hash(dstr)*(p_ord+1))%len(self.words)
        fresh_str = self.words[dhsh]
        # fresh_str = '{}_COL_{}'.format(
        #     self.ID, dhsh+p_ord
        # )
        return fresh_str

    def get_type(self, df, index):
        _rscript = 'sapply({df_name}, class)[{pos}]'.format(df_name=df, pos=index)
        ret_val = self.renv(_rscript)
        return ret_val[0]

    def get_head(self, df):
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

    def print_obj(self,obj):
        print(self.renv(obj))

    def print_cmp(self,obj):
        print(self.renv("tmp1 <- sapply({}, as.character)".format(obj)))

    # method that generate random input table
    # CAMB: should also generate a shadow table
    def random_table(self):
        dr = random.randint(
            self.init_settings["MIN_ROW"],
            self.init_settings["MAX_ROW"],
        )
        dc = random.randint(
            self.init_settings["MIN_COL"],
            self.init_settings["MAX_COL"],
        )

        vlist = [
            self.random_dicts[
                random.choices(
                    ["string","float","int","string_cat","int_cat","string_und"],
                    weights=[2,2,2,2,2,1],
                    k=1,
                )[0]
            ](dr, random.choice([2,3,4,5]))
            for i in range(dc)
        ]

        tmp_c = []
        for i in range(len(vlist)):
            tmp_c.append(
                # initialized column names using natural words without "COL" prefix anymore
                "'{}'=c(".format(self.random_dicts["string"](1,None)[0]) + 
                ",".join(
                    ["'{}'".format(j) if isinstance(j,str) else "{:.2f}".format(j) for j in vlist[i]]
                ) + ")")

        # data frame script
        ref_df_name = self.get_fresh_name()
        mr_script = "{} <- data.frame({}, stringsAsFactors=FALSE)".format(
            ref_df_name ,",".join(tmp_c),
        )

        # shadow script
        dr = 1 # [OVERRIDE] we don't care about row changes
        shadow_script = "{} <- data.frame(matrix(0L, {}, {}))".format(
            self.get_shadow_name(ref_df_name), dr, dc
        )

        try:
            ret_val = self.renv(mr_script)
            _ = self.renv(shadow_script) # create shadow value
            return ref_df_name
        except:
            # logger.error('Error in generating random table...')
            raise GeneralError(mr_script)

    # CAMB: twin function for random_table
    # init a new shadow table
    # used in Trainer when loading from pworker file
    def create_shadow(self, obj):
        # dr = self.renv("nrow({})".format(obj))[0]
        dr = 1 # [OVERRIDE] we don't care about row changes
        dc = self.renv("ncol({})".format(obj))[0]
        shadow_script = "{} <- data.frame(matrix(0L, {}, {}))".format(
            self.get_shadow_name(obj), dr, dc,
        )
        try:
            _ = self.renv(shadow_script)
            # no return
        except:
            raise GeneralError()

    def load_data_into_var(self, pdata):
        pvar = self.get_fresh_name()
        self.renv("{} <- {}".format(pvar,pdata))
        self.create_shadow(pvar) # CAMB: create shadow
        return pvar


    # ==================================
    # ========== SANITY CHECK ==========
    # ==================================
    # Last Modified: Cambrian/Spriggina
    # sanity check should be performed on ProgramSpace object
    # NOTICE: can be performed on any ProgramSpace
    #         including incomplete ones
    # RETURN: (is_pass, failed_reason/None)
    def sanity_check(self, p_ps):
        try:

            # ==== Rule/IONEQ ====
            # inputs should not be equal to output
            for i in range(len(p_ps.inputs)):
                if self.equal(p_ps.inputs[i],p_ps.output):
                    return (False, "Rule/IONEQ")

            # ==== Rule/UTIL ====
            # utilization coefficient should be equal to #ApplyNode in all roots
            tmp_roots = p_ps.get_roots()
            for i in tmp_roots:
                tmp_prog = p_ps.node_list[i]
                tmp_psze = tmp_prog.__repr__().count("ApplyNode")
                tmp_shadow_max = self.renv("max({})".format(
                    self.get_shadow_name(tmp_prog.ps_data),
                    ))[0]
                if not tmp_shadow_max==tmp_psze:
                    return (False, "Rule/UTIL")

            # TODO
            # ==== Rule/MUTDIV ====
            # if using mutate, do not divide on the same columns
            
            # ==== Rule/NODUP ====
            # do not include two consecutive same components in the same AST
            # i.e., parent node and child node are not using the same component
            def rec_check_con(p_current):
                for i in range(len(p_current.children)):
                    if isinstance(p_current.children[i], D.node.ApplyNode):
                        if p_current.name==p_current.children[i].name:
                            return True
                        elif rec_check_con(p_current.children[i])==True:
                            return True
                return False
            tmp_roots = p_ps.get_roots()
            for i in tmp_roots:
                tmp_prog = p_ps.node_list[i]
                ret_val = rec_check_con(tmp_prog)
                if ret_val==True:
                    return (False, "Rule/NODUP")

            # TODO
            # ==== Rule/NOLGP ====
            # no group_by in the last call (root node)

            # ==== Rule/OBO ====
            # roots should have at least 1x1 cell
            tmp_roots = p_ps.get_roots()
            for i in tmp_roots:
                tmp_prog = p_ps.node_list[i]

                tmp_script = "ncol({})<=0".format(tmp_prog.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    return (False, "Rule/OBO")

                tmp_script = "nrow({})<=0".format(tmp_prog.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    return (False, "Rule/OBO")

            # ==== Rule/NONA ====
            # for **every** ApplyNode, no numeric NA is allowed
            # the ParamNode is already ensured in the random_table method
            for i in p_ps.node_dict["ApplyNode"]:
                tmp_node = p_ps.node_list[i]
                tmp_script = "any(sapply({},function(x) is.na(x)))".format(tmp_node.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    # has NA
                    return (False, "Rule/NONA")

            # ==== Rule/NOINF ====
            # for **every** ApplyNode, no numeric INF is allowed
            # the ParamNode is already ensured in the random_table method
            for i in p_ps.node_dict["ApplyNode"]:
                tmp_node = p_ps.node_list[i]
                tmp_script = "any(sapply({},function(x) is.infinite(x)))".format(tmp_node.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    # has INF
                    return (False, "Rule/NOINF")

            # ==== Rule/NOEMP ====
            # for **every** ApplyNode, no empty string is allowed
            # the ParamNode is already ensured in the random_table method
            for i in p_ps.node_dict["ApplyNode"]:
                tmp_node = p_ps.node_list[i]
                tmp_script = "any(sapply({},function(x) x==''))".format(tmp_node.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    # has empty string
                    return (False, "Rule/NOEMP")

            # ==== Rule/NOSNA ====
            # for **every** ApplyNode, no string NA is allowed
            # the ParamNode is already ensured in the random_table method
            for i in p_ps.node_dict["ApplyNode"]:
                tmp_node = p_ps.node_list[i]
                tmp_script = "any(sapply({},function(x) grepl('NA',x)))".format(tmp_node.ps_data)
                ret_val = self.renv(tmp_script)
                if True==ret_val[0]:
                    # has string NA
                    return (False, "Rule/NOSNA")

        except Exception:
            return (False, "Rule/EXCEPTION")

        return (True, None)

    
    # =================================
    # ========== EVAL SERIES ==========
    # =================================

    ## Concrete interpreter
    def eval_ColInt(self, v):
        return int(v)

    def eval_ColList(self, v):
        return v

    def eval_const(self, node, args):
        return args[0]

    def eval_select(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise to secure shadow mechanism
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError()

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
        shadow_script = '{ret_df} <- select({table}, {cols}) + 1'.format(
                         ret_df=self.get_shadow_name(ret_df_name), 
                         table=self.get_shadow_name(args[0]), 
                         cols=self.get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
            _ = self.renv(shadow_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    def eval_neg_select(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    def eval_unite(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
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
        # if args[1]==args[2], raise to secure shadow mechanism
        if args[1]==args[2]:
            raise GeneralError()

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- unite({table}, "{TMP}", {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], TMP=self.get_fresh_col(args[0],0), col1=str(args[1]), col2=str(args[2]))
        shadow_mincol = min(args[1],args[2])
        shadow_maxcol = max(args[1],args[2])
        shadow_maxcnt = self.renv('max({table}[,c({col1},{col2})])'.format(
                                    table=self.get_shadow_name(args[0]),
                                    col1=str(args[1]), 
                                    col2=str(args[2])))[0]
        shadow_script_0 = '{ret_shadow} <- select({table},{cols})'.format(
            ret_shadow=self.get_shadow_name(ret_df_name),
            table=self.get_shadow_name(args[0]),
            cols="c(-{})".format(shadow_maxcol),
            )
        shadow_script_1 = '{ret_shadow}[,{col}]={smax}'.format(
            ret_shadow=self.get_shadow_name(ret_df_name),
            col=shadow_mincol,
            smax="c({})".format(shadow_maxcnt+1),
            ) 
        try:
            ret_val = self.renv(_script)
            _ = self.renv(shadow_script_0)
            _ = self.renv(shadow_script_1)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    def eval_filter(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: self.get_type(args[0], str(x)) != 'factor',
                capture_indices=[0])

        ret_df_name = get_fresh_name()

        _script = '{ret_df} <- {table} %>% filter(.[[{col}]] {op} {const})'.format(
                  ret_df=ret_df_name, table=args[0], op=args[1], col=str(args[2]), const=str(args[3]))
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    def eval_separate(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=1,
                cond=lambda x: (self.get_type(args[0], str(x)) != 'numeric') and (self.get_type(args[0], str(x)) != 'integer'),
                capture_indices=[0])

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- separate({table}, {col1}, c("{TMP1}", "{TMP2}"))'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), TMP1=self.get_fresh_col(args[0],0), TMP2=self.get_fresh_col(args[0],1))
        shadow_maxcnt = self.renv('max({table}[,{col}])'.format(
                                    table=self.get_shadow_name(args[0]),
                                    col=str(args[1]), 
                                    ))[0]
        shadow_script_0 = '{ret_shadow} <- as.data.frame(append({table},c(-1),after={pos}))'.format(
            ret_shadow=self.get_shadow_name(ret_df_name),
            table=self.get_shadow_name(args[0]),
            pos=args[1],
            )
        shadow_script_1 = '{ret_shadow}[,{cols}]={val}'.format(
            ret_shadow=self.get_shadow_name(ret_df_name),
            cols="c({},{})".format(args[1],args[1]+1),
            val="c({})".format(shadow_maxcnt+1),
            ) 
        try:
            ret_val = self.renv(_script)
            _ = self.renv(shadow_script_0)
            _ = self.renv(shadow_script_1)
            return ret_df_name
        except:
            raise GeneralError()

    def eval_spread(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
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
        # if args[1]==args[2], raise to secure shadow mechanism
        if args[1]==args[2]:
            raise GeneralError()

        # print("PASS assertion.")
        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- spread({table}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, table=args[0], col1=str(args[1]), col2=str(args[2]))
        shadow_maxcnt = self.renv('max({table}[,c({col1},{col2})])'.format(
                                    table=self.get_shadow_name(args[0]),
                                    col1=str(args[1]), 
                                    col2=str(args[2])))[0]
        try:
            ret_val = self.renv(_script)

            # shadow of spread needs to be computed after concrete execution
            dc0 = self.renv("ncol({})".format(args[0]))[0]
            dc1 = self.renv("ncol({})".format(ret_df_name))[0]
            dc2 = dc1-(dc0-2) # number of new columns
            shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix(select({table},{cols})),{smax}))'.format(
                             ret_shadow=self.get_shadow_name(ret_df_name), 
                             table=self.get_shadow_name(args[0]), 
                             cols="c(-{},-{})".format(args[1],args[2]),
                             smax=",".join(["c({})".format(shadow_maxcnt+1) for _ in range(dc2)]), )
            _ = self.renv(shadow_script)

            return ret_df_name
        except:
            raise GeneralError()

    def eval_gather(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        # ======== Cambrian Version ========
        # if args[1]==args[2], raise to secure shadow mechanism
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError()
        # gather on columns that share the same type
        if len(args[1])==2:
            if self.get_type(args[0], str(args[1][0])) != self.get_type(args[0], str(args[1][1])):
                raise GeneralError()

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- gather({table}, "{KEY}", "{VALUE}", {cols})'.format(
                   ret_df=ret_df_name, table=args[0], KEY=self.get_fresh_col(args[0],0), VALUE=self.get_fresh_col(args[0],1), cols=self.get_collist(args[1]))
        shadow_maxcnt = self.renv('max({table}[,c({cols})])'.format(
                                    table=self.get_shadow_name(args[0]),
                                    cols=self.get_collist(args[1])))[0]
        shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix(select({table},{cols})),{smax}))'.format(
                         ret_shadow=self.get_shadow_name(ret_df_name), 
                         table=self.get_shadow_name(args[0]), 
                         cols="c({})".format(",".join(["-{}".format(i) for i in args[1]])),
                         smax=",".join(["c({})".format(shadow_maxcnt+1) for _ in range(2)]), )
        try:
            ret_val = self.renv(_script)
            _ = self.renv(shadow_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    def eval_neg_gather(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])

        ret_df_name = get_fresh_name()
        _script = '{ret_df} <- gather({table}, KEY, VALUE, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    # NOTICE: use the scoped version: group_by_at to support column index
    def eval_group_by(self, node, args):

        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])
        
        # removing this assertion for benchmark#6
        # self.assertArg(node, args,
        #         index=1,
        #                cond=lambda x: len(x) == 1,
        #         capture_indices=[0])

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    # NOTICE: use the scoped version: group_by_at to support column index
    def eval_neg_group_by(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
                capture_indices=[0])
        self.assertArg(node, args,
                index=1,
                       cond=lambda x: len(x) == 1,
                capture_indices=[0])

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    # TODO: need to fix get_fresh_col key word conflice, e.g., 'function'
    def eval_summarise(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: self.get_type(args[0], str(x)) == 'integer' or self.get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])

        # get column names
        colname = self.renv("colnames({table})".format(table=args[0]))[args[2]-1]

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- {table} %>% summarise({TMP} = {aggr} (`{col}`))'.format(
                  ret_df=ret_df_name, table=args[0], TMP=self.get_fresh_col(args[0],0), aggr=str(args[1]), col=colname)
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: need to fix get_fresh_col key word conflice, e.g., 'function'
    def eval_mutate(self, node, args):
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
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
                cond=lambda x: self.get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])
        self.assertArg(node, args,
                index=3,
                cond=lambda x: self.get_type(args[0], str(x)) == 'numeric',
                capture_indices=[0])

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- {table} %>% mutate({TMP}=.[[{col1}]] {op} .[[{col2}]])'.format(
                  ret_df=ret_df_name, table=args[0], TMP=self.get_fresh_col(args[0],0), op=args[1], col1=str(args[2]), col2=str(args[3]))
        shadow_maxcnt = self.renv('max({table}[,{cols}])'.format(
                                    table=self.get_shadow_name(args[0]),
                                    cols="c({},{})".format(args[2],args[3]),))[0]
        shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix({table}),{smax}))'.format(
                         ret_shadow=self.get_shadow_name(ret_df_name), 
                         table=self.get_shadow_name(args[0]), 
                         smax="c({})".format(shadow_maxcnt+1), )
        try:
            ret_val = self.renv(_script)
            _ = self.renv(shadow_script)
            return ret_df_name
        except:
            raise GeneralError()

    # TODO: add shadow script
    def eval_inner_join(self, node, args):
        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- inner_join({t1}, {t2})'.format(
                  ret_df=ret_df_name, t1=args[0], t2=args[1])
        try:
            ret_val = self.renv(_script)
            return ret_df_name
        except:
            raise GeneralError()

    # ==================================
    # ========== APPLY SERIES ==========
    # ==================================

    ## Abstract interpreter
    def apply_row(self, val):
        df = self.renv(val)
        return df.nrow

    def apply_col(self, val):
        df = self.renv(val)
        return df.ncol

    def apply_head(self, val):
        input_df = self.renv('input0')
        curr_df = self.renv(val)

        head_input = self.get_head(input_df)
        content_input = self.get_content(input_df)
        head_curr = self.get_head(curr_df)
        return len(head_curr - head_input - content_input)

    def apply_content(self, val):
        input_df = self.renv('input0')
        curr_df = self.renv(val)

        content_input = self.get_content(input_df)
        content_curr = self.get_content(curr_df)
        return len(content_curr - content_input)


    # ========================================
    # ========== ABSTRACTION SERIES ==========
    # ========================================
    # ========== Cambrian/Spriggina ==========
    def camb_init(self):
        '''
        <PAD>: padding token for all maps
        <EXT>: existing token
        <tNUM>: numeric type
        <tSTR>: string type
        <tUNK>: undefined type
        '''
        self.CAMB_NCOL = 15
        self.CAMB_NROW = 15
        # CAMB_LIST = ["<PAD>","<EXT>","<tNUM>","<tSTR>","<tUNK>"]
        self.CAMB_LIST = ["<PAD>"]
        self.CAMB_LIST += ["<STR_{}>".format(i) for i in range(49)]
        self.CAMB_LIST += ["<NUM_{}>".format(i) for i in range(100)]

        self.CAMB_DICT = {self.CAMB_LIST[i]:i for i in range(len(self.CAMB_LIST))}

    '''
    helper for getting numpy object and simple metas (nrow, ncol)
    '''
    def camb_get_np_obj(self, p_obj):
        # get the table in numpy format
        try:
            # deal with
            # "data frame with 0 columns and 10 rows"
            dr = self.renv('nrow({})'.format(p_obj))[0]
            dc = self.renv('ncol({})'.format(p_obj))[0]
            if dr==0 or dc==0:
                np_obj = numpy.asarray([[]])
                dr = 0
                dc = 0
            else:
                np_obj = numpy.asarray(self.renv(p_obj),dtype=numpy.object).T
        except Exception:
            np_obj = numpy.asarray([[]])
            dr = 0
            dc = 0
        return (np_obj, dr, dc)

    def camb_get_rc(self, p_obj):
        # deal with
        # "data frame with 0 columns and 10 rows"
        dr = self.renv('nrow({})'.format(p_obj))[0]
        dc = self.renv('ncol({})'.format(p_obj))[0]
        return (dr, dc)

    '''
    string hashing function hash(s)= g(s[0])*5+g(s[-1])+1
    total: 49 tags, 0~48
    Notice: to lower first
    '''
    # def str2hash(self, s):
    #     hash_gp = ["","abcde","fghij","klmno","pqrst","uvwxyz","0123456789"]
    #     hash_dt = {
    #         hash_gp[i][j]:i for i in range(len(hash_gp)) for j in range(len(hash_gp[i]))
    #     }
    #     ls = s.lower()
    #     if len(ls)==0:
    #         return 0
    #     hash_val = hash_dt.setdefault(ls[0],0)*len(hash_gp) + hash_dt.setdefault(ls[-1],0)
    #     return hash_val

    '''
    float hashing function hash(x)= dLeft(x)*10+dRight
    total: 100 tags, 0~99
    '''
    # def num2hash(self, x):
    #     if numpy.isnan(x) or numpy.isinf(x):
    #         return 0
    #     dx = abs(x)
    #     dRight = int(dx*10)-int(dx)*10
    #     dLeft = int(dx)-int(dx/10)*10
    #     hash_val = dLeft*10 + dRight
    #     return hash_val

    '''
    Cambrian/Spriggina Version
    packed in a numpy array
    '''
    # def camb_get_abs(self, p_obj):
    #     np_obj, dr, dc = self.camb_get_np_obj(p_obj)

    #     hash_map = numpy.zeros((self.CAMB_NROW,self.CAMB_NCOL),dtype=numpy.int)
    #     for i in range(min(self.CAMB_NROW,dr)):
    #         for j in range(min(self.CAMB_NCOL,dc)):
    #             if isinstance(np_obj[i,j],numpy.str):
    #                 hash_map[i,j] = self.CAMB_DICT["<STR_{}>".format(self.str2hash(np_obj[i,j]))]
    #             elif isinstance(np_obj[i,j],numpy.float):
    #                 hash_map[i,j] = self.CAMB_DICT["<NUM_{}>".format(self.num2hash(np_obj[i,j]))]

    #     return hash_map


    '''
    get the very simplified abstraction representation
    one-hot nrow
    one-hot ncol
    first row binary underscore
    '''
    # def camb_get_simp_abs(self, p_obj, verbose=False):
    #     np_obj, dr, dc = self.camb_get_np_obj(p_obj)

    #     one_hot_nrow = [0 for _ in range(self.CAMB_NROW)]
    #     if dr<=self.CAMB_NROW:
    #         one_hot_nrow[dr-1] = 1

    #     one_hot_ncol = [0 for _ in range(self.CAMB_NCOL)]
    #     if dc<=self.CAMB_NCOL:
    #         one_hot_ncol[dc-1] = 1

    #     bin_und = [0 for _ in range(self.CAMB_NCOL)]
    #     if dr>0:
    #         for i in range(min(dc,self.CAMB_NCOL)):
    #             if isinstance(np_obj[0,i],numpy.str):
    #                 if "_" in np_obj[0,i]:
    #                     bin_und[i] = 1

    #     if verbose:
    #         print(one_hot_nrow)
    #         print(one_hot_ncol)
    #         print(bin_und)

    #     return one_hot_nrow + one_hot_ncol + bin_und



    '''
    abstraction function for 0729 Sophia Series
    to solve collided states problem
    '''
    def camb_get_shash_abs(self, p_obj, verbose=False):
        np_obj, dr, dc = self.camb_get_np_obj(p_obj)

        one_hot_nrow = [0 for _ in range(self.CAMB_NROW)]
        if dr<=self.CAMB_NROW:
            one_hot_nrow[dr-1] = 1

        one_hot_ncol = [0 for _ in range(self.CAMB_NCOL)]
        if dc<=self.CAMB_NCOL:
            one_hot_ncol[dc-1] = 1

        bin_und = [0 for _ in range(self.CAMB_NCOL)]
        if dr>0:
            for i in range(min(dc,self.CAMB_NCOL)):
                if isinstance(np_obj[0,i],numpy.str):
                    if "_" in np_obj[0,i]:
                        bin_und[i] = 1

        one_hot_hash = [0 for _ in range(15)]
        df_hash_val = self.hash(str(self.renv(p_obj)))%15
        one_hot_hash[df_hash_val] = 1

        one_hot_rhash = [0 for _ in range(15)]
        df_rhash_val = self.hash(str(self.renv(p_obj))[::-1])%15
        one_hot_rhash[df_rhash_val] = 1

        if verbose:
            print(one_hot_nrow)
            print(one_hot_ncol)
            print(bin_und)
            print(one_hot_hash)
            print(one_hot_rhash)

        return one_hot_nrow + one_hot_ncol + bin_und + one_hot_hash + one_hot_rhash


    '''
    Cambrian/Yorgia Version
    in-between abstraction of partial table
    '''
    def camb_get_yorgia(self, p_obj, modn=128, verbose=False):
        np_obj, dr, dc = self.camb_get_np_obj(p_obj)

        table_abs_row_one = [0 for _ in range(self.CAMB_NCOL)]
        for j in range(min(self.CAMB_NCOL,dc)):
            table_abs_row_one[j] = self.hash(np_obj[0,j]) % modn
        table_abs_col_one = [0 for _ in range(self.CAMB_NROW)]
        for i in range(min(self.CAMB_NROW,dr)):
            table_abs_col_one[i] = self.hash(np_obj[i,0]) % modn

        if verbose:
            print(table_abs_row_one)
            print(table_abs_col_one)

        return table_abs_row_one + table_abs_col_one


    '''
    Cambrian/Yorgia Version
    in-between abstraction of partial table
    '''
    def camb_get_zong(self, p_obj, verbose=False):
        np_obj, dr, dc = self.camb_get_np_obj(p_obj)

        one_hot_nrow = [0 for _ in range(self.CAMB_NROW)]
        if dr<=self.CAMB_NROW:
            one_hot_nrow[dr-1] = 1

        one_hot_ncol = [0 for _ in range(self.CAMB_NCOL)]
        if dc<=self.CAMB_NCOL:
            one_hot_ncol[dc-1] = 1


        if verbose:
            print(one_hot_nrow)
            print(one_hot_ncol)

        return one_hot_nrow + one_hot_ncol



'''
Chain Execution Single Input No Branch
'''
class MorpheusGenerator(object):
    _spec: S.TyrellSpec
    _interpreter: Interpreter

    def __init__(self,
                 spec: S.TyrellSpec,
                 interpreter: Interpreter,):
        self._interpreter = interpreter
        self._spec = spec

    # generate a program and wrap it into a ProgramSpace and return
    def generate(self, fixed_depth, example, probs=(1,5)):
        tmp_enumerator = RandomEnumeratorFD(self._spec, fixed_depth = fixed_depth)
        _exp_cnt = 0
        while True:
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
                _exp_cnt += 1
                if _exp_cnt >= 10:
                    # exceed the limit consider changing example
                    return None
                continue
            tmp_example = Example(input=example.input, output=tmp_eval)
            tmp_ps = ProgramSpace(
                self._spec, self._interpreter, tmp_example.input, tmp_example.output,
            )
            # load the program into ProgramSpace
            tmp_ps.init_by_prog(tmp_prog)
            # do the sanity check
            tmp_check = self._interpreter.sanity_check(tmp_ps)
            if tmp_check[0]:
                return tmp_ps
            else:
                # important, also prevents infinite loop
                _exp_cnt += 1
                continue

    # wrap the trial-and-generate process
    def get_new_chain_program(self, fixed_depth):
        # initialize a program first
        while True:
            p_input = self._interpreter.random_table()
            p_ps = self.generate(
                fixed_depth=fixed_depth,
                example=Example(input=[p_input], output=None),
            )
            # make sure at least one function call
            if p_ps is not None:
                break
        return p_ps
   









    












