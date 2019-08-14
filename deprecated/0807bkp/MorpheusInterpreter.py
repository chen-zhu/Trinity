#!/usr/bin/env python

'''
==== Morpheus Interpreter ====
==== Version/0803: Cambrian/Ventogyrus ====
'''

import re
import numpy
import random
import argparse
import hashlib
import itertools
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
        self.ID = "MI0002"
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
        self.shadow_dict = {} # store shadow traces
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

    # generated random table targeted at specific function call
    # ==== NOTICE: only applies for size 1 program ====
    def random_table_for_call(self, p_prog, dbg_name=None):
        if dbg_name is not None:
            d_name = dbg_name
        else:
            d_name = p_prog.name
        if d_name=="spread":
            d_arg_key = int(p_prog.args[1].data)
            d_arg_val = int(p_prog.args[2].data)
            if d_arg_key==d_arg_val:
                raise GeneralError("Random table (spread) assertion failed.")
            # ==== general process to generate a spread-fit table ====
            # (assume no spread(a,a) with the same key/val column id)
            # -> otherwise, hand over to random_table
            # 1. key should be cat, val can be any type
            # 2. except for key/val, other columns should be cats (cat group)
            # 3. every key and cat group id form an entry
            # -. cat group ids may not be complete, but key/id pairs should be complete
            dc = random.randint(
                self.init_settings["MIN_COL"],
                self.init_settings["MAX_COL"],
            )
            d_arg_max = max(d_arg_key,d_arg_val,dc)
            if d_arg_max<=2:
                d_arg_max = 3 # should have at least 1 id group
            # the number of columns can not be sampled, but computed via the process
            # according to heuristics, key column can only be of type "string_cat"
            key_column_set = set(
                random.choices(
                    self.words, 
                    k=random.choice([2,3,4,5]),
                )
            )
            n_idg = d_arg_max-2
            idg_column_sets = [
                set(
                    random.choices(
                        self.words,
                        k=random.choice([2,3]),
                    )
                )
                for _ in range(n_idg)
            ]
            idg_ids = list(itertools.product(*idg_column_sets))
            random.shuffle(idg_ids)
            selected_ids = idg_ids[:random.choice([2,3,4])]
            key_ids_pairs = list(itertools.product(
                key_column_set,
                selected_ids,
            ))
            val_column = self.random_dicts[
                random.choices(
                    ["string","float","int","string_cat","int_cat","string_und"],
                    weights=[2,2,2,2,2,1],
                    k=1,
                )[0]
            ](len(key_ids_pairs), random.choice([2,3,4,5]))

            # then re-organize every thing into vlist
            vlist = [[] for _ in range(d_arg_max)]
            vlist[d_arg_val-1] = val_column
            # put key in first
            for i in range(len(key_ids_pairs)):
                vlist[d_arg_key-1].append(
                    key_ids_pairs[i][0]
                )
            map_list = [i for i in range(d_arg_max) if i!=d_arg_key-1 and i!=d_arg_val-1]
            for i in range(len(key_ids_pairs)):
                for j in range(len(key_ids_pairs[i][1])):
                    vlist[
                        map_list[j]
                    ].append(
                        key_ids_pairs[i][1][j]
                    )
            # done constructing vlist

            # then copy the code from random_table()
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

            try:
                ret_val = self.renv(mr_script)
                # shadow script
                self.create_shadow(ref_df_name)
                return ref_df_name
            except:
                # logger.error('Error in generating random table...')
                raise GeneralError("Random table (spread) execution failed: {}".format(mr_script))

        else:
            return self.random_table()

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

        try:
            ret_val = self.renv(mr_script)
            # shadow script
            self.create_shadow(ref_df_name)
            return ref_df_name
        except:
            # logger.error('Error in generating random table...')
            raise GeneralError("Random table failed: {}".format(mr_script))

    # create a shadow list of the same number of columns with the host table
    def create_shadow(self, obj):
        dc = self.renv("ncol({})".format(obj))[0]
        self.shadow_dict[obj] = [0 for _ in range(dc)]

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
        # ==== Rule/IONEQ ====
        # inputs should not be equal to output
        for i in range(len(p_ps.inputs)):
            if self.equal(p_ps.inputs[i],p_ps.output):
                return (False, "Rule/IONEQ")

        # ==== Rule/NOSC ====
        # NOTICE: simple and easy version, may not cure the effect
        # no short-cut rule
        # if any intermediate nodes evaluate equal to input
        # then some path is redundant
        for tmp_node in p_ps.node_list:
            if tmp_node.is_apply():
                for tmp_input in p_ps.inputs:
                    if self.equal(tmp_node.ps_data, tmp_input):
                        return (False, "Rule/NOSC")

        # ==== Rule/UTIL ====
        # utilization coefficient should be equal to #ApplyNode in all roots
        tmp_roots = p_ps.get_roots()
        for i in tmp_roots:
            tmp_prog = p_ps.node_list[i]
            tmp_psze = tmp_prog.__repr__().count("ApplyNode")
            tmp_shadow_max = max(self.shadow_dict[tmp_prog.ps_data])
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

        # ==== Rule/SEND ====
        # only one select and it should be at last
        tmp_roots = p_ps.get_roots()
        for i in tmp_roots:
            tmp_prog = p_ps.node_list[i]
            tmp_psze = tmp_prog.__repr__().count("select")
            if tmp_psze>1:
                return (False, "Rule/SEND")
            elif tmp_psze==1:
                if p_ps.node_list[i].name!="select":
                    return (False, "Rule/SEND")

        # ==== Rule/CUNI ====
        # clues of unite operation should be observable
        # Notice: even with this rule, there's still outliers
        # e.g., unite on 3,4 and then separate, but the original input has underscore
        tmp_roots = p_ps.get_roots()
        for i in tmp_roots:
            tmp_prog = p_ps.node_list[i]
            if "unite" in tmp_prog.__repr__():
                tmp_script = "any(sapply({},function(x) grepl('_',x)))".format(tmp_prog.ps_data)
                ret_val = self.renv(tmp_script)
                if False==ret_val[0]:
                    # do not have underscore
                    return (False, "Rule/CUNI")

        # TODO
        # ==== Rule/NOLGP ====
        # no group_by in the last call (root node)

        # ==== Rule/OBO ====
        # roots should have at least 1x1 cell
        # tmp_roots = p_ps.get_roots()
        # for i in tmp_roots:
        #     tmp_prog = p_ps.node_list[i]

        #     tmp_script = "ncol({})<=0".format(tmp_prog.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         return (False, "Rule/OBO")

        #     tmp_script = "nrow({})<=0".format(tmp_prog.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         return (False, "Rule/OBO")

        # ==== Rule/NONA ====
        # for **every** ApplyNode, no numeric NA is allowed
        # the ParamNode is already ensured in the random_table method
        # for i in p_ps.node_dict["ApplyNode"]:
        #     tmp_node = p_ps.node_list[i]
        #     tmp_script = "any(sapply({},function(x) is.na(x)))".format(tmp_node.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         # has NA
        #         return (False, "Rule/NONA")

        # ==== Rule/NOINF ====
        # for **every** ApplyNode, no numeric INF is allowed
        # the ParamNode is already ensured in the random_table method
        # for i in p_ps.node_dict["ApplyNode"]:
        #     tmp_node = p_ps.node_list[i]
        #     tmp_script = "any(sapply({},function(x) is.infinite(x)))".format(tmp_node.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         # has INF
        #         return (False, "Rule/NOINF")

        # ==== Rule/NOEMP ====
        # for **every** ApplyNode, no empty string is allowed
        # the ParamNode is already ensured in the random_table method
        # for i in p_ps.node_dict["ApplyNode"]:
        #     tmp_node = p_ps.node_list[i]
        #     tmp_script = "any(sapply({},function(x) x==''))".format(tmp_node.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         # has empty string
        #         return (False, "Rule/NOEMP")

        # ==== Rule/NOSNA ====
        # for **every** ApplyNode, no string NA is allowed
        # the ParamNode is already ensured in the random_table method
        # for i in p_ps.node_dict["ApplyNode"]:
        #     tmp_node = p_ps.node_list[i]
        #     tmp_script = "any(sapply({},function(x) grepl('NA',x)))".format(tmp_node.ps_data)
        #     ret_val = self.renv(tmp_script)
        #     if True==ret_val[0]:
        #         # has string NA
        #         return (False, "Rule/NOSNA")


        return (True, None)

    
    # =================================
    # ========== EVAL SERIES ==========
    # =================================

    # Cambrian/Ventogyrus: postcondition check for:
    # n:NA, i:INF, e:EMP, s:SNA, o:1x1
    # on a single table level
    def cc_nieso(self, p_obj):
        tmp_script = "any(sapply({},function(x) is.na(x)))".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            # has NA
            return False

        tmp_script = "any(sapply({},function(x) is.infinite(x)))".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            # has INF
            return False

        tmp_script = "any(sapply({},function(x) x==''))".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            # has empty string
            return False

        tmp_script = "any(sapply({},function(x) grepl('NA',x)))".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            # has string NA
            return False

        tmp_script = "ncol({})<=0".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            return False

        tmp_script = "nrow({})<=0".format(p_obj)
        ret_val = self.renv(tmp_script)
        if True==ret_val[0]:
            return False

        return True

    ## Concrete interpreter
    def eval_ColInt(self, v):
        return int(v)

    def eval_ColList(self, v):
        return v

    def eval_const(self, node, args):
        return args[0]

    def eval_select(self, node, args):
        # ======== Cambrian/Ventogyrus Preconditions ======== #
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        # if select on two columns, they shall not be the same column
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError("select: Precondition #1 failed.")
        # ======== Cambrian/Ventogyrus Preconditions ======== #

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- select({table}, {cols})'.format(
                   ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))

        try:
            ret_val = self.renv(_script)
        except:
            raise GeneralError("select: Evaluation failed.")

        # ======== Cambrian/Ventogyrus Postconditions ======== #
        if not self.cc_nieso(ret_df_name):
            raise GeneralError("select: Postcondition #1 failed.")
        # ======== Cambrian/Ventogyrus Postconditions ======== #

        # perform shadow tracking
        # NOTICE: shadow tracking should always throws no exception
        self.shadow_dict[ret_df_name] = [-1 for _ in range(len(args[1]))]
        for i in range(len(args[1])):
            p = int(args[1][i])-1
            self.shadow_dict[ret_df_name][i] = self.shadow_dict[args[0]][p]+1

        return ret_df_name

    # TODO: add shadow script
    # def eval_neg_select(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=1,
    #             cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
    #             capture_indices=[0])

    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- select({table}, {cols})'.format(
    #                ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    def eval_unite(self, node, args):
        # ======== Cambrian/Ventogyrus Preconditions ======== #
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

        # don't unite the same column
        if args[1]==args[2]:
            raise GeneralError("unite: Precondition #1 failed.")

        # don't unite numerics, either categorical or non-cat
        if self.get_type(args[0],args[1])=='numeric':
            raise GeneralError("unite: Precondition #2 failed.")
        if self.get_type(args[0],args[2])=='numeric':
            raise GeneralError("unite: Precondition #3 failed.")
        # ======== Cambrian/Ventogyrus Preconditions ======== #

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- unite({table}, "{TMP}", {col1}, {col2})'.format(
                  ret_df=ret_df_name, 
                  table=args[0], 
                  TMP=self.get_fresh_col(args[0],0), 
                  col1=str(args[1]), 
                  col2=str(args[2]))
        
        try:
            ret_val = self.renv(_script)
        except:
            raise GeneralError("unite: Evaluation failed.")

        # ======== Cambrian/Ventogyrus Postconditions ======== #
        if not self.cc_nieso(ret_df_name):
            raise GeneralError("unite: Postcondition #1 failed.")
        # ======== Cambrian/Ventogyrus Postconditions ======== #

        # perform shadow tracking
        # NOTICE: shadow tracking should always throw no exception
        shadow_mincol = min(args[1],args[2])-1
        shadow_maxcol = max(args[1],args[2])-1
        shadow_maxcnt = max(
            self.shadow_dict[args[0]][shadow_mincol],
            self.shadow_dict[args[0]][shadow_maxcol],
        )
        self.shadow_dict[ret_df_name] = [
            self.shadow_dict[args[0]][i]
            for i in range(len(self.shadow_dict[args[0]]))
            if i!=shadow_maxcol
        ]
        self.shadow_dict[ret_df_name][shadow_mincol] = shadow_maxcnt+1

        return ret_df_name

    # TODO: add shadow script
    # def eval_filter(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: x <= n_cols,
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: self.get_type(args[0], str(x)) != 'factor',
    #             capture_indices=[0])

    #     ret_df_name = get_fresh_name()

    #     _script = '{ret_df} <- {table} %>% filter(.[[{col}]] {op} {const})'.format(
    #               ret_df=ret_df_name, table=args[0], op=args[1], col=str(args[2]), const=str(args[3]))
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    def eval_separate(self, node, args):
        # ======== Cambrian/Ventogyrus Preconditions ======== #
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        self.assertArg(node, args,
                index=1,
                cond=lambda x: (self.get_type(args[0], str(x)) != 'numeric') and (self.get_type(args[0], str(x)) != 'integer'),
                capture_indices=[0])

        # source column should not be numeric
        # ---- it's already ensured by the above assertArg
        # if self.get_type(args[0],args[1])=='numeric':
        #     raise GeneralError("Precondition #1 failed.")

        # source column should contain separator
        # this will be enforced by the postconditions

        # ======== Cambrian/Ventogyrus Preconditions ======== #

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- separate({table}, {col1}, c("{TMP1}", "{TMP2}"))'.format(
                  ret_df=ret_df_name, 
                  table=args[0], 
                  col1=str(args[1]), 
                  TMP1=self.get_fresh_col(args[0],0), 
                  TMP2=self.get_fresh_col(args[0],1))
        try:
            ret_val = self.renv(_script)
        except:
            raise GeneralError("separate: Evaluation failed.")

        # ======== Cambrian/Ventogyrus Postconditions ======== #
        if not self.cc_nieso(ret_df_name):
            raise GeneralError("separate: Postcondition #1 failed.")
        # ======== Cambrian/Ventogyrus Postconditions ======== #

        # perform shadow tracking
        # NOTICE: shadow tracking should always throw no exception
        shadow_baseip = args[1]-1
        shadow_basecnt = self.shadow_dict[args[0]][shadow_baseip]
        self.shadow_dict[ret_df_name] = []
        for i in range(len(self.shadow_dict[args[0]])):
            if i==shadow_baseip:
                self.shadow_dict[ret_df_name].append(shadow_basecnt+1)
                # do this twice
                self.shadow_dict[ret_df_name].append(shadow_basecnt+1)
            else:
                self.shadow_dict[ret_df_name].append(
                    self.shadow_dict[args[0]][i]
                )

        return ret_df_name

    def eval_spread(self, node, args):
        # ======== Cambrian/Ventogyrus Preconditions ======== #
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        first_idx = int(args[1])
        self.assertArg(node, args,
                index=1,
                cond=lambda x: x <= n_cols,
                capture_indices=[0])
        # self.assertArg(node, args,
        #         index=2,
        #         cond=lambda x: x <= n_cols and x > first_idx,
        #         capture_indices=[0, 1])
        self.assertArg(node, args,
                index=2,
                cond=lambda x: x <= n_cols,
                capture_indices=[0, 1])

        # don't spread the same column
        if args[1]==args[2]:
            raise GeneralError("spread: Precondition #1 failed.")

        # don't spread numeric columns (key should not be numerical)
        if self.get_type(args[0],args[1])=='numeric':
            raise GeneralError("spread: Precondition #2 failed.")
        # ======== Cambrian/Ventogyrus Preconditions ======== #

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- spread({table}, {col1}, {col2})'.format(
                  ret_df=ret_df_name, 
                  table=args[0], 
                  col1=str(args[1]), 
                  col2=str(args[2]))
        try:
            ret_val = self.renv(_script)
        except Exception as e:
            # print(str(e))
            # self.print_obj(args[0])
            raise GeneralError("spread: Evaluation failed.")

        # ======== Cambrian/Ventogyrus Postconditions ======== #
        if not self.cc_nieso(ret_df_name):
            raise GeneralError("spread: Postcondition #1 failed.")
        # ======== Cambrian/Ventogyrus Postconditions ======== #

        # perform shadow tracking
        # NOTICE: shadow tracking should always throw no exception
        shadow_ipk = int(args[1])-1
        shadow_ipv = int(args[2])-1
        shadow_dc0 = self.renv("ncol({})".format(args[0]))[0]
        shadow_dc1 = self.renv("ncol({})".format(ret_df_name))[0]
        shadow_dd = shadow_dc1-(shadow_dc0-2) # #new_cols
        shadow_basecnt = max(
            self.shadow_dict[args[0]][shadow_ipk],
            self.shadow_dict[args[0]][shadow_ipv],
        )
        self.shadow_dict[ret_df_name] = []
        for i in range(len(self.shadow_dict[args[0]])):
            if i==shadow_ipk or i==shadow_ipv:
                continue
            self.shadow_dict[ret_df_name].append(
                self.shadow_dict[args[0]][i]
            )
        for _ in range(shadow_dd):
            self.shadow_dict[ret_df_name].append(
                shadow_basecnt + 1
            )

        return ret_df_name

    def eval_gather(self, node, args):
        # ======== Cambrian/Ventogyrus Preconditions ======== #
        n_cols = self.renv('ncol(' + args[0] + ')')[0]
        self.assertArg(node, args,
                index=1,
                cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
                capture_indices=[0])

        # if gather on two columns, they shall not be the same column
        if len(args[1])==2 and args[1][0]==args[1][1]:
            raise GeneralError("gather: Precondition #1 failed.")
        # gather on columns that share the same type
        if len(args[1])==2:
            if self.get_type(args[0], args[1][0]) != self.get_type(args[0], args[1][1]):
                raise GeneralError("gather: Precondition #2 failed.")
        # ======== Cambrian/Ventogyrus Preconditions ======== #

        ret_df_name = self.get_fresh_name()
        _script = '{ret_df} <- gather({table}, "{KEY}", "{VALUE}", {cols})'.format(
                   ret_df=ret_df_name, 
                   table=args[0], 
                   KEY=self.get_fresh_col(args[0],0), 
                   VALUE=self.get_fresh_col(args[0],1), 
                   cols=self.get_collist(args[1]))
        try:
            ret_val = self.renv(_script)
        except:
            raise GeneralError("gather: Evaluation failed.")

        # ======== Cambrian/Ventogyrus Postconditions ======== #
        if not self.cc_nieso(ret_df_name):
            raise GeneralError("gather: Postcondition #1 failed.")
        # ======== Cambrian/Ventogyrus Postconditions ======== #

        # perform shadow tracking
        # NOTICE: shadow tracking should always throw no exception
        if len(args[1])==2:
            shadow_ip0 = int(args[1][0])-1
            shadow_ip1 = int(args[1][1])-1
            shadow_basecnt = max(
                self.shadow_dict[args[0]][shadow_ip0],
                self.shadow_dict[args[0]][shadow_ip1],
            )
            self.shadow_dict[ret_df_name] = []
            for i in range(len(self.shadow_dict[args[0]])):
                if i==shadow_ip0 or i==shadow_ip1:
                    continue
                self.shadow_dict[ret_df_name].append(
                    self.shadow_dict[args[0]][i]
                )
        elif len(args[1])==1:
            shadow_ip0 = int(args[1][0])-1
            shadow_basecnt = self.shadow_dict[args[0]][shadow_ip0]
            self.shadow_dict[ret_df_name] = []
            for i in range(len(self.shadow_dict[args[0]])):
                if i==shadow_ip0:
                    continue
                self.shadow_dict[ret_df_name].append(
                    self.shadow_dict[args[0]][i]
                )
        else:
            # this must not happen
            raise GeneralError("gather: Shadow precondition assertion error.")
        # always append 2 new columns
        for _ in range(2):
            self.shadow_dict[ret_df_name].append(shadow_basecnt+1)

        return ret_df_name

    # TODO: add shadow script
    # def eval_neg_gather(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=1,
    #             cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
    #             capture_indices=[0])

    #     ret_df_name = get_fresh_name()
    #     _script = '{ret_df} <- gather({table}, KEY, VALUE, {cols})'.format(
    #                ret_df=ret_df_name, table=args[0], cols=get_collist(args[1]))
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    # NOTICE: use the scoped version: group_by_at to support column index
    # def eval_group_by(self, node, args):

    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=1,
    #             cond=lambda x: max(list(map(lambda y: int(y), x))) <= n_cols,
    #             capture_indices=[0])
        
    #     # removing this assertion for benchmark#6
    #     # self.assertArg(node, args,
    #     #         index=1,
    #     #                cond=lambda x: len(x) == 1,
    #     #         capture_indices=[0])

    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
    #                ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    # TODO: add shadow script
    # NOTICE: use the scoped version: group_by_at to support column index
    # def eval_neg_group_by(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=1,
    #             cond=lambda x: max(list(map(lambda y: -int(y), x))) <= n_cols, # add negative
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=1,
    #                    cond=lambda x: len(x) == 1,
    #             capture_indices=[0])

    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- group_by_at({table}, {cols})'.format(
    #                ret_df=ret_df_name, table=args[0], cols=self.get_collist(args[1]))
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    # TODO: add shadow script
    # TODO: need to fix get_fresh_col key word conflice, e.g., 'function'
    # def eval_summarise(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: x <= n_cols,
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: self.get_type(args[0], str(x)) == 'integer' or self.get_type(args[0], str(x)) == 'numeric',
    #             capture_indices=[0])

    #     # get column names
    #     colname = self.renv("colnames({table})".format(table=args[0]))[args[2]-1]

    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- {table} %>% summarise({TMP} = {aggr} (`{col}`))'.format(
    #               ret_df=ret_df_name, table=args[0], TMP=self.get_fresh_col(args[0],0), aggr=str(args[1]), col=colname)
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    # TODO: need to fix get_fresh_col key word conflice, e.g., 'function'
    # def eval_mutate(self, node, args):
    #     n_cols = self.renv('ncol(' + args[0] + ')')[0]
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: x <= n_cols,
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=3,
    #             cond=lambda x: x <= n_cols,
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=2,
    #             cond=lambda x: self.get_type(args[0], str(x)) == 'numeric',
    #             capture_indices=[0])
    #     self.assertArg(node, args,
    #             index=3,
    #             cond=lambda x: self.get_type(args[0], str(x)) == 'numeric',
    #             capture_indices=[0])

    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- {table} %>% mutate({TMP}=.[[{col1}]] {op} .[[{col2}]])'.format(
    #               ret_df=ret_df_name, table=args[0], TMP=self.get_fresh_col(args[0],0), op=args[1], col1=str(args[2]), col2=str(args[3]))
    #     shadow_maxcnt = self.renv('max({table}[,{cols}])'.format(
    #                                 table=self.get_shadow_name(args[0]),
    #                                 cols="c({},{})".format(args[2],args[3]),))[0]
    #     shadow_script = '{ret_shadow} <- as.data.frame(cbind(as.matrix({table}),{smax}))'.format(
    #                      ret_shadow=self.get_shadow_name(ret_df_name), 
    #                      table=self.get_shadow_name(args[0]), 
    #                      smax="c({})".format(shadow_maxcnt+1), )
    #     try:
    #         ret_val = self.renv(_script)
    #         _ = self.renv(shadow_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

    # TODO: add shadow script
    # def eval_inner_join(self, node, args):
    #     ret_df_name = self.get_fresh_name()
    #     _script = '{ret_df} <- inner_join({t1}, {t2})'.format(
    #               ret_df=ret_df_name, t1=args[0], t2=args[1])
    #     try:
    #         ret_val = self.renv(_script)
    #         return ret_df_name
    #     except:
    #         raise GeneralError()

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
    # def camb_get_shash_abs(self, p_obj, verbose=False):
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

    #     one_hot_hash = [0 for _ in range(15)]
    #     df_hash_val = self.hash(str(self.renv(p_obj)))%15
    #     one_hot_hash[df_hash_val] = 1

    #     one_hot_rhash = [0 for _ in range(15)]
    #     df_rhash_val = self.hash(str(self.renv(p_obj))[::-1])%15
    #     one_hot_rhash[df_rhash_val] = 1

    #     if verbose:
    #         print(one_hot_nrow)
    #         print(one_hot_ncol)
    #         print(bin_und)
    #         print(one_hot_hash)
    #         print(one_hot_rhash)

    #     return one_hot_nrow + one_hot_ncol + bin_und + one_hot_hash + one_hot_rhash


    '''
    abstraction function for 0729 Sophia Series
    to solve collided states problem
    '''
    def camb_get_ventogyrus(self, p_obj, verbose=False):
        np_obj, dr, dc = self.camb_get_np_obj(p_obj)

        one_hot_nrow = [0 for _ in range(self.CAMB_NROW)]
        if dr<=self.CAMB_NROW:
            one_hot_nrow[dr-1] = 1

        one_hot_ncol = [0 for _ in range(self.CAMB_NCOL)]
        if dc<=self.CAMB_NCOL:
            one_hot_ncol[dc-1] = 1

        one_hot_first_row_und = [0 for _ in range(self.CAMB_NCOL)]
        one_hot_last_row_und = [0 for _ in range(self.CAMB_NCOL)]
        if dr>0:
            for i in range(min(dc,self.CAMB_NCOL)):
                if isinstance(np_obj[0,i],numpy.str):
                    if "_" in np_obj[0,i]:
                        one_hot_first_row_und[i] = 1
                if isinstance(np_obj[-1,i],numpy.str):
                    if "_" in np_obj[-1,i]:
                        one_hot_first_row_und[i] = 1

        # if len(set)<len, then we consider it's category
        one_hot_is_col_cat = [0 for _ in range(self.CAMB_NCOL)]
        if dr>0:
            for i in range(min(dc,self.CAMB_NCOL)):
                if len(set(np_obj[:,i]))<len(np_obj[:,i]):
                    one_hot_is_col_cat[i] = 1

        one_hot_is_col_str = [0 for _ in range(self.CAMB_NCOL)]
        if dr>0:
            for i in range(min(dc,self.CAMB_NCOL)):
                if isinstance(np_obj[0,i],numpy.str):
                    one_hot_is_col_str[i] = 1

        one_hot_is_col_num = [0 for _ in range(self.CAMB_NCOL)]
        if dr>0:
            for i in range(min(dc,self.CAMB_NCOL)):
                if isinstance(np_obj[0,i],numpy.float):
                    one_hot_is_col_num[i] = 1

        if verbose:
            print(one_hot_nrow)
            print(one_hot_ncol)
            print(one_hot_first_row_und)
            print(one_hot_last_row_und)
            print(one_hot_is_col_cat)
            print(one_hot_is_col_str)
            print(one_hot_is_col_num)

        return one_hot_nrow + \
               one_hot_ncol + \
               one_hot_first_row_und + \
               one_hot_last_row_und + \
               one_hot_is_col_cat + \
               one_hot_is_col_str + \
               one_hot_is_col_num


    '''
    Cambrian/Yorgia Version
    in-between abstraction of partial table
    '''
    # def camb_get_yorgia(self, p_obj, modn=128, verbose=False):
    #     np_obj, dr, dc = self.camb_get_np_obj(p_obj)

    #     table_abs_row_one = [0 for _ in range(self.CAMB_NCOL)]
    #     for j in range(min(self.CAMB_NCOL,dc)):
    #         table_abs_row_one[j] = self.hash(np_obj[0,j]) % modn
    #     table_abs_col_one = [0 for _ in range(self.CAMB_NROW)]
    #     for i in range(min(self.CAMB_NROW,dr)):
    #         table_abs_col_one[i] = self.hash(np_obj[i,0]) % modn

    #     if verbose:
    #         print(table_abs_row_one)
    #         print(table_abs_col_one)

    #     return table_abs_row_one + table_abs_col_one


    '''
    Cambrian/Yorgia Version
    in-between abstraction of partial table
    '''
    # def camb_get_zong(self, p_obj, verbose=False):
    #     np_obj, dr, dc = self.camb_get_np_obj(p_obj)

    #     one_hot_nrow = [0 for _ in range(self.CAMB_NROW)]
    #     if dr<=self.CAMB_NROW:
    #         one_hot_nrow[dr-1] = 1

    #     one_hot_ncol = [0 for _ in range(self.CAMB_NCOL)]
    #     if dc<=self.CAMB_NCOL:
    #         one_hot_ncol[dc-1] = 1


    #     if verbose:
    #         print(one_hot_nrow)
    #         print(one_hot_ncol)

    #     return one_hot_nrow + one_hot_ncol



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
    def generate(self, fixed_depth, example):
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
            except Exception as e:
                # print("DBG-PROG: {}".format(str(tmp_prog)))
                # print("DBG-ERR: {}, {}".format(type(e),str(e)))
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
                # print(tmp_check[1])
                # important, also prevents infinite loop
                _exp_cnt += 1
                continue

    # wrap the trial-and-generate process
    # the very random version
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


    # Cambrian/Ventogrus: fit an input table to the program
    # speed up for certain program, e.g., spread, that
    # requires special forms of input structure
    def get_new_size1_program(self):
        tmp_enumerator = RandomEnumeratorFD(self._spec, fixed_depth = 2)
        tmp_prog = tmp_enumerator.next()
        # fixed_program: a single ApplyNode
        _exp_cnt = 0
        
        while True:
            try:
                p_input = self._interpreter.random_table_for_call(tmp_prog)
                tmp_eval = self._interpreter.eval(
                    tmp_prog,
                    [p_input],
                )
            except Exception as e:
                # print("DBG-PROG: {}".format(str(tmp_prog)))
                # print("DBG-ERR: {}, {}".format(type(e),str(e)))
                _exp_cnt += 1
                if _exp_cnt>=10:
                    # exceed the limit consider changing program
                    return None
                continue
            tmp_example = Example(
                input=[p_input],
                output=tmp_eval,
            )
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



   









    












