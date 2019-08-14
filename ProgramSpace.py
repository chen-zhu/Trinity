#!/usr/bin/env python
import logging 
logging.basicConfig(level=logging.CRITICAL)

import copy
import numpy

from tyrell.enumerator import ExhaustiveEnumerator
from tyrell.interpreter import InterpreterError
from tyrell import dsl as D
from sexpdata import Symbol
from itertools import product

from tyrell.spec.production import ParamProduction, EnumProduction, FunctionProduction
from tyrell.dsl.node import AtomNode, ParamNode, ApplyNode


'''
======== Cambrian/Trilobite Version ========
The full version of Program Space 
that support chain & tree representation
as well as disconnected hypergraphs
---
multiple inputs & single output
'''

'''
function updates for several Nodes
'''
def Node_eq(self, other):
    return self.__repr__()==other.__repr__()
def Node_hash(self):
    return self.__repr__().__hash__()
setattr(AtomNode,"__eq__",Node_eq)
setattr(ParamNode,"__eq__",Node_eq)
setattr(ApplyNode,"__eq__",Node_eq)
setattr(AtomNode,"__hash__",Node_hash)
setattr(ParamNode,"__hash__",Node_hash)
setattr(ApplyNode,"__hash__",Node_hash)

def Prod_eq(self, other):
    return self.__repr__()==other.__repr__()
def Prod_hash(self):
    return self.__repr__().__hash__()
setattr(ParamProduction,"__eq__",Prod_eq)
setattr(EnumProduction,"__eq__",Prod_eq)
setattr(FunctionProduction,"__eq__",Prod_eq)
setattr(ParamProduction,"__hash__",Prod_hash)
setattr(EnumProduction,"__hash__",Prod_hash)
setattr(FunctionProduction,"__hash__",Prod_hash)


class ProgramSpace(object):
    # NOTICE:
    # Due to the nature of rpy2, never deep copy an interpreter
    # Interpreter is always shared
    def __init__(self, p_spec, p_interpreter, p_inputs, p_output):
        self.spec = p_spec
        # self.builder = D.Builder(self.spec)
        self.interpreter = p_interpreter
        self.inputs = p_inputs # multiple inputs, a list
        self.output = p_output # single output

        # stores all added shells
        self.shells = []

        # initialize a node dictionary
        self.node_list = []
        self.node_dict = {
            "ApplyNode": [],
            "ParamNode": [],
            "AtomNode": [],
        }
        self.prod_list = []
        self.prod_dict = {
            "ParamProduction": [],
            "EnumProduction": [],
            "FunctionProduction": [],
        }
        # type_dict stores node ids according to types
        # using it directly is OK, as different types have different __repr__() results
        self.type_dict = {
            p:[] for p in self.spec.types()
        }

        # add AtomNode and ParamNode first
        tmp_productions = self.spec.productions()
        for d_prod in tmp_productions:
            if isinstance(d_prod, EnumProduction):
                # dict first then list
                self.prod_dict["EnumProduction"].append(len(self.prod_list))
                self.prod_list.append(d_prod)
    
                self.node_dict["AtomNode"].append(len(self.node_list))
                self.type_dict[d_prod.lhs].append(len(self.node_list))
                self.node_list.append(
                    AtomNode(d_prod)
                )

            if isinstance(d_prod, FunctionProduction):
                # dict first then list
                self.prod_dict["FunctionProduction"].append(len(self.prod_list))
                self.prod_list.append(d_prod)

        # make sure that ParamNdoe come at last
        for d_prod in tmp_productions:
            if isinstance(d_prod, ParamProduction):
                # dict first then list
                self.prod_dict["ParamProduction"].append(len(self.prod_list))
                self.prod_list.append(d_prod)
                
                self.node_dict["ParamNode"].append(len(self.node_list))
                self.type_dict[d_prod.lhs].append(len(self.node_list))
                self.node_list.append(
                    ParamNode(d_prod)
                )

                # attach .ps_data attribute initially
                self.node_list[-1].ps_data = self.inputs[
                    self.node_list[-1].index
                ]

    '''
    support temporal solution for undo
    '''
    def make_copy(self):
        new_ps = ProgramSpace(
            self.spec,
            self.interpreter,
            self.inputs,
            self.output,
        )
        new_ps.shells = copy.deepcopy(self.shells)
        new_ps.node_list = copy.deepcopy(self.node_list)
        new_ps.node_dict = copy.deepcopy(self.node_dict)
        new_ps.prod_list = copy.deepcopy(self.prod_list)
        new_ps.prod_dict = copy.deepcopy(self.prod_dict)
        new_ps.type_dict = copy.deepcopy(self.type_dict)

        return new_ps

    '''
    initialize and add shells to the current Program Space
    NOTICE: should call this on a newly initialized Program Space
    e.g., self.shells is empty
    use DFS to traverse all the nodes to add: postorder traversal (left-right-root)
    assumes no error in the prog
    '''
    def init_by_prog(self,p_prog):
        def post_order_travesal(droot):
            for p in droot.children:
                post_order_travesal(p)
            if droot.is_apply():
                # it's equavelent to a shell
                # and if it's apply, then it has children
                d_prod = self.prod_list.index(droot._prod)
                d_rhs = [self.node_list.index(q) for q in droot.children]
                d_shell = (d_prod, tuple(d_rhs))
                self.add_neighboring_shell(d_shell)
        # just call the recursive function
        post_order_travesal(p_prog)

    '''
    initialize from entry of saved dataset
    entry: (prog, str_example)
    '''
    def init_from_entry(self, p_entry):
        d_prog = p_entry[0]
        ds_example = p_entry[1]
        self.inputs = [
            self.interpreter.load_data_into_var(p) 
            for p in ds_example.input
        ]
        self.output = self.interpreter.load_data_into_var(
            ds_example.output
        )
        self.init_by_prog(d_prog)
        # had better check for validity manually after this
        

    '''
    get all neighboring shells given the current self.nodes
    shell: (prod, [ids for rhs replacement])
    '''
    def get_neighboring_shells(self):
        # enumerate all possible production rules
        ret_shells = []
        for pid in self.prod_dict["FunctionProduction"]:
            d_prod = self.prod_list[pid]
            expanded_rhs = [
                self.type_dict[p] for p in d_prod.rhs
            ]
            for p in product(*expanded_rhs): # expand expanded_rhs
                # if (d_prod, p) not in self.shells: # check for duplication
                if (pid, p) not in self.shells: # check for duplication
                    ret_shells.append(
                        # (d_prod, p)
                        (pid, p)
                    )
        return ret_shells


    '''
    return a sample ApplyNode from shell based on the current state
    does not change the state of ProgramSpace at all
    '''
    def get_node_from_shell(self, d_shell):
        d_prod = self.prod_list[d_shell[0]]
        d_rhs = d_shell[1]
        return ApplyNode(
            d_prod,
            [self.node_list[i] for i in d_rhs],
        )


    def add_neighboring_shell(self, d_shell, update_node_data=True):
        # shell: (prod_id, [ids for rhs replacement])
        # prod must be "FunctionProduction" according to the get_ method
        # so new node can only be ApplyNode

        # first check for duplication
        if d_shell in self.shells:
            return False
        else:
            self.shells.append(d_shell)

        d_prod = self.prod_list[d_shell[0]]
        d_rhs = d_shell[1]
        d_lhs = d_prod.lhs
        d_nid = len(self.node_list)

        self.node_dict["ApplyNode"].append(d_nid)
        self.type_dict[d_lhs].append(d_nid)
        self.node_list.append(
            ApplyNode(
                d_prod,
                [self.node_list[i] for i in d_rhs],
            )
        )

        # add additional tracking tags: ps_parents & ps_children
        # hypergraph can have multiple parents, AST cannot
        # but in value computing, this attribute doesn't pose any challenges
        ## ps_parent
        for i in d_rhs:
            if not hasattr(self.node_list[i],"ps_parents"):
                self.node_list[i].ps_parents = []
            self.node_list[i].ps_parents.append(d_nid)
        ## ps_children
        if not hasattr(self.node_list[d_nid],"ps_children"):
            self.node_list[d_nid].ps_children = []
        for i in d_rhs:
            self.node_list[d_nid].ps_children.append(i)

        # update node data (if necessary on design)
        # wrap an exception to catch failure
        if update_node_data:
            try:
                tmp_outv = self.interpreter.eval(self.node_list[d_nid],self.inputs)
                self.node_list[d_nid].ps_data = tmp_outv
            except InterpreterError as e:
                # interpreter fail

                # recover all status before function call
                del self.shells[-1]
                del self.node_dict["ApplyNode"][-1]
                del self.type_dict[d_lhs][-1]
                del self.node_list[-1]
                for i in d_rhs:
                    del self.node_list[i].ps_parents[-1]
                # node_list[d_nid] is already deleted

                return False
        
        return True

    '''
    see README/Jargons for definition
    '''
    def get_strict_frontiers(self):
        ret_frontiers = []
        for i in self.node_dict["ApplyNode"]+self.node_dict["ParamNode"]:
            # check if the node is root or not
            # only add root nodes
            if (not hasattr(self.node_list[i],"ps_parents")) or len(self.node_list[i].ps_parents)==0:
                # this is one root
                ret_frontiers.append(i)
        return ret_frontiers

    '''
    see README/Jargons for definition
    '''
    def get_roots(self):
        ret_frontiers = []
        for i in self.node_dict["ApplyNode"]:
            # check if the node is root or not
            # only add root nodes
            if (not hasattr(self.node_list[i],"ps_parents")) or len(self.node_list[i].ps_parents)==0:
                # this is one root
                ret_frontiers.append(i)
        return ret_frontiers

    '''
    check whether there are any frontiers that match the desired output
    if yes, return that specific node id
    otherwise, return None
    '''
    def check_eq(self):
        tmp_frontiers = self.get_roots()
        for i in tmp_frontiers:
            if self.interpreter.equal( self.node_list[i].ps_data, self.output ):
                return i
        return None

    '''
    get the number of calls/ApplyNode in the specified node
    if pid is None, sum all number of calls in frontiers
    '''
    def get_ncalls(self, pid=None):
        if pid is None:
            nl = self.get_frontiers()
            ret_n = 0
            for i in nl:
                ret_n += self.node_list[i].__repr__().count("ApplyNode")
            return ret_n
        else:
            return self.node_list[pid].__repr__().count("ApplyNode")


    '''
    get the adjacency matrix (undirected)
    return in a numpy.ndarray type
    do not tell apart different edge types (all connected in one type of )
    '''
    def get_adjacency_matrix_u(self):
        ds = len(self.node_list)
        ret_matrix = numpy.eye(ds,dtype=numpy.int)
        for i in range(len(self.node_list)):
            dnode = self.node_list[i]
            if dnode is None:
                ret_matrix[i,i] = 0
                continue
            if hasattr(dnode,"ps_parents"):
                for j in dnode.ps_parents:
                    ret_matrix[i,j] = 1
            if hasattr(dnode,"ps_children"):
                for j in dnode.ps_children:
                    ret_matrix[i,j] = 1
        return numpy.matrix(ret_matrix)

    '''
    get the degree matrix (undirected)
    following the setting of get_adjacency_matrix_u
    '''
    def get_degree_matrix_u(self):
        ds = len(self.node_list)
        adj_matrix = self.get_adjacency_matrix_u()
        ret_matrix = numpy.zeros((ds,ds),dtype=numpy.int)
        sum_array = numpy.sum(adj_matrix,axis=1)
        for i in range(ds):
            ret_matrix[i,i] = sum_array[i]
        return numpy.matrix(ret_matrix)

    def get_normalized_adjacency_matrix_u(self):
        ds = len(self.node_list)
        adj_matrix = self.get_adjacency_matrix_u()
        deg_matrix = self.get_degree_matrix_u()
        ret_matrix = (deg_matrix**(-1)) * adj_matrix
        return ret_matrix









'''
This version only supports chain execution and only one input.
Used for early testing only.
'''
class ProgramSpaceChainOneNB(object):
    # NOTICE:
    # Due to the nature of rpy2, never deep copy an interpreter
    # Interpreter is always shared
    def __init__(self, p_spec, p_interpreter, p_eq, p_input, p_output):
        self.spec = p_spec
        self.builder = D.Builder(self.spec)
        self.interpreter = p_interpreter
        self.eq = p_eq
        self.input = p_input
        self.output = p_output
        
        self.outv_list = [] # list of Example.output: [output1, output2, ....]
        self.prog_list = [] # list of Nodes
        self.sexp_list = [] # list of sexps: [Symbol(...),...]
        
        self.PARAM_NODE = self.builder._from_sexp([Symbol('@param'), 0])
        
        # generate shells (of depth 1)
        tmp_enumerator = ExhaustiveEnumerator(p_spec, max_depth=2)
        self.shell_list = [] # list of shells (prog/Nodes)
        while True:
            tmp_shell = tmp_enumerator.next()
            if tmp_shell is None:
                break
            
            if len(tmp_shell.children)>0:
                # should have at least one child
                self.shell_list.append(tmp_shell)
        # NOTICE: str(prog) is NOT sexp
        self.str_shell_list = [str(p) for p in self.shell_list]
        self.str_shell_dict = {
            self.str_shell_list[i]:i 
            for i in range(len(self.str_shell_list))
        }
        
    '''
    convert the prog_list to a full program
    (chain program is assumed)
    '''
    def get_full_prog(self, prog_list):
        prog_list = copy.deepcopy(prog_list)
        pnode = prog_list[0]
        for i in range(1,len(prog_list)):
            d_prog = prog_list[i]
            for j in range(len(d_prog.children)):
                if d_prog.children[j].is_param():
                    # replace with pnode
                    d_prog.children[j] = pnode
                    pnode = d_prog
                    continue
        return pnode
    
    '''
    convert a full program to prog_list
    (chain program is assumed)
    '''
    def get_prog_list(self, full_prog):
        def dfs_traverse(dnode):
            # convert to sexp to avoid pointers
            pl = []
            for i in range(len(dnode.children)):
                cnode = dnode.children[i]
                if cnode.is_apply():
                    pl += dfs_traverse(cnode)
                    # change the ApplyNode to ParamNode and for a prog again
                    dnode.children[i] = self.PARAM_NODE
                    break
            pl.append(dnode.to_sexp())
            return pl
        full_prog = copy.deepcopy(full_prog)
        d_sexp_list = dfs_traverse(full_prog)
        d_prog_list = [self.builder._from_sexp(p) for p in d_sexp_list]
        return d_prog_list
        
    '''
    compare the current output with the designated output
    calling the eq function
    '''
    def out_eq(self):
        if len(self.outv_list)==0:
            return False
        return self.eq(self.output, self.outv_list[-1][0]) # access 0 since it's in input format
    
    '''
    return the last output (aka. the current input)
    '''
    def get_frontier(self):
        if len(self.outv_list)==0:
            return self.input
        else:
            return self.outv_list[-1]
        
    '''
    add an sexp, execute and generate intermediate outputs
    if succeeded return True, otherwise return False
    '''
    def add_sexp(self, p_sexp):
        assert len(self.outv_list)==\
               len(self.prog_list)==\
               len(self.sexp_list)
        
        tmp_input = self.get_frontier()
        tmp_prog = self.builder._from_sexp(p_sexp)
        
        try:
            tmp_outv = self.interpreter.eval(tmp_prog,tmp_input)
        except InterpreterError as e:
            # failed to add sexp
            return False
        
        # succeed
        self.prog_list.append(tmp_prog)
        self.sexp_list.append(p_sexp)
        # NOTICE: wrap output in [] to be in the input format
        self.outv_list.append([tmp_outv])
        return True
    
    '''
    make a copy of the current instance, with shared interpreter
    '''
    def make_copy(self):
        new_ps = ProgramSpaceChainOneNB(
            self.spec,
            self.interpreter,
            self.eq,
            self.input,
            self.output,
        )
        new_ps.outv_list = copy.deepcopy(self.outv_list)
        new_ps.prog_list = copy.deepcopy(self.prog_list)
        new_ps.sexp_list = copy.deepcopy(self.sexp_list)
        new_ps.shell_list = copy.deepcopy(self.shell_list)
        new_ps.str_shell_list = copy.deepcopy(self.str_shell_list)
        new_ps.str_shell_dict = copy.deepcopy(self.str_shell_dict)
        return new_ps