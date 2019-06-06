#!/usr/bin/env python
import logging 
logging.basicConfig(level=logging.CRITICAL)

import copy
from tyrell.enumerator import ExhaustiveEnumerator
from tyrell.interpreter import InterpreterError
from tyrell import dsl as D
from sexpdata import Symbol

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