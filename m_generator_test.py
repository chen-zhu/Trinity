#!/usr/bin/env python
import logging 
logging.basicConfig(level=logging.CRITICAL)

import copy
import tyrell.spec as S
from tyrell.interpreter import Interpreter, PostOrderInterpreter, GeneralError, InterpreterError
from tyrell.enumerator import Enumerator, SmtEnumerator, RandomEnumerator, DesignatedEnumerator, RandomEnumeratorS, ExhaustiveEnumerator
from tyrell.decider import Example, ExampleConstraintPruningDecider, ExampleDecider, TestDecider
from tyrell.synthesizer import Synthesizer
from tyrell.logger import get_logger
from sexpdata import Symbol
from tyrell import dsl as D
from typing import Callable, NamedTuple, List, Any

from utils_morpheus import *
from ProgramSpace import *

def main():

    sts = {1:0, 2:0, 3:0}

    spec = S.parse_file('example/mChainOneNB.tyrell')

    mcoder = MorpheusInterpreter()

    while True:
        while True:
            minput = mcoder.random_table()
            generator = MorpheusGenerator(
                spec=spec,
                interpreter=mcoder,
                sfn=mcoder.sanity_check,
            )
            p_prog, p_example = generator.generate(
                fixed_depth=4,
                example=Example(input=[minput], output=None),
            )
            if p_prog.is_apply():
                break
        ps_full = ProgramSpaceChainOneNB(
            spec, mcoder, eq_r, p_example.input, p_example.output,
        )
        p_prog_list = ps_full.get_prog_list(p_prog)
        for p in p_prog_list:
            ps_full.add_sexp(p.to_sexp())
        sts[len(ps_full.prog_list)] += 1

        print("\r# 1:{}, 2:{}, 3:{}".format(
            sts[1],sts[2],sts[3]
        ), end="")






if __name__ == '__main__':
    logger.setLevel('DEBUG')
    main()