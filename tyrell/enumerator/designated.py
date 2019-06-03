from typing import Set, Optional
from random import Random
from .enumerator import Enumerator
from .. import dsl as D
from .. import spec as S

from sexpdata import Symbol


class DesignatedEnumerator(Enumerator):
    _rand: Random
    _max_depth: int
    _builder: D.Builder

    def __init__(self, spec: S.TyrellSpec, sexp: list):
        self._builder = D.Builder(spec)
        self._sexp = sexp

    def _do_generate(self):
        # just build and return the designated program
        # #############
        return self._builder._from_sexp(self._sexp)

    def _generate(self, curr_type: S.Type, curr_depth: int):
        return self._do_generate()

    def next(self):
        return self._generate(self._builder.output, 0)
