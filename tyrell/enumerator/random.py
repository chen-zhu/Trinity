from typing import Set, Optional
from random import Random
from .enumerator import Enumerator
from .. import dsl as D
from .. import spec as S


class RandomEnumerator(Enumerator):
    _rand: Random
    _max_depth: int
    _builder: D.Builder

    def __init__(self, spec: S.TyrellSpec, max_depth: int, seed: Optional[int]=None):
        self._rand = Random(seed)
        self._builder = D.Builder(spec)
        if max_depth <= 0:
            raise ValueError(
                'Max depth cannot be non-positive: {}'.format(max_depth))
        self._max_depth = max_depth

    def _do_generate(self, curr_type: S.Type, curr_depth: int, force_leaf: bool):
        # First, get all the relevant production rules for current type
        productions = self._builder.get_productions_with_lhs(curr_type)
        if force_leaf:
            productions = list(
                filter(lambda x: not x.is_function(), productions))
        if len(productions) == 0:
            raise RuntimeError('RandomASTGenerator ran out of productions to try for type {} at depth {}'.format(
                curr_type, curr_depth))

        # Pick a production rule uniformly at random
        prod = self._rand.choice(productions)
        if not prod.is_function():
            # make_node() will produce a leaf node
            return self._builder.make_node(prod)
        else:
            # Recursively expand the right-hand-side (generating children first)
            children = [self._generate(x, curr_depth + 1) for x in prod.rhs]
            # make_node() will produce an internal node
            return self._builder.make_node(prod, children)

    def _generate(self, curr_type: S.Type, curr_depth: int):
        return self._do_generate(curr_type, curr_depth,
                                 force_leaf=(curr_depth >= self._max_depth - 1))

    def next(self):
        return self._generate(self._builder.output, 0)

class RandomEnumeratorS(Enumerator):
    _rand: Random
    _max_depth: int
    _builder: D.Builder

    def __init__(self, spec: S.TyrellSpec, max_depth: int, probs: list, seed: Optional[int]=None):
        self._rand = Random(seed)
        self._builder = D.Builder(spec)
        if max_depth <= 0:
            raise ValueError(
                'Max depth cannot be non-positive: {}'.format(max_depth))
        self._max_depth = max_depth
        self._probs = probs # [leaf prob, non-leaf prob]

    def _do_generate(self, curr_type: S.Type, curr_depth: int, force_leaf: bool):
        # First, get all the relevant production rules for current type
        productions = self._builder.get_productions_with_lhs(curr_type)
        # for p in productions:
        #     print(p)
        # input("PAUSE")
        if force_leaf:
            productions = list(
                filter(lambda x: not x.is_function(), productions))
        if len(productions) == 0:
            raise RuntimeError('RandomASTGenerator ran out of productions to try for type {} at depth {}'.format(
                curr_type, curr_depth))

        weights = [self._probs[1] if p.is_function() else self._probs[0] for p in productions]
        # Pick a production rule uniformly at random
        prod = self._rand.choices(productions,weights=weights,k=1)[0]
        if not prod.is_function():
            # make_node() will produce a leaf node
            return self._builder.make_node(prod)
        else:
            # Recursively expand the right-hand-side (generating children first)
            children = [self._generate(x, curr_depth + 1) for x in prod.rhs]
            # make_node() will produce an internal node
            return self._builder.make_node(prod, children)

    def _generate(self, curr_type: S.Type, curr_depth: int):
        return self._do_generate(curr_type, curr_depth,
                                 force_leaf=(curr_depth >= self._max_depth - 1))

    def next(self):
        return self._generate(self._builder.output, 0)

'''
Notice: Chain Execution One Input No Branch ONLY
enumerator that only generates programs of designated size
'''
class RandomEnumeratorFD(Enumerator):
    _rand: Random
    _fixed_depth: int
    _builder: D.Builder

    def __init__(self, spec: S.TyrellSpec, fixed_depth: int, seed: Optional[int]=None):
        self._rand = Random(seed)
        self._builder = D.Builder(spec)
        if fixed_depth <= 0:
            raise ValueError(
                'Max depth cannot be non-positive: {}'.format(fixed_depth))
        self._fixed_depth = fixed_depth

    def _do_generate(self, curr_type: S.Type, curr_depth: int, force_leaf: bool):
        # First, get all the relevant production rules for current type
        productions = self._builder.get_productions_with_lhs(curr_type)

        expandable = False
        for pd in productions:
            if pd.is_function():
                expandable = True
                break

        if force_leaf:
            productions = list(
                filter(lambda x: not x.is_function(), productions))
        else:
            if expandable:
            # if False:
                # not force leaf, then force non-leaf
                productions = list(
                    filter(lambda x: x.is_function(), productions))
            # else: it is already leaf because it's not expandable

        if len(productions) == 0:
            raise RuntimeError('RandomASTGenerator ran out of productions to try for type {} at depth {}'.format(
                curr_type, curr_depth))

        # Pick a production rule uniformly at random
        prod = self._rand.choice(productions)
        if not prod.is_function():
            # make_node() will produce a leaf node
            return self._builder.make_node(prod)
        else:
            # Recursively expand the right-hand-side (generating children first)
            children = [self._generate(x, curr_depth + 1) for x in prod.rhs]
            # make_node() will produce an internal node
            return self._builder.make_node(prod, children)

    def _generate(self, curr_type: S.Type, curr_depth: int):
        return self._do_generate(curr_type, curr_depth,
                                 force_leaf=(curr_depth >= self._fixed_depth - 1))

    def next(self):
        return self._generate(self._builder.output, 0)