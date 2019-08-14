## Quick Start Guide (08/14/2019)

(The instructions may be out of date since this repo is updating nondeterministically.)

**(The current version only applies to chain programs.)**

You need to have the Trinity framework installed before running the following scripts.

`0811CambBLE_Newton.ipynb` is the exclusion network that learns to rank all function calls according to the degree of validity given only the input tables. Function calls that cause no exception and fit the human defined heuristics (see `sanity_check` in `MorpheusInterpreter.py`) will rank higher than others.

`0813CambAgent_Thomas_Size3.ipynb` trains the execution-guided synthesis network on size 3 programs using the provided training dataset `0812Size3RelaxedFiltered.pkl`. Trained model is saved in the `saved_models` folder.

`0813Thomas_Size3_Test.ipynb` tests the execution-guided synthesis network on benchmarks. The benchmarks are stored in `benchmarks` folder with solutions defined in `benchmarks/pldi17_sol.py`.

Some supporting libraries are:

- `ProgramSpace.py` offers a class to support easy program level modifications (e.g., executing one step, obtaining all possible next steps given input and production rules). It's based on the Trinity framework.
- `MorpheusInterpreter.py` implements (wraps) the environment for the Morpheus DSL, together with some additional features such as `sanity_check` that checks the validity of a program on a higher level (e.g., we don't use `group_by` at the end), `MorpheusGenerator` that generates random programs, feature extraction function `camb_get_pomoria` that generates one-hot features for a given table, etc..

Code names like `Alice`, `Bob` or `Newton` are used for version control internally only and carry no further meaning.

## Versions

- Proterozonic: collections of all old versions backups, see `deprecated` folder.
- Cambrian: major branch merged with Trinity framework.
  - Trilobite: subversion
  - Charniodiscus: subversion
  - (0714) Spriggina: subversion
    - unified `MorpheusInterpreter` class
    - clean out `MorpheusGenerator` class
    - better `sanity_check` framework method
    - (others not listed here but also important)
  - (0724) Inaria:  subversion
    - poly structure to fit fast online adaptation
  - (0726) Yorgia: subversion for debugging
    - debugging
    - introducing Triangle ranker
    - introducing TransE ranker
  - (0803) Ventogyrus: subversion for refined generator
    - refined generator: pre-conditions and post-conditions
    - refined shadow mechanism
  - (0806) Haootia: introduce few size 3 as pretrain programs
  - (0809) Pomoria: version with a Behaviral Law Engine
    - Behaviral Law Engine
    - Refined Morpheus Interpeter (extended components and better shadow mechanism)
    - Sample program by provided sketch `fixed_sketch` parameter

## Jargons

- `frontiers`: 
  - expandable nodes in `ProgramSpace`, typically a `ParamNode`/`ApplyNode` is always a `frontier`
  - a `frontier` is what the agent decides actions on, i.e., an agent do not expand the `ProgramSpace` on nodes other than `frontiers`
- `strict_frontiers`:
  - a `frontier` that has no parent nodes is a `strict_frontier`
  - a `strict_frontier` is used when the agent decides actions in a **chain execution** environment, where single input and single output is assumed and only **one** `strict_frontier` can be found in chain execution
- `roots`: 
  - an `ApplyNode` that has no parent nodes is a `root` in a `ProgramSpace`
  - notice that according to the `IONEQ` rule, a `ParamNode` can never be a `root`
- difference between `strict_frontiers` and `roots`
  - a `strict_frontier` can be a `ParamNode`, but a `root` can't
  - a `root` need not be expandable, but a `strict_frontier` should be expandable

## Some Notes for Reference

- The current shadow mechanism in `sanity_check` cannot prevent `unite`->`separate` redundant patterns.

- TODO: need to fix get_fresh_col key word conflict, e.g., 'function'

- Useful commands for fast execution

- ```
  screen -S 0811Size3_t0
  python ./0811GenSize3.py 0811Size3_t0.pkl 0811Size3_t0
  
  screen -S 0811Size4_t0
  python ./0811GenSize4.py 0811Size4_t0.pkl 0811Size4_t0

  screen -S 0811Size5_t0
  python ./0811GenSize5.py 0811Size5_t0.pkl 0811Size5_t0
  ```
  
- ```python
  # selected Morpheus benchmarks within camb6.tyrell DSL
  # 4 is uniting on numerics, which can't pass the precondition check
  # 38,49 is gathering on columns of different types
  # 83,85 is uniting on numeric and string
  # 5,15 column names won't match, no better solution for now, drop
  # unknown inconsistent: 20
  bmrk_size3 = [1,2,3,9,10,13,21,22,23,31,42,44,47,51,52,53,56,59,60,61,63,64,90,]
  # 33 is gathering on different types
  # 58 column names won't match, no better solution for now, drop
  bmrk_size4 = [6,81,88,]
  bmrk_size5 = [7,11,]
  ```

- ```
  screen -S 0812Size3Relaxed_t0
  python ./0812GenSize3Relaxed.py 0812Size3Relaxed_t0.pkl 0812Size3Relaxed_t0
  ```

- 

## Some Resources

- https://www.facebook.com/icml.imls/videos/2970931166257998/
- https://himanshusahni.github.io/2018/02/23/reinforcement-learning-never-worked.html



