## Instructions

In Jupyter, open one of the following notebook and run:

- `0610_RL1_AlphaNeo_ChainOneNB.ipynb`: train an agent using Morpheus DSL (except "inner_join")
- `0610_RL1_AlphaNeo_Simple.ipynb`: train an agent using Morpheus DSL (with only "select")
- `0610_RL1_AlphaNeo_Cheating.ipynb`: train an agent using Morpheus DSL (with only "select") and specifically designed features that gives indications of argument. E.g., does the ith column from the current output appear in the target output.

To speed up the training, enable multi processing using the `AlphaNeo_pworker.py` or `AlphaNeo_Delta_pworker.py`:

`python ./AlphaNeo_pworker.py <worker_id>`

Open up several (at most 3 suggested) instances for every `worker_id` and this will enable the program to train and generate training samples simultaneously. 

For training `0610_RL1_AlphaNeo_Simple.ipynb` and `0610_RL1_AlphaNeo_Cheating.ipynb`, use `AlphaNeo_Delta_pworker.py`.

For training `0610_RL1_AlphaNeo_ChainOneNB.ipynb`, use `AlphaNeo_pworker.py`.

The folder `pworker_storage` is used to store generated programs, empty it before you start new training. If you want to enable multi processing, you may train only one of the above 3 agent at a time, since they are sharing the same `pworker_storage` folder and files.

