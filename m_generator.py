#!/usr/bin/env python
from utils_morpheus import *

def main():

    logger.info('Parsing Spec...')
    spec = S.parse_file('example/m.tyrell')
    logger.info('Parsing succeeded')

    mcoder = MorpheusInterpreter()
    minput = mcoder.random_table()
    print("========INPUT(random)========")
    print(robjects.r(minput))
    generator = MorpheusGenerator(
        spec=spec,
        interpreter=mcoder,
        sfn=mcoder.sanity_check,
    )
    train_sample = generator.generate(
        max_depth=4,
        example=Example(input=[minput], output=None),
        probs=(1,5),
    )
    print("========PROGRAM========")
    print(train_sample[0])
    print("========OUTPUT========")
    print(robjects.r(train_sample[1].output))


if __name__ == '__main__':
    logger.setLevel('DEBUG')
    main()