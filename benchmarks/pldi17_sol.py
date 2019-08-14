from sexpdata import Symbol

solutions = {
    1: [Symbol('spread'),
            [Symbol('unite'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1,4]],
                ],
                [Symbol('ColInt'),3],
                [Symbol('ColInt'),1],
            ],
            [Symbol('ColInt'),1],
            [Symbol('ColInt'),3],
        ],
    2: [Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    3: [Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    4: [Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    5: [Symbol('neg_select'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]]
            ],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColList'),[3]],
    ],
    6: [Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('separate'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1,5]],
                ],
                [Symbol('ColInt'),3],
            ],
            [Symbol('ColList'),[1,3]],
        ],
        [Symbol('Aggr'),"sum"],
        [Symbol('ColInt'),5],
    ],
    7: [Symbol('neg_select'),
        [Symbol('mutate'),
            [Symbol('spread'),
                [Symbol('separate'),
                    [Symbol('neg_gather'),
                        [Symbol('@param'),0],
                        [Symbol('ColList'),[1,6]],
                    ],
                    [Symbol('ColInt'),3],
                ],
                [Symbol('ColInt'),3],
                [Symbol('ColInt'),5],
            ],
            [Symbol('NumFunc'),"/"],
            [Symbol('ColInt'),4],
            [Symbol('ColInt'),5],
        ],
        [Symbol("ColList"),[2]],
    ],
    8: [Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('inner_join'),
                [Symbol('summarise'),
                    [Symbol('group_by'),
                        [Symbol('@param'),0],
                        [Symbol('ColList'),[1]],
                    ],
                    [Symbol('Aggr'),"mean"],
                    [Symbol('ColInt'),4],
                ],
                [Symbol('@param'),0],
            ],
            [Symbol('ColList'),[1,2]],
        ],
        [Symbol('Aggr'),"mean"],
        [Symbol('ColInt'),4],
    ],
    9: [Symbol('neg_select'),
        [Symbol('mutate'),
            [Symbol('spread'),
                [Symbol('@param'),0],
                [Symbol('ColInt'),3],
                [Symbol('ColInt'),4],
            ],
            [Symbol('NumFunc'),"/"],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),4],
        ],
        [Symbol('ColList'),[3,4]],
    ],
    10:[Symbol('neg_select'),
        [Symbol('gather'),
            [Symbol('separate'),
                [Symbol('@param'),0],
                [Symbol('ColInt'),4],
            ],
            [Symbol('ColList'),[4,5]],
        ],
        [Symbol('ColList'),[4]],
    ],
    11:[Symbol('mutate'),
        [Symbol('spread'),
            [Symbol('separate'),
                [Symbol('summarise'),
                    [Symbol('group_by'),
                        [Symbol('@param'),0],
                        [Symbol('ColList'),[1]],
                    ],
                    [Symbol('Aggr'),"mean"],
                    [Symbol('ColInt'),2],
                ],
                [Symbol('ColInt'),1],
            ],
            [Symbol('ColInt'),1],
            [Symbol('ColInt'),3],
        ],
        [Symbol('NumFunc'),"/"],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    12: None, # `dest`== "SEA" is not supported
    13:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    14:[Symbol('select'),
        [Symbol('filter'),
            [Symbol('separate'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1]]
                ],
                [Symbol('ColInt'),2],
            ],
            [Symbol('BoolFunc'),">"],
            [Symbol('ColInt'),4],
            [Symbol('SmallInt'),0],
        ],
        [Symbol('ColList'),[1,3]],
    ],
    15:[Symbol('spread'),
            [Symbol('separate'),
                [Symbol('gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[3,5]],
                ],
                [Symbol('ColInt'),4],
            ],
            [Symbol('ColInt'),4],
            [Symbol('ColInt'),6],
        ],
    16: None, # `W2` > 26.86 is not supported
    17: None, # `Group` == "A" is not supported
    18: None, # n() in summarise is not supported
    19: None, # n() in summarise is not supported
    20:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],
    21:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),4],
    ],
    22:[Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[4,5]],
            ],
            [Symbol('ColList'),[4]],
        ],
        [Symbol('Aggr'),"mean"],
        [Symbol('ColInt'),5],
    ],
    23:[Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColList'),[1]],
        ],
        [Symbol('Aggr'),"sum"],
        [Symbol('ColInt'),3],
    ],
    24: None, # `value` > 1090 is not supported
    25: None, # n() in summarise is not supported
    26: None, # there's an arrange that is not supported, also input2
    27: None, # contains input2, temporarily not supported
    28: None, # contains input2, temporarily not supported
    # 29 has 2 inputs, but the solution only utilizes 1
    29: None, # there's an arrange that is not supported, also input2
    30: None, # MISSING

    # 30 in FSE
    31:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol("ColList"),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),4],
    ],

    # 31 in FSE
    32:[Symbol('inner_join'),
        [Symbol('summarise'),
            [Symbol('group_by'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1]],
                ],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('Aggr'),"mean"],
            [Symbol('ColInt'),3],
        ],
        [Symbol('@param'),0],
    ],

    # 32 in FSE
    33:[Symbol('neg_select'),
        [Symbol('spread'),
            [Symbol('separate'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1,2]],
                ],
                [Symbol('ColInt'),3],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),5],
        ],
        [Symbol('ColList'),[1,3]],
    ],

    # 33 in FSE
    34: None, # it gathers all, which is currently not supported

    # 34 in FSE
    35: None, # contains input2, temporarily not supported

    # 35 in FSE
    36: None, # Morpheus timeout

    # 36 in FSE
    37: None, # n/sum(n) temporarily not supported

    # 37 in FSE
    38:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),5],
    ],

    # 38 in FSE
    39: None, # contains input2, temporarily not supported

    # 39 in FSE
    40: None, # contains arrange

    # 40 in FSE
    41: None, # contains input2

    # 41 in FSE
    42:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[5]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),4],
    ],

    # 42 in FSE
    43: None, # contains input2

    # 43 in FSE
    44:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),4],
    ],

    45: None, # MISSING

    # 45 in FSE
    46: None, # `COL` != "EMP", not supported

    # 46 in FSE
    47:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    48: None, # MISSING

    # 47 in FSE
    49:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),4],
    ],

    50: None, # MISSING

    # 48 in FSE
    51:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 49 in FSE
    52:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),4],
    ],

    # 50 in FSE
    53:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 51 in FSE
    54: None, # unsupported filter conditions

    # 52 in FSE
    55: None, # unsupported filter conditions

    # 53 in FSE
    56:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),1],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),1],
        [Symbol('ColInt'),3],
    ],

    57: None, # MISSING

    # 55 in FSE
    58:[Symbol('spread'),
        [Symbol('summarise'),
            [Symbol('group_by'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1,5]],
                ],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('Aggr'),"mean"],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 56 in FSE
    59:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),4],
        [Symbol('ColInt'),5],
    ],

    # 57 in FSE
    60:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),4],
    ],

    # 58 in FSE
    61:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),4],
    ],

    # 59 in FSE
    62: None, # spread and gather, require a string match of column name in cell

    # 60 in FSE
    63:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 61 in FSE
    64:[Symbol('spread'),
        [Symbol('separate'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1,2]],
            ],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),3],
        [Symbol('ColInt'),5],
    ],

    65: None, # MISSING
    66: None, # MISSING
    67: None, # MISSING
    68: None, # MISSING
    69: None, # MISSING
    70: None, # MISSING
    71: None, # MISSING
    72: None, # MISSING
    73: None, # MISSING

    # 65 in FSE
    74: None, # Morpheus Timeout

    75: None, # MISSING
    76: None, # MISSING

    # 66 in FSE
    77:[Symbol('select'),
        [Symbol('filter'),
            [Symbol('summarise'),
                [Symbol('group_by'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[2]],
                ],
                [Symbol('Aggr'),"sum"],
                [Symbol('ColInt'),1],
            ],
            [Symbol('BoolFunc'),">"],
            [Symbol('ColInt'),2],
            [Symbol('SmallInt'),6],
        ],
        [Symbol("ColList"),[2]],
    ],

    # 67 in FSE
    78: None, # unsupported filter conditions

    # 68 in FSE
    79: None, # unsupported n() in summarise

    80: None, # MISSING

    # 69 in FSE
    81:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('separate'),
                [Symbol('gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[2,3]],
                ],
                [Symbol('ColInt'),1],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 70 in FSE
    82:[Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('filter'),
                [Symbol('gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[3,4]],
                ],
                [Symbol('BoolFunc'),">"],
                [Symbol('ColInt'),4],
                [Symbol('SmallInt'),0],
            ],
            [Symbol('ColList'),[3]],
        ],
        [Symbol('Aggr'),"mean"],
        [Symbol('ColInt'),2],
    ],

    # 71 in FSE
    83:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[2,3]],
            ],
            [Symbol('ColInt'),2],
            [Symbol('ColInt'),3],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    # 72 in FSE
    84:[Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('filter'),
                [Symbol('@param'),0],
                [Symbol('BoolFunc'),">"],
                [Symbol('ColInt'),3],
                [Symbol('SmallInt'),0],
            ],
            [Symbol('ColList'),[1,2]],
        ],
        [Symbol('Aggr'),"mean"],
        [Symbol('ColInt'),3],
    ],

    # 73 FSE
    85:[Symbol('summarise'),
        [Symbol('group_by'),
            [Symbol('unite'),
                [Symbol('@param'),0],
                [Symbol('ColInt'),2],
                [Symbol('ColInt'),3],
            ],
            [Symbol('ColList'),[2]],
        ],
        [Symbol('Aggr'),"mean"],
        [Symbol('ColInt'),3],
    ],

    86: None, # MISSING
    87: None, # MISSING

    # 75 in FSE
    88:[Symbol('neg_select'),
        [Symbol('spread'),
            [Symbol('separate'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),[1,2]],
                ],
                [Symbol('ColInt'),3],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),5],
        ],
        [Symbol('ColList'),[3]],
    ],

    89: None, # MISSING

    # 76 in FSE
    90:[Symbol('spread'),
        [Symbol('unite'),
            [Symbol('gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[3,4]],
            ],
            [Symbol('ColInt'),3],
            [Symbol('ColInt'),2],
        ],
        [Symbol('ColInt'),2],
        [Symbol('ColInt'),3],
    ],

    91: None, # MISSING

    # 77 in FSE
    92: None, # contains n() in summarise

    # 78 in FSE
    93: None, # contains unsupported filter condition

    # 79 in FSE
    94: None, # contains unsupported filter condition

    # 80 in FSE
    95:[Symbol('select'),
        [Symbol('filter'),
            [Symbol('neg_gather'),
                [Symbol('@param'),0],
                [Symbol('ColList'),[1]],
            ],
            [Symbol('BoolFunc'),">"],
            [Symbol('ColInt'),3],
            [Symbol('SmallInt'),0],
        ],
        [Symbol('ColList'),[1,2]],
    ],

    96: None, # contains input2



    "test": [Symbol('spread'),
            [Symbol('unite'),
                [Symbol('neg_gather'),
                    [Symbol('@param'),0],
                    [Symbol('ColList'),['1','4']],
                ],
                [Symbol('ColInt'),3],
                [Symbol('ColInt'),1],
            ],
            [Symbol('ColInt'),1],
            [Symbol('ColInt'),8],
        ],
}