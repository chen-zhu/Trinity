'''
offline dataset generator
for chain execution only
'''
from MorpheusInterpreter import *
from ProgramSpace import *

import tyrell.spec as S
from tyrell.decider import Example
from tyrell.enumerator import RandomEnumeratorFD


import pickle

m_setting = {
	"fixed_depth": 5,
	"data_to": "./0731MDsize4.pkl",
	"spec_from": "./example/camb3.tyrell",
	"ntotal": 1000,
}


print(m_setting)

m_interpreter = MorpheusInterpreter()
m_spec = S.parse_file('./example/camb3.tyrell')
m_generator = MorpheusGenerator(
    spec=m_spec,
    interpreter=m_interpreter,
)


dt = []
print("# Start collecting...")
for i in range(m_setting["ntotal"]):
	print("\r# {}/{}".format(i, m_setting["ntotal"]),end="")
	tmp_ps = m_generator.get_new_chain_program(
		m_setting["fixed_depth"],
	)
	p_prog = tmp_ps.node_list[-1] # only for chain
	str_example = Example(
		input=[m_interpreter.renv(p).r_repr() for p in tmp_ps.inputs],
		output=m_interpreter.renv(tmp_ps.output).r_repr(),
	)
	dt.append(
		(p_prog, str_example)
	)
	with open(m_setting["data_to"],"wb") as f:
		pickle.dump(dt,f)

print()
print("# Done.")



















