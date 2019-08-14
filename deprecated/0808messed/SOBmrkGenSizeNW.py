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
	"size": 4,
	"data_to": "./0807SOsize3.pkl",
	"spec_from": "./example/camb5.tyrell",
	"sketch_from": "./0807SODICS.pkl",
}

print(m_setting)

m_interpreter = MorpheusInterpreter()
m_spec = S.parse_file('./example/camb5.tyrell')
m_generator = MorpheusGenerator(
    spec=m_spec,
    interpreter=m_interpreter,
)

with open(m_setting["sketch_from"],"rb") as f:
	sodics = pickle.load(f)

def get_sketch_from_prog(p_prog):
	tmps_str = str(p_prog)
	tmp_s = tmps_str.index('@')
	tmp_sl= tmps_str[:tmp_s].split("(")[::-1]
	tmp_sls=[p for p in tmp_sl if p]
	return tuple(tmp_sls)

# sample a sketch every time and filter the enumerator
tmp_enumerator = RandomEnumeratorFD(m_spec, fixed_depth = m_setting["size"]+1)
# tmp_enumerator = RandomEnumeratorFD(m_spec, fixed_depth = 2)
SODT = []
print("# Start collecting...")
while True:
	print("# #collected: {}".format(len(SODT)))
	target_prog = random.choices(
		list(sodics[m_setting["size"]].keys()),
		list(sodics[m_setting["size"]].values()),
		k=1,
	)[0]
	# target_prog = ("unite",)
	print("# Target: {}".format(target_prog))
	while True:
		tmp_prog = tmp_enumerator.next()
		str_prog = get_sketch_from_prog(tmp_prog)
		if str_prog==target_prog:
			# find a match, move on to fill in the programs
			break
		# else:
		# 	print("(unmatched) target:{}, generated:{}".format(
		# 		str(target_prog), str(str_prog),
		# 		))

	print("# Trying: {}".format(tmp_prog))
	is_solvable = True
	_exp_cnt = 0
	while True:
		tmp_input = m_interpreter.random_table()
		# m_interpreter.print_obj(tmp_input)
		# input("PAUSE")
		try:
			tmp_eval = m_interpreter.eval(
				tmp_prog,
				[tmp_input],
			)
			break
		except Exception as e:
			# print(str(e))
			_exp_cnt += 1
			if _exp_cnt>=100:
				is_solvable = False
				break
			continue

	if not is_solvable:
		print("# Failed: unsolvable.")
		# print("# EF: Sketch {} with Prog {}, skip".format(target_prog, str(tmp_prog)))
		continue
	
	# then wrap into ProgramSpace and do sanity check
	tmp_example = Example(input=[tmp_input],output=tmp_eval)
	tmp_ps = ProgramSpace(
		m_spec, m_interpreter, tmp_example.input, tmp_example.output,
	)
	tmp_ps.init_by_prog(tmp_prog)
	# do the sanity check
	tmp_check = m_interpreter.sanity_check(tmp_ps)
	if not tmp_check[0]:
		# fail
		# print(tmp_check[1])
		print("# Failed: sanity failure {}".format(tmp_check[1]))
		# print("# SC: Sketch {} with Prog {}, skip".format(target_prog, str(tmp_prog)))
		continue

	# then it's successful
	print("# Succeeded.".format(target_prog))
	str_example = Example(
		input=[m_interpreter.renv(p).r_repr() for p in tmp_ps.inputs],
		output=m_interpreter.renv(tmp_ps.output).r_repr(),
	)
	SODT.append(
		(tmp_prog, str_example)
	)
	# with open(m_setting["data_to"]) as f:
	# 	pickle.dump(m_setting["data_to"])

print()
print("# Done.")


















