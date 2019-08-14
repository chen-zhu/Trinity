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

p_setting = {
	"fixed_depth": 2,
	"data_to": "./morpheus_data_size1.pkl",
	"spec_from": "./example/camb3.tyrell",
	"nper": 1000, # number of programs per shell_list
	"ntry": 10000,
}

# p_setting = {
# 	"fixed_depth": 3,
# 	"data_to": "./morpheus_data_size2.pkl",
# 	"spec_from": "./example/camb3.tyrell",
# 	"nper": 10, # number of programs per shell_list
# 	"ntry": 500000, # 14400
# }

print(p_setting)

m_interpreter = MorpheusInterpreter()
m_spec = S.parse_file(p_setting["spec_from"])
m_generator = MorpheusGenerator(m_spec, m_interpreter)

# ==== collecting ====
# first collect all the possible progs
m_enumerator = RandomEnumeratorFD(m_spec, fixed_depth=p_setting["fixed_depth"])
m_prog_set = set()
for i in range(p_setting["ntry"]):
	print("\r# Enumerating programs {}/{}".format(
		len(m_prog_set), i
	),end="")
	tmp_prog = m_enumerator.next()
	m_prog_set.add(tmp_prog)
print()
print("# Collected {} valid programs.".format(len(m_prog_set)))

# ==== generating ====
print("# Start filling in programs...")
dt = {}
nth = 0
for p_prog in m_prog_set:
	nth += 1
	# first try some examples
	# if one of them succeed, then proceed to filling
	# otherwise, dump the prog since it's invalid
	str_prog = str(p_prog)
	print("\r# ({}/{}) Detecting: {}...".format(
		nth, len(m_prog_set), str_prog
	),end="")
	for _ in range(100):
		tmp_input = m_interpreter.random_table()
		try:
			tmp_eval = m_interpreter.eval(
				p_prog,
				[tmp_input],
			)
		except Exception:
			continue

		# still need to pass the sanity check before creating a key
		tmp_example = Example(
			input=[tmp_input],
			output=tmp_eval,
		)
		tmp_ps = ProgramSpace(
			m_spec, m_interpreter, tmp_example.input, tmp_example.output,
		)
		# load the program into ProgramSpace
		tmp_ps.init_by_prog(p_prog)
		# do sanity check
		tmp_check = m_interpreter.sanity_check(tmp_ps)
		if tmp_check[0]:
			# if you reach here, then the program is valid
			# create a key
			dt[str_prog] = []
			break
		else:
			# fail the sanity check, restart
			continue

	if str_prog in dt:
		# valid program
		# start generating examples
		nd_last = len(dt[str_prog])
		nd_curr = len(dt[str_prog])
		nk = 0
		while len(dt[str_prog])<p_setting["nper"]:
			nd_last = nd_curr
			nd_curr = len(dt[str_prog])
			if nd_last==nd_curr:
				nk += 1
			else:
				nk = 0
			if nk>1000:
				print("\r# ({}/{}) Filling  : {}...{}, too slow, drop".format(
					nth, len(m_prog_set), str_prog, len(dt[str_prog]),
				),end="")
				if len(dt[str_prog])==0:
					del dt[str_prog]
				break

			print("\r# ({}/{}) Filling  : {}...{}".format(
				nth, len(m_prog_set), str_prog, len(dt[str_prog]),
			),end="")
			tmp_input = m_interpreter.random_table()
			try:
				tmp_eval = m_interpreter.eval(
					p_prog,
					[tmp_input],
				)
			except Exception:
				continue
			# if you reach here, then the example is good
			tmp_example = Example(
				input=[tmp_input],
				output=tmp_eval,
			)
			tmp_ps = ProgramSpace(
				m_spec, m_interpreter, tmp_example.input, tmp_example.output,
			)
			# load the program into ProgramSpace
			tmp_ps.init_by_prog(p_prog)
			# do sanity check
			tmp_check = m_interpreter.sanity_check(tmp_ps)
			if tmp_check[0]:
				# succeed, append
				str_example = Example(
					input=[m_interpreter.renv(p).r_repr() for p in tmp_example.input],
					output=m_interpreter.renv(tmp_example.output).r_repr(),
				)
				dt[str_prog].append(
					(p_prog, str_example)
				)
			else:
				# fail the sanity check, restart
				continue
		print()


	else:
		print("\r# ({}/{}) Detecting: {}...Failed".format(
			nth, len(m_prog_set), str_prog
		))
		continue

print("# Done Filling.")
print("# Total Valid Templates: {}".format(len(dt.keys())))
print("# Total Valid Programs: {}".format(
	sum([len(dt[dkey]) for dkey in dt.keys()])
))

print("# Writing to file...")
with open(p_setting["data_to"],"wb") as f:
	pickle.dump(dt, f)
print("# Done.")









