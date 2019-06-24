'''
program generator to help speed up training phase, can work in multi-processing mode
'''

from utils_morpheus import *
from ProgramSpace import *

import tyrell.spec as S
from tyrell.decider import Example

import os
import sys
import time
import fcntl
import pickle

from pathlib import Path

def main():
	p_max_depth = 3

	if len(sys.argv)!=2:
		print("# INVALID.")
		sys.exit()

	try:
		myid = int(sys.argv[1])
	except:
		print("# ERROR PARSING.")
		sys.exit()

	print("# myid: {}".format(myid))

	data_path = "./pworker_storage/data_{}.pkl".format(myid)
	Path(data_path).touch()

	data_list = []

	spec = S.parse_file('example/set1.tyrell')
	mcoder = MorpheusInterpreter()

	while True:
		new_data = []
		for i in range(30):
			print("\r# gen: {}".format(i),end="")

			while True:
				minput = mcoder.random_table()
				# mcoder.print_obj(minput)
				# print(robjects.r(minput).r_repr())
				generator = MorpheusGenerator(
					spec=spec,
					interpreter=mcoder,
					sfn=mcoder.sanity_check,
				)
				p_prog, p_example = generator.generate(
					fixed_depth=p_max_depth,
					example=Example(input=[minput],output=None),
				)
				if p_prog is not None and p_prog.is_apply():
					break

			# Morpheus Only: serializable
			str_example = Example(
				input=[robjects.r(p).r_repr() for p in p_example.input],
				output=robjects.r(p_example.output).r_repr(),
			)

			if False:
				is_pass = None
				# Debug: reload test
				bp_example = Example(
					input=[get_fresh_name() for p in str_example.input],
					output=get_fresh_name(),
				)
				for i in range(len(bp_example.input)):
					mcoder.load_data_into_var(
						str_example.input[i],
						bp_example.input[i],
					)
				mcoder.load_data_into_var(
					str_example.output,
					bp_example.output,
				)
				print("reload input: {}".format(
					eq_r(p_example.input[0],bp_example.input[0])
				))
				print("reload output: {}".format(
					eq_r(p_example.output,bp_example.output)
				))

			# print("========INPUT========")
			# mcoder.print_obj(p_example.input[0])
			# print("========OUTPUT========")
			# mcoder.print_obj(p_example.output)
			# print("========VECTOR========")
			# print(morpheus_mutate(
			# 	p_example.input[0],
			# 	p_example.output
			# ))
			# print(p_prog)
			# time.sleep(1)
			new_data.append(
				# first and last
				(p_prog,str_example)
			)
		print()
		# write back to the data file
		with open(data_path,"rb+") as f:
			fcntl.flock(f.fileno(), fcntl.LOCK_EX) # will wait 
			if os.path.getsize(data_path)>0:
				old_data = pickle.load(f)
			else:
				print("# init old_data")
				old_data = []
			print("# old_data:{}, combined:{}".format(len(old_data),len(old_data+new_data)))
			f.seek(0) # should add this, otherwise 5,5,5
			pickle.dump(old_data+new_data,f)
			# time.sleep(3)

		# done, start next cycle



if __name__ == "__main__":
	main()