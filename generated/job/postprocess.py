import os
import re
import sys

pattern = (
	r'(std::string|int) (\w+) = (""|0);\n'
	r'\s*for \(const auto &(\w+) : (\w+)\) {'
	r'\n\s*\n'
	r'\s*.+ \+= (.+);\n'
	r'\s*}'
)

type2inf = {
	"std::string": '"zzzzzzzzzzzzzzzzz"',
	"int": "INT_MAX"
}


def replacement(match):
	tpe = match.group(1)
	mn_var = match.group(2)
	off_var = match.group(4)
	it_var = match.group(5)
	update_val = match.group(6)
	rpl = (
		f"{tpe} {mn_var} = {type2inf[tpe]};\n"
		f"for (const auto &{off_var}: {it_var}) {{\n"
		f"{mn_var} = std::min({mn_var}, {update_val});\n"
		f"}}"
	)
	return rpl


def process(filename):
	print(filename)
	file_path = os.path.join(os.path.dirname(__file__), filename)

	with open(file_path, "r") as cpp_file:
		content = cpp_file.read()

	content = re.sub(pattern, replacement, content, re.MULTILINE)

	with open(file_path, "w") as cpp_file:
		cpp_file.write(content)

	os.system(f'clang-format -i {file_path} -style "{{ColumnLimit: 120, IndentWidth: 4}}"')


if __name__ == '__main__':
	args = sys.argv[1:]

	if args:
		filename = args[0]
		process(filename)
	else:
		for filename in os.listdir(os.path.dirname(__file__)):
			if filename.endswith("cpp"):
				print(filename)
				process(filename)
