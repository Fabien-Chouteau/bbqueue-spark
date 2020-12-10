import fileinput
import re

for line in fileinput.input():
    line = line.strip()
    if line.startswith("[Counterexample] "):
        line = line.strip("[Counterexample] ")
        # print(line)
        pattern = re.compile(r'([a-zA-Z_]+) => ([0-9]+)[,)]', re.IGNORECASE)
        value = {}
        for match in pattern.findall(line):
            value[match[0]] = int(match[1])

        pattern = re.compile(r'This\.Buf\'Last = ([0-9]+)', re.IGNORECASE)
        all = pattern.findall(line)
        if all != []:
            value['Size'] = int(pattern.findall(line)[0])
        pattern = re.compile(r'Max = ([0-9]+)', re.IGNORECASE)
        all = pattern.findall(line)
        if all != []:
            value['Size'] = int(pattern.findall(line)[0])

        print(value)

        if 'Size' in value:
            print('|' + '-' * value['Size'] + '|')
        for key in ['Write', 'Read', 'Reserve', 'Last']:
            if key in value:
                print(' ' + ' ' * value[key] + key[0])

