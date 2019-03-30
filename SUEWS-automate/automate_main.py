import pandas as pd
import numpy as np
# import sub_modules_automate as sa
import json


def find_add(filePath, in_file, query, to_add):

    print('Reading file:' + in_file)
    with open(filePath+in_file, 'r') as ifile:  # Reading the file
        buf = ifile.readlines()

    out_file = in_file

    print('Writing file:'+out_file)
    with open(filePath+out_file, 'w') as ofile:
        for line in buf:
            if(query in line):
                line = line+to_add+'\n'
            ofile.write(line)

    print('Modifying file='+in_file+' is finished\n')


change_list=json.load(open('changes_list.json'))

for key in change_list.keys():
    in_file = key
    filePath = change_list[key]["filePath"]
    query = change_list[key]["query"]
    for qkey in query.keys():
        find_add(filePath, in_file, qkey, query[qkey])
