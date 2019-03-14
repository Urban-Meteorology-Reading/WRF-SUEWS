import pandas as pd
import numpy as np
import sub_modules_automate as sa
import json


change_list=json.load(open('changes_list.dict'))

for key in change_list.keys():
    in_file = key
    filePath = change_list[key]["filePath"]
    query = change_list[key]["query"]
    for qkey in query.keys():
        sa.find_add(filePath, in_file, qkey, query[qkey])

