# coding=utf-8
#@Time	  : 2018-12-09 11:24
#@Author   : Monolith
#@FileName : file_operation.py
#@License  : (C)LINKINGMED,2018
#@Contact  : baibaidj@126.com, 18600369643


import os

data_root = r'/media/dejun/holder/datavisualizeR/data/18.12.10'

for subroot, subdir, subfile in os.walk(data_root):
    if len(subfile)==2:
        hosp = subroot.split('/')[-1]
        if hosp!='18.12.10':
            old_name_1 = os.path.join(subroot, subfile[0])
            old_name_2 = os.path.join(subroot, subfile[1])
            new_name_1 = os.path.join(data_root, '.'.join([hosp, subfile[0]]))
            new_name_2 = os.path.join(data_root, '.'.join([hosp, subfile[1]]))
            os.rename(old_name_1, new_name_1)
            os.rename(old_name_2, new_name_2)

