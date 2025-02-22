import re
exprs = []
with open("fullpatchedexprs") as f:
    for line in f:
        exprs.append(line.strip().split(','))

for i in range(1000):
    print(exprs[len(exprs)-1-i][0],',',exprs[len(exprs)-1-i][1])
