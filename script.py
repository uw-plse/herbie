import re
import math
import warnings
maxLength = math.inf

def subExprs(s):
    ret = []
    stack = []

    for i, c in enumerate(s):
        if c == '(':
            stack.append(i)  # Push the index of '(' onto the stack
        elif c == ')':
            if stack:
                start = stack.pop()  # Pop the index of the matching '('
                subexpr = s[start:i+1]  # Extract the subexpression
                ret.append(subexpr)
    return ret


def renameVars(s):
    toReplace = list(re.findall(r"\b(?!\+|-|\*|\/|f64|s|approx|f32|neg|PI|literal|binary64|binary32|fabs|fma|exp|exp2|expm1|log|log10|log2|log1p|pow|sqrt|cbrt|hypot|sin|cos|tan|asin|acos|atan|atan2|sinh|cosh|tanh|asinh|acosh|atanh|erf|erfc|tgamma|lgamma|ceil|floor|fmod|remainder|fmax|fmin|fdim|copysign|trunc|round|nearbyint|<|>|<=|>=|==|!=|and|or|not|isfinite|isinf|isnan|isnormal|signbit)([a-zA-Z_][.a-zA-Z0-9_-]*)\b",s))
    newToReplace = []
    for i in toReplace:
        if i not in newToReplace: newToReplace.append(i)
    toReplace = newToReplace
    if(len(toReplace) == 0):
        return ''
    for i,r in enumerate(toReplace):
        s = re.sub(fr'\b{r}\b',f'z{i}',s)
    return s
def isFP(s):
    return 'approx' not in s and ('.f64' in s or '.f32' in s)

def isAccelerator(s):
    return not re.match(r'\(\S+\.(f64|f32) z0( z1)?\)',s)

lines = []
final = {}
exprs = dict()

with open("fullherbieexprs") as f:
    for i,line in enumerate(f):
        if(len(line) > maxLength):
            continue
        # if 'approx' in line:
        #     continue
        warnings.warn(i)
        for subExpr in subExprs(line):
            if('if' in subExpr):
                continue
            renamed = renameVars(subExpr)
            if(isFP(renamed) and isAccelerator(renamed)):
                if(renamed not in exprs):
                    exprs[renamed] = 1
                else:
                    exprs[renamed] +=1


sortedExprs = [k for k, v in sorted(exprs.items(), key=lambda item: item[1])]
# for i in sortedExprs:
#     print(i,',',exprs[i])

for i in range(len(sortedExprs)):
    print(sortedExprs[i],',',exprs[sortedExprs[i]])