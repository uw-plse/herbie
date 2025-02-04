vars = set()
nodes = []

op_parse = dict({
            'div':'/',
            'mul':'*',
            'add':'+',
            'sub':'-',
            'square':'pow',
            'sqrt':'sqrt',
            'exp':'exp',
            'ln':'log',
            'abs':'fabs',
            'max':'fmax',
            'min':'fmin',
            'neg':'-',
            'cos':'cos',
            'sin':'sin'})

def parse_node(node):
    idx = int(node[0].replace('_', ''), 16)
    op = node[1]
    if 'var' in op:
        vars.add(op.replace('var-', ''))
        nodes.append([op.replace('var-', '')])
    elif 'const' == op:
        nodes.append([node[2]])
    elif op == 'square':
        nodes.append(['pow'] + [node[2]] + ['2'])
    else:
        op = op_parse[op]
        nodes.append([op] + node[2:])
           

with open("hi.vm", "r") as file:
    for line in file:
        if line[0] != '#' and line[0] != '\n':
            node = line.replace('\n', '').split(' ')
            parse_node(node)
          
def recurse_nodes(idx):
    node = nodes[idx]
    if len(node) == 2:
        idx1 = int(node[1].replace('_', ''), 16)
        return '(' + node[0] + ' ' + recurse_nodes(idx1) + ')'
    elif len(node) == 3 and node[0] == 'pow' and node[2] == '2':
        idx1 = int(node[1].replace('_', ''), 16)
        return '(' + node[0] + ' ' + recurse_nodes(idx1) + ' ' + node[2] + ')'
    elif len(node) == 3:
        idx1 = int(node[1].replace('_', ''), 16)
        idx2 = int(node[2].replace('_', ''), 16)
        return '(' + node[0] + ' ' + recurse_nodes(idx1) + ' ' + recurse_nodes(idx2) + ')'
    else:
        return node[0]
        
print(recurse_nodes(len(nodes)-1))
print(vars)

            
