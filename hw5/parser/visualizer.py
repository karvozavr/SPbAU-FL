from collections import deque

import graphviz as gv


def render_ast(node, dir='rendered'):
    ast = gv.Graph()
    queue = deque()
    ast.attr('node', shape='box')
    ast.node(name=str(id(node)), label=str(node))
    queue.appendleft(node)
    while len(queue) > 0:
        node = queue.pop()

        for child in node.children:
            queue.appendleft(child)
            ast.node(name=str(id(child)), label=str(child) + '\n' + str(child.info))
            ast.edge(str(id(node)), str(id(child)))

    ast.render(filename='ast', directory=dir, view=True, cleanup=True)
    ast.save(filename='ast.dot', directory=dir)


def renderNode(node):
    info = node.info
    if info is None:
        return '''<<TABLE>
                <TR>
                    <TD>{name}</TD>
                </TR>
            </TABLE>>
            '''.format(name=node.node_name)
    else:
        return '''
            <<TABLE>
                <TR>
                    <TD>{name}</TD>
                </TR>
                <TR>
                    <TD>Line: {line}</TD>
                    <TD>Index: {left}-{right}</TD>
                </TR>
            </TABLE>>
            '''.format(name=node.node_name,
                       line=node.info.line,
                       left=node.info.interval[0],
                       right=node.info.interval[1])
