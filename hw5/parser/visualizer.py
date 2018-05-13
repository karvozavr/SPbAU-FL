import graphviz as gv
from collections import deque
from parser.parser import *


def main():
    g1 = gv.Graph(format='png')

    g1.node('A')
    g1.node('B')
    g1.edge('A', 'B')

    g1.view()
    print(g1.source)

    filename = g1.render(filename='img/tree.dot')


def render_ast(node):
    graph = gv.Graph(format='png')
    queue = deque()
    graph.node(name=str(id(node)), label=str(node))
    queue.appendleft(node)
    while len(queue) > 0:
        node = queue.pop()

        for child in node.children:
            queue.appendleft(child)
            graph.node(name=str(id(child)), label=str(child) + '\n' + str(child.info))
            graph.edge(str(id(node)), str(id(child)))

    graph.view()
    print(graph.source)


if __name__ == '__main__':
    main()
