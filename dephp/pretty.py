from dephp import phpast as syn
import sys

class PrettyOutputter(object):
    def __init__(self, ast):
        self.ast = ast
        self.out = sys.stdout

    def run(self):
        self.loop(self.ast)

    def loop(self, ast):
        for node in ast:
            if isinstance(node, syn.Class):
                self.out.write("class " + node.name + " {\n")
                self.loop(node.nodes)
                self.out.write("}\n")
            else:
                pass
                # print node
                # self.loop(node)
