from dephp import phpast as syn
import sys, pprint

def flatten(S):
    if S == []:
        return S
    if isinstance(S[0], list):
        return flatten(S[0]) + flatten(S[1:])
    return S[:1] + flatten(S[1:])

class PrettyOutputter(object):
    def __init__(self, ast):
        self.ast = ast
        self.out = sys.stdout

    def run(self):
        def visitor(node, children):
            print node.__class__
            if isinstance(node, syn.Variable):
                return node.name

            if isinstance(node, syn.String):
                return '"{0}"'.format(node.value)

            if isinstance(node, syn.AssignOp):
                return "{0} {1} {2};\n".format(*flatten(children))

            if isinstance(node, syn.Program):
                return "".join(flatten(children))

            return "<unknown node: {0}>".format(node.__class__.__name__)

        self.out.write(str(self.ast.depth_first(visitor)))
        self.out.write("\n")

class NoopOutputter(object):
    """Does nothing.  Useful if you just want to output debugging
    information."""
    def __init__(self, ast):
        pass

    def run(self):
        pass
