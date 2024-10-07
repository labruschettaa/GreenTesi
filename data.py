from enum import Enum
import random

class NodeT(Enum):
    NANO = 0
    MICRO = 1
    SMALL = 2
    MEDIUM = 3
    LARGE = 4
    XLARGE = 5
    X2LARGE = 6


EMISSIONS = [
    {
        "name": "gas",
        "emission": 0.610
    },
    {
        "name": "coal",
        "emission": 1.1
    },
    {
        "name": "onshorewind",
        "emission": 0.0097
    },
    {
        "name": "offshorewind",
        "emission": 0.0165
    },
    {
        "name": "solar",
        "emission": 0.05
    }
]



class Node:
    def __init__(self, name, ncpu, ram, bwin, bwout, e, el, te, pue, i):
        self.name = name
        self.ncpu = ncpu
        self.ram = ram
        self.bwin = bwin
        self.bwout = bwout
        self.e = e
        self.el = el
        self.te = te
        self.pue = pue
        self.i = i
    def __str__(self):
        return f'Node: {self.name}, tor({self.ncpu},{self.ram},{self.bwin},{self.bwout}), {self.e}, {self.el}, {self.te}, {self.pue}'

class Microservice:
    def __init__(self, name, ncpu, ram, bwin, bwout, tir):
        self.name = name
        self.ncpu = ncpu
        self.ram = ram
        self.bwin = bwin
        self.bwout = bwout
        self.tir = tir
    def __str__(self):
        return f'Microservice: {self.name}, rr({self.ncpu},{self.ram},{self.bwin},{self.bwout}), {self.tir}'

class FactoryNode:
    numNodes = [0] * len(NodeT)
    
    @staticmethod
    def getRandNode():
        """Generates a random `node` class `Node`."""
        randNum = random.randint(0, len(NodeT) - 1)
        node = FactoryNode.getNode(NodeT(randNum))
        return node

    @staticmethod
    def getNode(nodeType:NodeT):
        """Generates a node class `Node` of the specified value `nodeType`."""
        te = random.randint(1000, 2000)
        el = random.randint(3, 7)
        pue = random.uniform(1.1, 3.0)
        numNode = str(FactoryNode.numNodes[nodeType.value])
        i = EMISSIONS[random.randint(0, len(EMISSIONS) - 1)]['emission']
        FactoryNode.numNodes[nodeType.value] += 1
        match nodeType:
            case NodeT.NANO:
                e = random.uniform(0.001, 0.005)
                return Node("n_" + numNode, 2, 0.5, 5, 5, e, el, te, pue, i)
            case NodeT.MICRO:
                e = random.uniform(0.005, 0.01)
                return Node("mi_" + numNode, 2, 1, 5, 5, e, el, te, pue, i)
            case NodeT.SMALL:
                e = random.uniform(0.01, 0.015)
                return Node("s_" + numNode, 2, 2, 5, 5, e, el, te, pue, i)
            case NodeT.MEDIUM: 
                e = random.uniform(0.015, 0.025)
                return Node("m_" + numNode, 2, 4, 12.5, 12.5, e, el, te, pue, i)
            case NodeT.LARGE:
                e = random.uniform(0.025, 0.04)
                return Node("l_" + numNode, 2, 8, 12.5, 12.5, e, el, te, pue, i)
            case NodeT.XLARGE:
                e = random.uniform(0.04, 0.06)
                return Node("xl_" + numNode, 4, 16, 12.5, 12.5, e, el, te, pue, i)
            case NodeT.X2LARGE:
                e = random.uniform(0.06, 0.1)
                return Node("xl2_" + numNode, 8, 32, 15, 15, e, el, te, pue, i)
            case _:
                raise Exception("Invalid node type.")
            
    @staticmethod  
    def resetNumNodes():
        """Resets the number of nodes generated for each type of node."""
        FactoryNode.numNodes = [0] * len(NodeT)
