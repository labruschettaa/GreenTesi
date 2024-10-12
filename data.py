from enum import Enum
import random
from typing import Union

EMISSIONS = [
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
    },
    {
        "name": "gas",
        "emission": 0.610
    },
    {
        "name": "coal",
        "emission": 1.1
    }
]

FITPUE = 1.1
FITCI = EMISSIONS[1]['emission']
CLEAN_PUE = 1.05
CLEAN_CI = EMISSIONS[0]['emission']
DIRTY_PUE = 3.0
DIRTY_CI = EMISSIONS[len(EMISSIONS)-1]['emission']

class NodeS(Enum):
    NANO = 0
    MICRO = 1
    SMALL = 2
    MEDIUM = 3
    LARGE = 4
    XLARGE = 5
    X2LARGE = 6

class NodeT(Enum):
    RANDOM = 0
    FIT = 1
    BROKEN = 2
    DIRTY = 3


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
    numNodesS = [0] * len(NodeS)
    numNodesT = [0] * len(NodeT)

    @staticmethod
    def node(node: Union[NodeS, NodeT], ms:Microservice=None):
        if isinstance(node, NodeS):
            return FactoryNode.__nodeS(node)
        elif isinstance(node, NodeT):
            return FactoryNode.__nodeT(node, ms)
        else:
            raise Exception("Invalid node type.")

    
    @staticmethod
    def __randNode():
        """Generates a random `node` class `Node`."""
        randNum = random.randint(0, len(NodeS) - 3)
        node = FactoryNode.node(NodeS(randNum))
        return node    

    @staticmethod
    def __nodeT(nodeType:NodeT, ms:Microservice=None):
        numNode = str(FactoryNode.numNodesT[nodeType.value])
        e = random.uniform(0.020, 0.025)
        te = random.randint(1100, 2000)
        el = random.randint(4, 7)
        FactoryNode.numNodesT[nodeType.value] += 1
        match nodeType:
            case NodeT.RANDOM:
                return FactoryNode.__randNode()
            case NodeT.FIT:
                if ms is None:
                    raise Exception("Microservice not provided.")
                return Node("f_" + numNode, ms.ncpu, ms.ram, ms.bwin, ms.bwout, 0.015, 3, 1000, FITPUE, FITCI)
            case NodeT.DIRTY:
                if ms is None:
                    raise Exception("Microservice not provided.")
                return Node("d_" + numNode, ms.ncpu, ms.ram, ms.bwin, ms.bwout, e, el, te, DIRTY_PUE, DIRTY_CI)
            case NodeT.BROKEN:
                if ms is None:
                    raise Exception("Microservice not provided.")
                specs = {0: ms.ncpu, 1: ms.ram, 2: ms.bwin, 3: ms.bwout}
                broken_spec = random.randint(0, len(specs)-1)
                specs[broken_spec] = 0
                return Node("b_" + numNode, specs[0], specs[1], specs[2], specs[3], 0.015, 3, 1000, CLEAN_PUE, CLEAN_CI)
            case _:
                raise Exception("Invalid node type.")
            
    @staticmethod
    def __nodeS(nodeType:NodeS):
        """Generates a node class `Node` of the specified value `NodeSype`."""
        te = random.randint(1000, 2000)
        el = random.randint(3, 7)
        pue = random.uniform(1.1, 3.0)
        numNode = str(FactoryNode.numNodesS[nodeType.value])
        i = EMISSIONS[random.randint(0, len(EMISSIONS) - 1)]['emission']
        FactoryNode.numNodesS[nodeType.value] += 1
        match nodeType:
            case NodeS.NANO:
                e = random.uniform(0.001, 0.005)
                return Node("n_" + numNode, 2, 0.5, 5, 5, e, el, te, pue, i)
            case NodeS.MICRO:
                e = random.uniform(0.005, 0.01)
                return Node("mi_" + numNode, 2, 1, 5, 5, e, el, te, pue, i)
            case NodeS.SMALL:
                e = random.uniform(0.01, 0.015)
                return Node("s_" + numNode, 2, 2, 5, 5, e, el, te, pue, i)
            case NodeS.MEDIUM: 
                e = random.uniform(0.015, 0.025)
                return Node("m_" + numNode, 2, 4, 12.5, 12.5, e, el, te, pue, i)
            case NodeS.LARGE:
                e = random.uniform(0.025, 0.04)
                return Node("l_" + numNode, 2, 8, 12.5, 12.5, e, el, te, pue, i)
            case NodeS.XLARGE:
                e = random.uniform(0.04, 0.06)
                return Node("xl_" + numNode, 4, 16, 12.5, 12.5, e, el, te, pue, i)
            case NodeS.X2LARGE:
                e = random.uniform(0.06, 0.1)
                return Node("xl2_" + numNode, 8, 32, 15, 15, e, el, te, pue, i)
            case _:
                raise Exception("Invalid node type.")
            
    @staticmethod  
    def __resetNumNodesS():
        """Resets the number of nodes generated for each type of node."""
        FactoryNode.numNodesS = [0] * len(NodeS)
