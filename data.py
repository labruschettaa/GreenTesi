from enum import Enum
import random

class NodeT(Enum):
    MEDIUM = 0
    LARGE = 1
    XLARGE = 2
    X2LARGE = 3
    X4LARGE = 4
    X8LARGE = 5
    X12LARGE = 6
    X16LARGE = 7


class Node:
    def __init__(self, name, ncpu, ram, bwin, bwout, e, el, te, pue):
        self.name = name
        self.ncpu = ncpu
        self.ram = ram
        self.bwin = bwin
        self.bwout = bwout
        self.e = e
        self.el = el
        self.te = te
        self.pue = pue
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
        randNum = random.randint(0,7)
        node = FactoryNode.getNode(NodeT(randNum))
        return node

    @staticmethod
    def getNode(nodeType:NodeT):
        """Generates a node class `Node` of the specified value `nodeType`."""
        te = 2000
        el = random.randint(3,7)
        pue = random.uniform(1.1, 3.0)
        numNode = str(FactoryNode.numNodes[nodeType.value])
        FactoryNode.numNodes[nodeType.value] += 1
        match nodeType:
            case NodeT.MEDIUM: 
                return Node("m_" + numNode, 1, 4, 12.5, 12.5, 0.01, el, te, pue)
            case NodeT.LARGE:
                return Node("l_" + numNode, 2, 8, 12.5, 12.5, 0.015, el, te, pue)
            case NodeT.XLARGE:
                return Node("xl_" + numNode, 4, 16, 12.5, 12.5, 0.025, el, te, pue)
            case NodeT.X2LARGE:
                return Node("xl2_" + numNode, 8, 32, 15, 15, 0.04, el, te, pue)
            case NodeT.X4LARGE:
                return Node("xl4_" + numNode, 16, 64, 15, 15, 0.06, el, te, pue)
            case NodeT.X8LARGE:
                return Node("xl8_" + numNode, 32, 128, 15, 15, 1, el, te, pue)
            case NodeT.X12LARGE:
                return Node("xl12_" + numNode, 48, 192, 22.5, 22.5, 1.5, el, te, pue)
            case NodeT.X16LARGE:
                return Node("xl16_" + numNode, 64, 256, 30, 30, 2, el, te, pue)
            
    @staticmethod  
    def resetNumNodes():
        """Resets the number of nodes generated for each type of node."""
        FactoryNode.numNodes = [0] * len(NodeT)
