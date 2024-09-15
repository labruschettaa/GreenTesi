from swiplserver import PrologMQI, PrologThread
from data import Node, NodeT
import random
import time
import argparse
import subprocess
import os

TESTING_DIRECTORY = 'resources/testing/'
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

    
def checkInput(appName, arrayNums, prolog_thread):
    """Checks if the input `arrayNums` is a set of distinct positive integers and if `appName` is a valid application name."""
    try:
        arrayNums = [ int(x) for x in arrayNums ]
    except:
        raise Exception("The input must be a set of distinct positive integers.")
    if not all(isinstance(x, int) for x in arrayNums) or not all(i >= 0 for i in arrayNums) or not len(arrayNums) == len(set(arrayNums)):
        raise Exception("The input must be a set of distinct positive integers.")
    result = prolog_thread.query(f"application({appName}, _, _).")
    if not isinstance(result, bool) or not result:
        raise Exception("Invalid appName.")
    return arrayNums


def generateNodes(num, filename):
    """Generates a file in the testing directory with the name `filename` containing 2**`num` nodes."""
    powerOfNum = 2**num
    for n in range(powerOfNum):
        node = FactoryNode.getRandNode()
        prolog_thread.query(f"create(experimentNode, '{filename}', {node.name}, tor({node.ncpu}, {node.ram}, {node.bwin}, {node.bwout}), {node.e}, {node.el}, {node.te}, {node.pue}).")


def runExperiment(filename, appName, prolog_thread: PrologThread, heuristic=False):
    """Runs the experiment for the file `filename` using the `prolog_thread`."""
    start = time.time()
    result = prolog_thread.query_async(f"minPlacement('{filename}', {appName}, P, SCI, NumberOfNodes).")
    result = prolog_thread.query_async_result()
    end = time.time()
    duration = end - start
    if isinstance(result, bool):
        print(f"""
            TEST FOR {filename} RETURNED {result}, ENDED IN {duration:.6f} SECONDS.
              """)
        return
    result = result[0]
    resultP = []
    for p in result['P']:
        resultP.append(p['args'])
    print(f"""
        TEST FOR {filename} ENDED IN {duration:.6f} SECONDS.
        RESULTS   ->  PACKING = {resultP},
                      SCI = {result['SCI']}
                      NUMBER OF NODES = {result['NumberOfNodes']}
          """)
    

def getDirectoryFiles(directory):
    try:
        files = [f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]
        return files
    except FileNotFoundError:
        print(f"The directory {directory} does not exist.")
        return []

with PrologMQI() as mqi:
    parser = argparse.ArgumentParser(description='Run experiments with heuristic flag.')
    parser.add_argument('--h', action='store_true', help='Set heuristic flag')
    parser.add_argument('--r', action='store_true', help='Rerun the experiment on the existing files.')
    parser.add_argument('appName', type=str, help='Application name')
    parser.add_argument('args', nargs=argparse.REMAINDER, help='Additional arguments')
    parsedArgs = parser.parse_args()
    appName = parsedArgs.appName
    args = [arg for arg in parsedArgs.args if not arg.startswith('--')]
    with mqi.create_thread() as prolog_thread:
        prolog_thread.query("consult('experiment.pl').")
        arrayNums = checkInput(appName, args, prolog_thread)
        files = []
        print(parsedArgs.r)
        if parsedArgs.r:
            files = getDirectoryFiles(TESTING_DIRECTORY)
        else:
            prolog_thread.query("create(experiment).")
            for num in arrayNums:
                prolog_thread.query("cleanup.")
                filename = 'n' + str(num) + '.pl'
                files.append(filename)
                FactoryNode.resetNumNodes()
                generateNodes(num, filename)
        for filename in files:
            if parsedArgs.h:
                subprocess.run(['python', '--t==true'], filename)
            runExperiment(filename, appName, prolog_thread, parsedArgs.h)
     