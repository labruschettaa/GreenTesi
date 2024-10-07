from swiplserver import PrologMQI, PrologThread
from data import FactoryNode
import random, time, argparse, subprocess, os, re


TESTING_DIRECTORY = 'resources/testing/'
 
def checkInput(appName, arrayNums, prolog_thread):
    """Checks if the input `arrayNums` is a set of distinct positive integers and if `appName` is a valid application name."""
    try:
        arrayNums = [ int(x) for x in arrayNums ]
    except:
        raise Exception("The input must be a set of distinct positive integers.")
    if not all(isinstance(x, int) for x in arrayNums) or not all(i >= 0 for i in arrayNums) or not len(arrayNums) == len(set(arrayNums)):
        raise Exception("The input must be a set of distinct positive integers.")
    prolog_thread.query("consult('resources/variables.pl').")
    result = prolog_thread.query(f"application({appName}, _, _).")
    if not isinstance(result, bool) or not result:
        raise Exception("Invalid appName.")
    return arrayNums


def generateNodes(num, filename):
    """Generates a file in the testing directory with the name `filename` containing 2**`num` nodes."""
    powerOfNum = 2**num
    nodes = []
    carbonIs = []
    for n in range(powerOfNum):
        node = FactoryNode.getRandNode()
        nodes.append(f"node('{node.name}', tor({node.ncpu}, {node.ram}, {node.bwin}, {node.bwout}), {node.e}, {node.el}, {node.te}, {node.pue}).")
        carbonIs.append(f"carbon_intensity('{node.name}', {node.i}).")
    prolog_thread.query(f"create(experimentNodes, '{filename}', {nodes}, {carbonIs}).")


def runExperiment(filename, appName, prolog_thread: PrologThread, heuristic=False):
    """Runs the experiment for the file `filename` using the `prolog_thread`."""
    start = time.perf_counter()
    if parsedArgs.h:
        prolog_thread.query(f"consult('ghost.pl').")
        prolog_thread.query(f"experimentalEnvironment('{filename}').")
        prolog_thread.query_async(f"qPlacement('{appName}', P, SCI, NumberOfNodes).")
    else:
        prolog_thread.query(f"consult('main.pl').")
        prolog_thread.query(f"experimentalEnvironment('{filename}').")
        prolog_thread.query_async(f"minPlacement('{appName}', P, SCI, NumberOfNodes).")
    print(f"Running test for {filename}...")
    result = prolog_thread.query_async_result()
    end = time.perf_counter()
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
    
    

def sort_key(filename):
    match = re.search(r'n(\d+)\.pl', filename)
    return int(match.group(1)) if match else float('inf')


def getDirectoryFiles(directory):
    try:
        files = [f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]
        files.sort(key=sort_key)
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

    if '--r' in parsedArgs.args:
        parsedArgs.r = True
    if '--h' in parsedArgs.args:
        parsedArgs.h = True
    appName = parsedArgs.appName
    args = [arg for arg in parsedArgs.args if not arg.startswith('--')]
    with mqi.create_thread() as prolog_thread:
        prolog_thread.query("consult('experiment.pl').")
        arrayNums = checkInput(appName, args, prolog_thread)
        files = []
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
            runExperiment(filename, appName, prolog_thread, parsedArgs.h)
     