from swiplserver import PrologMQI
import random as rnd
from data import FactoryNode, NodeT, Microservice
import argparse, os
from enum import Enum


TESTING_DIRECTORY = "resources/testing/"

class Mode(Enum):
    FKOPT = 0
    OPT = 1
    QUICK = 2
    BASE = 3
    ALL = 4


def generateNodes(num, nodeT:NodeT, ms:Microservice=None):
    """Generates a file in the testing directory with the name `filename` containing 2**`num` nodes."""
    nodes = ""
    intensities = ""
    for _ in range(num):
        node = FactoryNode.node(nodeT, ms)
        nodes+=f"node('{node.name}', tor({node.ncpu}, {node.ram}, {node.bwin}, {node.bwout}), {node.e}, {node.el}, {node.te}, {node.pue}).\n"
        intensities+=f"carbon_intensity('{node.name}', {node.i}).\n"
    return nodes, intensities


def consult(files):
    with mqi.create_thread() as prolog_thread:
        for file in files:
            prolog_thread.query(f"consult('{file}')")


def generateFileNodes(n, filename, mode:Mode, app=None):
    nodes, intensities = "", ""
    msList = []
    if mode == Mode.FKOPT:
        consult([app])
        with mqi.create_thread() as prolog_thread:
            msPList = prolog_thread.query(f"application(A, MS, EPs)")[0]['MS']
            if n < len(msPList):
                raise Exception(f"Number of nodes ({n}) must be greater than the number of microservices ({len(msPList)}).")
            k = n - len(msPList)
            for msP in msPList: 
                ms = prolog_thread.query(f"microservice({msP}, rr(CPU, RAM, BWIN, BWOUT), TiR)")[0]
                ms = Microservice(msP, ms['CPU'], ms['RAM'], ms['BWIN'], ms['BWOUT'], ms['TiR'])
                msList.append(ms)
            for _ in range(int(k/2)):
                randMs = rnd.choice(msList)
                brokenNode, brokenIntensity = generateNodes(1, NodeT.BROKEN, randMs)
                dirtyNode, dirtyIntensity = generateNodes(1, NodeT.DIRTY, randMs)
                nodes += dirtyNode + brokenNode
                intensities += dirtyIntensity + brokenIntensity
            for ms in msList:
                fitNode, fitIntensity = generateNodes(1, NodeT.FIT, ms)
                nodes += fitNode 
                intensities += fitIntensity 
    else:
        nodes, intensities = generateNodes(n, NodeT.RANDOM)
    if not os.path.exists(TESTING_DIRECTORY):
        os.makedirs(TESTING_DIRECTORY)
    with open(filename, "w") as file:
        file.write(nodes+intensities)


def place(app, infra, m:Mode):
    if m==Mode.FKOPT:
        mode = "quick"
    else :
        mode = m.name.lower()
    with mqi.create_thread() as prolog_thread:
        consult([infra, app, 'main.pl', 'resources/flags.pl'])
        prolog_thread.query_async(f"timedPlacement({mode},A,P,SCI,N,Time)", find_all=False)
        result = prolog_thread.query_async_result()
        return result


def checkInput(aFile, nList):
    """Checks if the input `nList` is a set of distinct positive integers greater than 1 and if `aFile` is a valid application name."""
    try:
        nList = [ int(x) for x in nList ]
    except:
        raise Exception("The input must be a set of distinct integers greater than 1.")
    if not all(isinstance(x, int) for x in nList) or not all(i > 1 for i in nList) or not len(nList) == len(set(nList)):
        raise Exception("The input must be a set of distinct positive integers greater than 1.")
    if not os.path.isfile(aFile):
        raise Exception("Invalid aFile.")
    return nList


def cleanDirectory(dir):
    try:
        for fname in os.listdir(dir):
            fpath = os.path.join(dir, fname)
            if os.path.isfile(fpath):
                os.remove(fpath)
            elif os.path.isdir(fpath):
                os.rmdir(fpath)
    except Exception as e:
        print(f"Failed to delete: {fpath}. Reason: {e}")


def getParameters():
    prsr = argparse.ArgumentParser(description='Run experiments')
    prsr.add_argument('aFile', type=str, help='Application file')
    prsr.add_argument('args', type=int, nargs='+', help='List of integers')
    prsr.add_argument('--mode', type=str, choices=['fkopt', 'opt', 'quick', 'base', 'all'], required=True, help='Mode of operation')
    prsr.add_argument('--seed', type=int, help='Seed value')
    prsr.add_argument('--Save', action='store_true', help='Keep the testing directory')
    prsr.add_argument('--rerun', action='store_true', help='Runs the experiment again')
    prsdArgs = prsr.parse_args()
    checkInput(prsdArgs.aFile, prsdArgs.args)
    return prsdArgs.aFile, prsdArgs.args, Mode[prsdArgs.mode.upper()], prsdArgs.seed, prsdArgs.Save, prsdArgs.rerun


def unpackP(place):
    dictP = {}
    for p in place:
        ms, n = p['args'][0], p['args'][1]
        if n not in dictP:
            dictP[n] = [ms]
        else:
            dictP[n].append(ms)
    return dictP

def printP(p):
    for n in p:
        print(f"\t{n} -> {p[n]}")


with PrologMQI() as mqi:
    aFile, nList, mode, seed, save, rerun = getParameters()

    if seed is not None:
        rnd.seed(seed)   

    if not rerun:
        for n in nList:
            fname = TESTING_DIRECTORY + f"n_{n}.pl"
            generateFileNodes(n, fname, mode, aFile)

    for n in nList:
        f = f"n_{n}.pl"
        result = place(aFile, TESTING_DIRECTORY + f, mode)
        print()
        if isinstance(result, bool):
            print(f"""
                TEST FOR {f} RETURNED {result}.
                """)
        else:
            result, resultP =  result[0], []
            unpackedP = unpackP(result['P'])
            for p in result['P']:
                resultP.append(p['args'])
            print(f"\nTEST FOR {f} ENDED IN {result['Time']:.6f} SECONDS.\nRESULTS   ->  PACKING =")
            printP(unpackedP)
            print(f"\nSCI = {result['SCI']}\nNUMBER OF NODES = {result['N']}\n")
    print()
    if not save:
        cleanDirectory(TESTING_DIRECTORY)


def test():
    result = place('resources/application.pl', 'resources/infra.pl', 'quick')

    if result is None:
        print('No (more) results')
    else:
        print(result)
        
     