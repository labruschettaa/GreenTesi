from swiplserver import PrologMQI, PrologThread
from data import Node, Microservice
import argparse, os
from typing import List, Union
from enum import Enum
# fare una versione di testing.py che usa heuristic.py per ordinare i file generati.

MICROSERVICE_FILE = 'resources/microservices.pl'
NODES_FILE = 'resources/nodes.pl'
TESTING_DIRECTORY = 'resources/testing/'
HEURISTIC_DIRECTORY = 'resources/testing/heuristic/'

class ElemT(Enum):
    NODE = 0
    MICROSERVICE = 1


def assignScores(type: ElemT, lst: List[object]):

    SIZE_KEYS = ["ncpu", "ram", "bwin", "bwout"]
    CARBON_KEYS = ["pue", "ec", "i"]
    arrRes = []

    def findMinMax(arr, keyFun):
        if not arr:
            return {"min": None, "max": None}
        min = keyFun(arr[0])
        max = keyFun(arr[0])
        for elem in arr:
            val = keyFun(elem)
            if val < min:
                min = val
            if val > max:
                max = val
        return min, max

    def operation(factor, elem, min, max):
        if min == max:
            return 0
        return factor * (elem - min) / (max - min)

    def sizeScore(ms, min, max):
        sizeScore = 0
        for key in SIZE_KEYS:
            sizeScore = sizeScore + operation(1/4, ms.__getattribute__(key), min[key], max[key])
        return sizeScore

    def carbonScore(node, min, max):
        carbonScore = 0
        for key in CARBON_KEYS:
            if key == "pue": 
                carbonScore = carbonScore + operation(1/3, node["node"].__getattribute__(key), min[key], max[key])
            else:
                carbonScore = carbonScore + operation(1/3, node[key], min[key], max[key])
        return carbonScore

    if type == ElemT.MICROSERVICE:
        min, max = {key: None for key in SIZE_KEYS}, {key: None for key in SIZE_KEYS}
        for key in SIZE_KEYS:
            min[key], max[key] = findMinMax(lst, lambda ms: ms.__getattribute__(key))
        for elem in lst:
            arrRes.append({"ms": elem, "sizeScore": sizeScore(elem, min, max)})
    
    elif type == ElemT.NODE:
        SIZE_AND_CARBON_KEYS = SIZE_KEYS + CARBON_KEYS
        min, max = {key: None for key in SIZE_AND_CARBON_KEYS}, {key: None for key in SIZE_AND_CARBON_KEYS}
        for index, key in enumerate(SIZE_AND_CARBON_KEYS):
            min[key], max[key] = findMinMax(lst, lambda node: node["node"].__getattribute__(key)) if index < len(SIZE_KEYS)+1 else findMinMax(lst, lambda node: node[key])
        for elem in lst:
            arrRes.append({"node": elem["node"], "carbonScore": carbonScore(elem, min, max),"sizeScore": sizeScore(elem["node"], min, max)})
    
    else:
        raise Exception("Invalid type.")
    
    return arrRes


def writeElems(type, lst, file):
    if parsedArgs.t and type == ElemT.NODE:
        file = HEURISTIC_DIRECTORY + parsedArgs.file
        directory = os.path.dirname(file)
        if not os.path.exists(directory):
            os.makedirs(directory)
    with open(file, 'w') as f:
        if type == ElemT.NODE and not parsedArgs.t:
            f.write(":- dynamic node/6.\n\n")
        for elem in lst:
            if type == ElemT.NODE:
                node = elem["node"]
                f.write(f"node({node.name}, tor({node.ncpu}, {node.ram}, {node.bwin}, {node.bwout}), {node.e}, {node.el}, {node.te}, {node.pue}).\n")
            elif type == ElemT.MICROSERVICE:
                ms = elem["ms"]
                f.write(f"microservice({ms.name}, rr({ms.ncpu}, {ms.ram}, {ms.bwin}, {ms.bwout}), {ms.tir}).\n")
            else:
                raise Exception("Invalid type.")
            

def readElems(type, file):
    if type == ElemT.NODE:
        prolog_thread.query_async("node(N, tor(NCPU, RAM, BWIn, BWOut), E, EL, TE, PUE).")
    elif type == ElemT.MICROSERVICE:
        prolog_thread.query_async("microservice(M, rr(NCPU, RAM, BWIn, BWOut), TiR).")
    lstElemsPL, lstElems = prolog_thread.query_async_result(), []
    for elem in lstElemsPL:
        if type == ElemT.NODE:
            node = Node(elem['N'], elem['NCPU'], elem['RAM'], elem['BWIn'], elem['BWOut'], elem['E'], elem['EL'], elem['TE'], elem['PUE'])
            crbnI = prolog_thread.query(f"carbon_intensity({node.name}, I).")
            embdCrbn = node.te / node.el
            lstElems.append({"node": node, "i": crbnI[0]['I'], "ec": embdCrbn})
        elif type == ElemT.MICROSERVICE:
            elem = Microservice(elem['M'], elem['NCPU'], elem['RAM'], elem['BWIn'], elem['BWOut'], elem['TiR'])
            lstElems.append(elem)
        else:
            raise Exception("Invalid type.")
    return lstElems


with PrologMQI() as mqi:
    nodesFile = NODES_FILE
    parser = argparse.ArgumentParser(description='Run the heuristic version of the program.')
    parser.add_argument('--t', action='store_true', help='Set testing flag')
    parser.add_argument('file', type=str, default=nodesFile, help='The file that contain the nodes.')
    parsedArgs = parser.parse_args()
    if parsedArgs.t:
        nodesFile = TESTING_DIRECTORY + parsedArgs.file
    nodes, mss = [], []
    
    with mqi.create_thread() as prolog_thread:
        prolog_thread.query("consult('main.pl').")
        if parsedArgs.t:
            prolog_thread.query("consult('experiment.pl').")
            prolog_thread.query("cleanup.")
            prolog_thread.query(f"consult('{nodesFile}').")
        nLst = readElems(ElemT.NODE, nodesFile)
        msLst = readElems(ElemT.MICROSERVICE, MICROSERVICE_FILE)

    nLst = assignScores(ElemT.NODE, nLst)
    msLst = assignScores(ElemT.MICROSERVICE, msLst)
    nLstSrtd = sorted(nLst, key=lambda x: (x["carbonScore"], -x["sizeScore"]))
    msLstSrtd = sorted(msLst, key=lambda x: -x["sizeScore"])
    writeElems(ElemT.NODE, nLstSrtd, nodesFile)
    writeElems(ElemT.MICROSERVICE, msLstSrtd, MICROSERVICE_FILE)




    