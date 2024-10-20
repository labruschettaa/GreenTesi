from swiplserver import PrologMQI
import random as rnd
from data import FactoryNode, NodeT, Microservice
import argparse, os, re
from enum import Enum


def getParameters():
    prsr = argparse.ArgumentParser(description='Run experiments')
    prsr.add_argument('aFile', type=str, help='Application file')
    prsr.add_argument('iFile', type=str, help='Infrastructure file')
    prsr.add_argument('--mode', type=str, choices=['quick', 'opt', 'base'], required=True, help='Mode of operation')
    prsdArgs = prsr.parse_args()
    return prsdArgs.aFile, prsdArgs.iFile, prsdArgs.mode


def printDict(dict):
    for n in dict:
        print(f"\t\t\t{n} -> {dict[n]}")


def insert(ms, n, dict):
    if n not in dict:
        dict[n] = [ms]
    else:
        dict[n].append(ms)
    return dict


def unpackP(place):
    dictP = {}
    for p in place:
        ms, n = p['args'][0], p['args'][1]
        insert(ms, n, dictP)
    return dictP


with PrologMQI() as mqi:
    aFile, iFile, mode = getParameters()
    print(f"\nGenerating file for {aFile}, {iFile}, {mode}...")
    with mqi.create_thread() as prolog_thread:
        prolog_thread.query(f"consult('{aFile}').")
        prolog_thread.query(f"consult('{iFile}').")
        prolog_thread.query(f"consult('main.pl').")
        prolog_thread.query_async(f"timedPlacement({mode}, App, P, SCI, N, Time).")
        result = prolog_thread.query_async_result()[0]
        if isinstance(result, bool):
            print("No solution found")
        else:
            resultP =[]
            unpackedP = unpackP(result['P'])
            for p in result['P']:
                resultP.append(p['args'])
            print(f"\n\nRESULTS   ->\tTIME = {result['Time']:.6f}\n\t\tPACKING =")
            printDict(unpackedP)
            print(f"\t\tSCI = {result['SCI']}\n\t\tNUMBER OF NODES = {result['N']}\n\n")
        

