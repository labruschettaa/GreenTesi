from swiplserver import PrologMQI, PrologThread
import random as rnd
from data import FactoryNode

def generateNodes(num, filename):
    """Generates a file in the testing directory with the name `filename` containing 2**`num` nodes."""
    powerOfNum = 2**num
    nodes = ""
    intensities = ""
    for n in range(powerOfNum):
        node = FactoryNode.getRandNode()
        nodes+=f"node('{node.name}', tor({node.ncpu}, {node.ram}, {node.bwin}, {node.bwout}), {node.e}, {node.el}, {node.te}, {node.pue}).\n"
        intensities+=f"carbon_intensity('{node.name}', {node.i}).\n"

    with open(filename, "w") as file:
        file.write(nodes+intensities)
    

def place(app, infra, mode):
    with mqi.create_thread() as prolog_thread:
        prolog_thread.query("consult('main.pl')")

        prolog_thread.query(f"consult('{app}')")

        prolog_thread.query(f"consult('{infra}')")

        prolog_thread.query_async(f"timedPlacement({mode},A,P,SCI,N,Time)", find_all=False)

        return prolog_thread.query_async_result()


with PrologMQI() as mqi:
    
        rnd.seed(481183)   
        n = 20
        generateNodes(n, 'resources/infra.pl')

        result = place('resources/application.pl', 'resources/infra.pl', 'quick')

        if result is None:
            print('No (more) results')
        else:
            print(result)
        
     