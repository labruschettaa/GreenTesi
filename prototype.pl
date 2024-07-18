
%# Include di file ausiliari.
:- consult('variables.pl').
:- consult('auxfuncs.pl').
:- use_module(library(apply)).

%# Calcolo della quantità di energia necessaria per eseguire il micrservizio M
%# sul nodo N per tutta la TiL.
operationalEnergy(Node, Microservice, E) :-
    node(Node, _, PowerPerCPU, _, _, PUE),
    microservice(Microservice, _, TiR),
    E is PUE * (TiR * 365 * 24) * PowerPerCPU.

%# Calcolo dell' intensità di carbonio dell'esecuzione del microservizio M su un nodo N.
operationalCarbon(Node, Microservice, O) :-
    carbon_intensity(_, I),
    operationalEnergy(Node, Microservice, E),
    O is E * I.

%# Calcolo dell' intensità di carbonio embodied del microservizio M su un nodo N.
embodiedCarbon(Node, Microservice, M) :-
    node(Node, tor(CPU,_,_,_), _, EL, TE, _),
    microservice(Microservice, rr(CPUReq,_,_,_), TiR),
    TS is TiR / EL,
    RS is CPUReq / CPU,
    M is TE * TS * RS.

%# Calcolo del SCI.
sci(Node, Microservice, SCI) :-
    node(Node,_,_,_,_,_),
    functionalUnits(R),
    microservice(Microservice, _, _),
    embodiedCarbon(Node, Microservice, M),
    operationalCarbon(Node, Microservice, O),
    C is (M + O),
    SCI is C / R.

%# Funzione principale che si occupa di restituire un array di nodi N_i in grado
%# di supportare il microservizio M, ordinati in ordine crescente di SCI.
sciSortedNodes(Microservice, SortedNodes) :-
    microservice(Microservice, _, _),
    findall((SCI,Node), sci(Node, Microservice, SCI), Nodes),
    sort(Nodes, SortedNodes).

bestCandidateNode(Microservice, Node, SCI) :-
    sciSortedNodes(Microservice, [(SCI, Node)|_]).