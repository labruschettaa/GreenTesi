/*
%# Calcola R in base agli anni che viene seguito il microservizio.
totalR(Microservice, R) :-
    microservice(Microservice, _, TiR, RPerYear),
    R is RPerYear * TiR.
*/

%# Controlla che il microservizio M possa essere supportato dal nodo N.
/*isNodeElegible(Node, rr(CPUReq,RAMReq,BWinReq,BWoutReq)) :-
    node(Node, tor(CPU,RAM,BWin,BWout), _, _, _, _),
    CPU >= UCPU + CPUReq, 
    RAM >= URAM + RAMReq, 
    BWin >= UBWin + BWinReq, 
    BWout >= UBWout + BWoutReq.


%# Custom filter per filtrare solo i nodi N_i che possono supportare M.
filterNodes(Nodes, RR, FilteredNodes) :-
    include({RR}/[Node]>>isNodeElegible(Node, RR), Nodes, FilteredNodes).
*/

/*
%# Funzione principale che si occupa di restituire un array di nodi N_i in grado
%# di supportare il microservizio M, ordinati in ordine crescente di SCI.
sciSortedNodes(Microservice, SortedNodes) :-
    microservice(Microservice, _, _),
    findall((SCI,Node), sci(Node, Microservice, SCI), Nodes),
    sort(Nodes, SortedNodes).

bestCandidateNode(Microservice, Node, SCI) :-
    sciSortedNodes(Microservice, [(SCI, Node)|_]).
*/