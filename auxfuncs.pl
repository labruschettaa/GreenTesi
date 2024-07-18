
%# Calcola R in base agli anni che viene seguito il microservizio.
totalR(Microservice, R) :-
    microservice(Microservice, _, TiR, RPerYear),
    R is RPerYear * TiR.

%# Controlla che il microservizio M possa essere supportato dal nodo N.
isNodeElegible(Node, RR) :-
    node(Node, ToR, _, _, _, _),
    compareREQ(ToR, RR).
compareREQ([Tn|ToR], [Rm|RR]) :-
    Tn >= Rm,
    compareREQ(ToR, RR).
compareREQ([],[]).

%# Custom filter per filtrare solo i nodi N_i che possono supportare M.
filterNodes(Nodes, RR, FilteredNodes) :-
    include({RR}/[Node]>>isNodeElegible(Node, RR), Nodes, FilteredNodes).
