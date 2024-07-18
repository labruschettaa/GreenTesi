
%# Include di file ausiliari.
:- consult('variables.pl').
:- consult('auxfuncs.pl').
:- use_module(library(apply)).

%# Calcolo della quantità di energia necessaria per eseguire il micrservizio M
%# sul nodo N per tutta la TiL.
energy_to_execute_m(Node, Microservice, EnergyToExecuteM) :-
    node(Node, _, EnergyPerCPU, _, _, PUE),
    microservice(Microservice, _, TiL, _),
    EnergyToExecuteM is PUE * (TiL * 365 * 24) * EnergyPerCPU.

%# Calcolo dell' intensità di carbonio dell'esecuzione del microservizio M su un nodo N.
operational_carbon_emissions(Node, Microservice, OperationalCarbon) :-
    carbon_intensity(_, Carbon_Intensity),
    transport(_, Transport_Factor),
    energy_to_execute_m(Node, Microservice, EnergyToExecuteM),
    OperationalCarbon is EnergyToExecuteM * Carbon_Intensity * Transport_Factor.

%# Calcolo dell' intensità di carbonio embodied del microservizio M su un nodo N.
embodied_carbon_emissions(Node, Microservice, EmbodiedCarbon) :-
    node(Node, ToR, _, EL, TE, _),
    microservice(Microservice, RR, TiR, _),
    TS is TiR / EL,
    calculate_RS(ToR, RR, RS),
    EmbodiedCarbon is TE * TS * RS.
calculate_RS([SpecN|ToR], [SpecM|RR], TotalRS) :-
    calculate_RS(ToR, RR, Total),
    TotalRS is (SpecM / SpecN) + Total.
calculate_RS([],[],TotalRS) :-
    TotalRS is 0.

%# Calcolo del SCI.
calculate_SCI(Node, Microservice, SCI) :-
    totalR(Microservice, R),
    embodied_carbon_emissions(Node, Microservice, EmbodiedCarbon),
    operational_carbon_emissions(Node, Microservice, OperationalCarbon),
    Carbon is (EmbodiedCarbon + OperationalCarbon),
    SCI is Carbon / R.

%# Funzione principale che si occupa di restituire un array di nodi N_i in grado
%# di supportare il microservizio M, ordinati in ordine crescente di SCI.
sortNodesByLowestSCI(Microservice, SortedNodes) :-
    findall(Node, node(Node, _, _, _, _, _), Nodes),
    microservice(Microservice, RR, _, _),
    filterNodes(Nodes, RR, FilteredNodes),
    predsort(compareSCI(Microservice), FilteredNodes, SortedNodes).

%# Funzione di confronto del SCI, utilizzata nel sorting.
compareSCI(Microservice, Delta, Node1, Node2) :-
    calculate_SCI(Node1, Microservice, SCI1),
    calculate_SCI(Node2, Microservice, SCI2),
    (   SCI1 < SCI2 -> Delta = <
    ;   SCI1 > SCI2 -> Delta = >
    ;   Delta = =
    ).