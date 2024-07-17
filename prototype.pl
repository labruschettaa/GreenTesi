
%# Calcolo della quantità di energia necessaria per eseguire il micrservizio M
%# sul nodo N per tutta la TiL.
energy_to_execute_m(Node, Microservice, EnergyToExecuteM) :-
    node(Node, _, _, _, _, PUE),
    energy(Node, Energy),
    microservice(Microservice, _, TiL),
    EnergyToExecuteM is PUE * (TiL * 365 * 24) * Energy.

%# Calcolo dell' intensità di carbonio dell'esecuzione del microservizio M su un nodo N.
operational_carbon_emissions(Node, Microservice, OperationalCarbon) :-
    carbon_intensity(_, Carbon_Intensity),
    transport(_, Transport_Factor),
    energy_to_execute_m(Node, Microservice, EnergyToExecuteM),
    OperationalCarbon is EnergyToExecuteM * Carbon_Intensity * Transport_Factor.

%# Calcolo dell' intensità di carbonio embodied del microservizio M su un nodo N.
embodied_carbon_emissions(Node, Microservice, EmbodiedCarbon) :-


%# Controlla che il microservizio M possa essere supportato dal nodo N.
elegibleNode(Node, Microservice) :-
    node(Node, ListN, _, _, _, _),
    microservice(Microservice, ListM),
    compareREQ(ListN, ListM).
compareREQ([Rn|Rqsn], [Rm|Rqsm]) :-
    Rn >= Rm,
    compareREQ(Rqsn, Rqsm).
compareREQ([],[]).

%# FAI CALCOLO ENERGIA
%# TRASFORMA ANNI IN MESI





