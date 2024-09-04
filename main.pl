%# Include di file ausiliari.
:- use_module(library(apply)).
:- consult('resources/variables.pl').


%# Finds the placement with the lowest SCI. In case of a tie in SCI, returns the placement that uses the fewest nodes.
minPlacement(App, P, SCI, NumberOfNodes) :-
    placement(App, P, SCI, NumberOfNodes),
    \+ (placement(App, P1, S1, N1), dif(P1,P),  (S1 < SCI ; (S1 =:= SCI, N1 < NumberOfNodes))),
    !.


%# Finds a valid placement for the application and returns the SCI and the number of nodes associated with the placement.
placement(App, P, SCI, NumberOfNodes) :-
    application(App, Ms, R),
    eligiblePlacement(Ms, [], P), 
    involvedNodes(P, NumberOfNodes),
    (sci(App, R, P, SCI) -> true ; !, fail).


%# Finds a valid placement for the list of microservices.
eligiblePlacement([M|Ms], P, NewP) :-
    microservice(M, RR, _),
    placementNode(N, P, RR),
    eligiblePlacement(Ms, [on(M,N)|P], NewP).
eligiblePlacement([], P, P).


%# Checks if the node N can host the microservice M.
placementNode(N, P, rr(CPUReq, RAMReq, BWinReq, BWoutReq)) :-
    node(N, tor(CPU, RAM, BWin, BWout), _, _, _, _),
    hardwareUsedAtNode(N, P, rr(UCPU, URAM, UBWin, UBWout)),
    CPU >= UCPU + CPUReq, 
    RAM >= URAM + RAMReq, 
    BWin >= UBWin + BWinReq, 
    BWout >= UBWout + BWoutReq.


%# Counts the number of nodes used by the placement.
involvedNodes(P, InvolvedNodes) :-
    findall(N, distinct(node(N,_), member(on(_,N),P)), Nodes),
    length(Nodes, InvolvedNodes).


%# Calculates the amount of hardware used on node N.
hardwareUsedAtNode(N, P, rr(UCPU, URAM, UBWin, UBWout)) :-
    findall(rr(CPU,RAM,BWin,BWout), (member(on(M,N),P), microservice(M,rr(CPU,RAM,BWin,BWout),_)), RRs),
    sumHWReqs(RRs, rr(UCPU, URAM, UBWin, UBWout)).


%# Sums the hardware requirements of each microservice M placed on node N.
sumHWReqs([rr(CPU,RAM,BWin,BWout) | RRs], rr(TCPU, TRAM, TBWin, TBWout)) :-
    sumHWReqs(RRs, rr(AccCPU, AccRAM, AccBWin, AccBWout)),
    TCPU is AccCPU + CPU,
    TRAM is AccRAM + RAM,
    TBWin is AccBWin + BWin,
    TBWout is AccBWout + BWout.
sumHWReqs([], rr(0,0,0,0)).


%# Finds all the endpoints relative to an application.
getAllEndpoints(App, EPs) :-
    findall(Endpoints, interface(App, Endpoints), NestedEPs),
    flatten(NestedEPs, EPs).
     

%# Calculates the SCI relative to a single endpoint considering the probability
%# that said endpoint is called.
sciEP(EP, R, P, SCI) :-
    endpoint(EP, EPMs),
    findall(on(M,N), (member(M, EPMs), member(on(M, N), P)), FilteredP),
    probability(EP, Prob),
    carbonEmissions(FilteredP, C),
    SCI is (C / R) * Prob.


%# Calculates the SCI of the application's placement.
sci(App, R, P, SCI) :-
    getAllEndpoints(App, EPs),
    calculateEPsSCI(EPs, R, P, SCI).


%# Calculates the SCI of all the application's endpoints.
calculateEPsSCI([EP | EPs], R, P, SCI) :-
    calculateEPsSCI(EPs, R, P, AccSCI),
    sciEP(EP, R, P, EPSCI),
    SCI is EPSCI + AccSCI.
calculateEPsSCI([], _, _, 0).


%# Calculates the carbon amount of a placement.
carbonEmissions([on(Microservice, Node) | P], C) :-
    carbonEmissions(P, AccC),
    operationalCarbon(Node, Microservice, O),
    embodiedCarbon(Node, Microservice, E),
    C is AccC + O + E.
carbonEmissions([], 0).


%# Calculates the amount of energy required to run microservice M on node N for the entire TiL.
operationalEnergy(Node, Microservice, E) :-
    node(Node, _, PowerPerCPU, _, _, PUE),
    microservice(Microservice, _, TiR),
    E is PUE * (TiR * 365 * 24) * PowerPerCPU.


%# Calculates the carbon intensity of running microservice M on node N.
operationalCarbon(Node, Microservice, O) :-
    carbon_intensity(_, I),
    operationalEnergy(Node, Microservice, E),
    O is E * I.


%# Calculates the embodied carbon intensity of microservice M on node N.
embodiedCarbon(Node, Microservice, M) :-
    node(Node, tor(CPU,_,_,_), _, EL, TE, _),
    microservice(Microservice, rr(CPUReq,_,_,_), TiR),
    TS is TiR / EL,
    RS is CPUReq / CPU,
    M is TE * TS * RS.

