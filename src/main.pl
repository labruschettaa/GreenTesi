% Load utility predicates.
:- ['utils.pl'].
:- discontiguous placement/5.

timedPlacement(Mode, App, P, SCI, N, Time) :-
    statistics(cputime, TStart),
    placement(Mode, App, P, SCI, N),
    statistics(cputime, TEnd),
    Time is TEnd - TStart.

%# Finds a valid placement for the application and returns the SCI and the number of nodes associated with the placement.
placement(quick0, App, P, SCI, NumberOfNodes) :- placement(quick, 0, App, P, SCI, NumberOfNodes).
placement(quick1, App, P, SCI, NumberOfNodes) :- placement(quick, 1, App, P, SCI, NumberOfNodes).
placement(quick2, App, P, SCI, NumberOfNodes) :- placement(quick, 2, App, P, SCI, NumberOfNodes).
placement(quick, NSort, App, P, SCI, NumberOfNodes) :-
    scoredNodes(Nodes, NSort),
    application(App, _, EPs),
    scoredMicroservices(Microservices),
    functionalUnits(App, R),
    eligiblePlacement(Microservices, Nodes, P), 
    involvedNodes(P, NumberOfNodes),
    sci(EPs, R, P, SCI).
placement(base, App, P, SCI, NumberOfNodes) :-
    application(App, Ms, EPs),
    functionalUnits(App, R),
    eligiblePlacement(Ms, P), 
    involvedNodes(P, NumberOfNodes),
    sci(EPs, R, P, SCI).
placement(tempBase, App, P, SCI, NumberOfNodes) :-
    application(App, _, EPs),
    functionalUnits(App, R),
    involvedNodes(P, NumberOfNodes),
    sci(EPs, R, P, SCI).
placement(oldopt, App, P, SCI, NumberOfNodes) :-
    placement(base, App, P, SCI, NumberOfNodes),
    \+ (placement(base, App, P1, S1, N1), dif(P1,P), (S1 < SCI ; (S1 =:= SCI, N1 < NumberOfNodes))).
placement(opt, App, BestP, BestSCI, BestNumberOfNodes) :-
    findall(p(SCI, N, P), placement(base, App, P, SCI, N), [P|Placements]),
    findMinP(Placements, P, p(BestSCI,BestNumberOfNodes,BestP)).

findMinP([p(SCI,N,P)|Placement], p(OldMinSCI,_,_), p(NewSCI,NewN,NewP)) :-
    SCI < OldMinSCI,
    findMinP(Placement, p(SCI,N,P), p(NewSCI, NewN, NewP)).
findMinP([p(SCI,_,_)|Placement], p(OldMinSCI, OldN, OldP), p(NewSCI, NewN, NewP)) :-
    SCI >= OldMinSCI,
    findMinP(Placement, p(OldMinSCI, OldN, OldP), p(NewSCI, NewN, NewP)).
findMinP([], P, P).

scoredNodes(Nodes, NSort) :-
    retractall(cs(_,_)), retractall(rs(_,_)),
    carbonRankingFactors(), resourceRankingFactors(node),
    findall(candidate(CS,RS,N), scores(N,CS,RS), TmpNodes), 
    sort(NSort,@=<,TmpNodes,SNodes),
    findall(N, member(candidate(_,_,N), SNodes), Nodes),
    cleanUp().

scoredMicroservices(Microservices) :-
    retractall(rs(_,_)), resourceRankingFactors(microservice),
    findall(ms(RS,M), scores(M,RS), TmpMs), 
    sort(0,@=<,TmpMs,SMs),
    findall(M, member(ms(_,M), SMs), Microservices),
    cleanUp().

resourceScore(E, CPU, RAM, BWIn, BWOut, RS) :-
    maxResources(MaxCPU,MaxRAM,MaxBWIn,MaxBWOut),
    minResources(MinCPU,MinRAM,MinBWIn,MinBWOut),
    safeROp(0.25, CPU, MaxCPU, MinCPU, P1),
    safeROp(0.25, RAM, MaxRAM, MinRAM, P2),
    safeROp(0.25, BWIn, MaxBWIn, MinBWIn, P3),
    safeROp(0.25, BWOut, MaxBWOut, MinBWOut, P4),
    RS is P1 + P2 + P3 + P4, 
    assert(rs(E,RS)).

carbonScore(N,CS) :- 
    node(N,_,_,_,_,_),
    of(N,OF), minOF(MinOF), maxOF(MaxOF),
    safeCOp(0.5, OF, MaxOF, MinOF, P1),
    mf(N,MF), minMF(MinMF), maxMF(MaxMF),
    safeCOp(0.5, MF, MaxMF, MinMF, P2),
    CS is P1 + P2, assert(cs(N,CS)).

carbonRankingFactors() :-
    findall(OF, nodeOF(N,OF), OFs), max_list(OFs, MaxOF), min_list(OFs,MinOF),
    assert(maxOF(MaxOF)), assert(minOF(MinOF)),
    findall(MF, nodeMF(N,MF), MFs), max_list(MFs,MaxMF), min_list(MFs,MinMF),
    assert(maxMF(MaxMF)), assert(minMF(MinMF)).

nodeOF(N,OF) :- node(N,_,PowerPerCPU,_,_,PUE), carbon_intensity(N,I), OF is PUE * I * PowerPerCPU, assert(of(N,OF)).

nodeMF(N,MF) :- node(N,_,_,EL,TE,_), MF is TE/EL, assert(mf(N,MF)).

scores(Ms,RS) :- resourceScore(microservice,Ms,RS).
scores(N,CS,RS) :- carbonScore(N,CS), resourceScore(node,N,RS).

resourceScore(microservice,M,RS) :- microservice(M,rr(CPU, RAM, BWIn, BWOut),_), resourceScore(M, CPU, RAM, BWIn, BWOut, RS).
resourceScore(node,N,RS) :- node(N,tor(CPU, RAM, BWIn, BWOut),_,_,_,_), resourceScore(N, CPU, RAM, BWIn, BWOut, RS).

eligiblePlacement(Ms, Nodes, P) :- eligiblePlacement(Ms, Nodes, [], P).
eligiblePlacement([M|Ms], Nodes, P, NewP) :-
    microservice(M, RR, _),
    member(N,Nodes), placementNode(N, P, RR),
    eligiblePlacement(Ms, Nodes, [on(M,N)|P], NewP).
eligiblePlacement([], _, P, P).
eligiblePlacement(Ms, P) :- eligible(Ms, [], P).

eligible([M|Ms], P, NewP) :-
    microservice(M, RR, _),
    placementNode(N, P, RR),
    eligible(Ms, [on(M,N)|P], NewP).
eligible([], P, P).

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

%# Calculates the SCI of the application's placement.
sci(EndPoints, R, P, SCI) :- sci(EndPoints,R,P,0,SCI).
sci([EP|EPs], R, P, OldSCI, NewSCI) :-
    endpointSCI(EP,R,P,EPSCI),
    TmpSCI is OldSCI + EPSCI,
    sci(EPs,R,P,TmpSCI,NewSCI).
sci([],_,_,SCI,SCI).

%# Calculates the SCI relative to a single endpoint considering the probability
%# that said endpoint is called.
endpointSCI(EP, R, P, SCI) :-
    endpoint(EP, EPMs),
    findall(on(M,N), (member(M, EPMs), member(on(M, N), P)), FilteredP),
    probability(EP, Prob),
    carbonEmissions(FilteredP, C),
    SCI is (C / R) * Prob.

%# Calculates the carbon amount of a placement.
carbonEmissions([on(Microservice,Node)|P], C) :-
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
    carbon_intensity(Node, I),
    operationalEnergy(Node, Microservice, E),
    O is E * I.

%# Calculates the embodied carbon intensity of microservice M on node N.
embodiedCarbon(Node, Microservice, M) :-
    node(Node, tor(CPU,_,_,_), _, EL, TE, _),
    microservice(Microservice, rr(CPUReq,_,_,_), TiR),
    TS is TiR / EL,
    RS is CPUReq / CPU,
    M is TE * TS * RS.