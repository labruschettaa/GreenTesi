%# Include di file ausiliari.
:- consult('variables.pl').
:- consult('auxfuncs.pl').
:- use_module(library(apply)).

%# Trova il piazzamento con lo SCI più basso. In parità di SCI restituisce il piazzamento che utilizza
%# meno nodi.
minPacking(App, Packing, SCI, NumberOfNodes) :-
    packing(App, Packing, SCI, NumberOfNodes), 
    \+ (packing(App, P1, S1, N1), dif(P1,Packing),  (S1 < SCI ; (S1 =:= SCI, N1 < NumberOfNodes))).

%# Trova un piazzamento valido per l'applicazione e restituisce lo SCI e i numeri di nodi associati
%# al piazzamento
packing(App, Packing, SCI, NumberOfNodes) :-
    application(App, Ms, R),
    eligiblePacking(Ms, [], Packing), 
    involvedNodes(Packing, NumberOfNodes),
    sci(R, Packing, SCI).

%# Trova un piazzamento valido per la lista di microservizi.
eligiblePacking([M|Ms], P, NewP) :-
    microservice(M, rr(CPUReq, RAMReq, BWinReq, BWoutReq), _),
    node(N, tor(CPU, RAM, BWin, BWout), _, _, _, _),
    hardwareUsedAtNode(N, P, rr(UCPU, URAM, UBWin, UBWout)),
    CPU >= UCPU + CPUReq, 
    RAM >= URAM + RAMReq, 
    BWin >= UBWin + BWinReq, 
    BWout >= UBWout + BWoutReq,
    eligiblePacking(Ms, [on(M,N)|P], NewP).
eligiblePacking([], P, P).

%# Conta la quantità di nodi utilizzati dal piazzamento.
involvedNodes(Packing, InvolvedNodes) :-
    findall(N, distinct(node(N,_), member(on(_,N),Packing)), Nodes),
    length(Nodes, InvolvedNodes).

%# Calcola la quantità di HW usato sul nodo N.
hardwareUsedAtNode(N, P, rr(UCPU, URAM, UBWin, UBWout)) :-
    findall(rr(CPU,RAM,BWin,BWout), (member(on(M,N),P), microservice(M,rr(CPU,RAM,BWin,BWout),_)), RRs),
    sumHWReqs(RRs, rr(UCPU, URAM, UBWin, UBWout)).

%# Somma gli HW richiesti di ogni microservizio M piazzati sul nodo N.
sumHWReqs([rr(CPU,RAM,BWin,BWout) | RRs], rr(TCPU, TRAM, TBWin, TBWout)) :-
    sumHWReqs(RRs, rr(AccCPU, AccRAM, AccBWin, AccBWout)),
    TCPU is AccCPU + CPU,
    TRAM is AccRAM + RAM,
    TBWin is AccBWin + BWin,
    TBWout is AccBWout + BWout.
sumHWReqs([], rr(0,0,0,0)).

%# Calcola lo SCI di un piazzamento.
sci(R, Packing, SCI) :-
    calculateCarbon(Packing, C),
    SCI is C / R.

%# Calcola la quantità di carbonio di un piazzamento.
calculateCarbon([on(Microservice, Node) | P], C) :-
    calculateCarbon(P, AccC),
    operationalCarbon(Node, Microservice, O),
    embodiedCarbon(Node, Microservice, E),
    C is AccC + O + E.
calculateCarbon([], 0).

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

