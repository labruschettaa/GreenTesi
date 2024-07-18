
%# Valori di default nel caso in cui E o TE non siano conosciuti.
default_E(0.2).
default_TE(2000).
default_EL(5).

default_RPerYear(10000).
%# Assumiamo che siamo in Italia.
carbon_intensity(_, 0.389).
transport(_, 0.95).

%# Dichiarazione dei nodi.
% node(nodeID, ToR, E (kW), EL (Years), TE (kgCO2-eq), PUE).
% ToR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)].

node(good_entry_node, [4, 32, 1, 1], EnergyPerCPU, EL, TE, 1.2) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

node(bad_entry_node, [4, 32, 1, 1], EnergyPerCPU, EL, TE, 2.2) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

node(average_mid-range_node, [40, 256, 10, 10], EnergyPerCPU, EL, TE, 1.5) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

node(good_mid-range_node, [40, 256, 10, 10], EnergyPerCPU, EL, TE, 1.2) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

node(very_bad_high-range_node, [56, 3000, 25, 25], EnergyPerCPU, EL, TE, 3.0) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

node(average_high-range_node, [56, 3000, 25, 25], EnergyPerCPU, EL, TE, 1.5) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

%# Dichiarazione dei microservizi.
% microservice(microserviceID, RR, TiR (Years), R)
% RR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)]

microservice(entry_microservice, [4, 32, 1, 1], 0.5, RPerYear) :-
    default_RPerYear(RPerYear).

microservice(mid-range_microservice, [30, 150, 5, 5], 2, RPerYear) :-
    default_RPerYear(RPerYear).