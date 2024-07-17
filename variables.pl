
%# Valori di default nel caso in cui E o TE non siano conosciuti.
default_E(200).
default_TE(20000).
default_EL(5).

%# Assumiamo che siamo in Italia.
carbon_intensity(_, 0.389).
transport(_, 0.95).

%# Dichiarazione dei nodi.
% node(ToR, E (W), EL, TE, PUE).
% ToR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)].

node(entry_node, [4, 32, 1, 1], EnergyPerCPU, EL, TE, 1.5) :-
    default_E(EnergyPerCPU),
    default_EL(EL),
    default_TE(TE).

%# Dichiarazione dei microservizi.
microservice(entry_microservice, [4,32,1,1], 2).