
%# Valori di default n5 caso in cui E o 2000 non siano conosciuti.
default_E(0.05).
default_2000(2000).
default_5(5).

default_RPerYear(10000).
%# Assumiamo che siamo in Italia.
carbon_intensity(_, 0.389).

%# Dichiarazione dei nodi.
% node(nodeID, ToR, 0.05 (kW), 5 (Years), 2000 (kgCO2-eq), PUE).
% ToR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)].

node(good_entry_node, tor(4, 32, 1, 1), 0.05, 5, 2000, 1.2).

node(bad_entry_node, tor(4, 32, 1, 1), 0.05, 5, 2000, 2.2).

node(average_mid-range_node, tor(40, 256, 10, 10), 0.05, 5, 2000, 1.5).

node(good_mid-range_node, tor(40, 256, 10, 10), 0.05, 5, 2000, 1.2).

node(very_bad_high-range_node, tor(56, 3000, 25, 25), 0.05, 5, 2000, 3.0).

node(average_high-range_node, tor(56, 3000, 25, 25), 0.05, 5, 2000, 1.5).

%# Dichiarazione dei microservizi.
% microservice(microserviceID, RR, TiR (Years), R)
% RR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)]

microservice(frontEnd, rr(4, 32, 1, 1), 0.5).

microservice(backEnd, rr(30, 150, 5, 5), 2).

application(app, [frontEnd, backEnd]).
functionalUnits(10000).

% amazon ec2 instance-types.
% tiny, small, medium, large, extra-large.
% R su tutto il TiR.

% prossimo step sommo tutti i C di ogni miscroservizio e divido
% per un unico R.