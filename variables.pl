%# Assumiamo che siamo in Italia.
carbon_intensity(_, 0.389).

%# Dichiarazione dei nodi.
% node(nodeID, ToR, 0.05 (kW), 5 (Years), 2000 (kgCO2-eq), PUE).
% ToR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)].

node(t412, tor(4, 8, 1, 1), 0.05, 5, 2000, 1.2).
node(t422, tor(4, 8, 1, 1), 0.05, 5, 2000, 2.2).
node(r515, tor(40, 32, 10, 10), 0.05, 5, 2000, 1.5).
node(r512, tor(40, 32, 10, 10), 0.05, 5, 2000, 1.2).
node(r730, tor(56, 128, 25, 25), 0.05, 5, 2000, 3.0).
node(r715, tor(56, 128, 25, 25), 0.05, 5, 2000, 1.5).

%# Dichiarazione dei microservizi.
% microservice(microserviceID, RR, TiR (Years))
% RR = [CPU (num), RAM (GB), BWin (Gbps), BWout (Gbps)]

microservice(frontEnd, rr(4, 8, 1, 1), 0.5).
microservice(backEnd, rr(10, 32, 10, 10), 2).

%# Dichiarazione dell'app.
% application(appID, microservices, R)
application(app1, [frontEnd], 1000).
application(app, [frontEnd, backEnd], 1000).

