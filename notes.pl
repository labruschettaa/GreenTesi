app(A,[Microservices], [PublicEndpoints/Interface]).
ep(EPId, [InvolvedMicroservices]).
prob(EPId, InvokeProb).
functionalUnits(R).

SCI = sum_{i \in EP} SCI_i * p_i