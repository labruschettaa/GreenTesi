app(A,[Microservices], [PublicEndpoints/Interface]).
ep(EPId, [InvolvedMicroservices]).
prob(EPId, InvokeProb).
functionalUnits(R).

SCI = sum_{i \in EP} SCI_i * p_i


% emissions in CO2 kg/kWh
emissions(gas, 0.610).
emissions(coal, 1.1).
emissions(onshorewind, 0.0097).
emissions(offshorewind, 0.0165).
emissions(solar, 0.05). % https://solarbay.com.au/portfolio-item/how-much-emissions-does-solar-power-prevent/