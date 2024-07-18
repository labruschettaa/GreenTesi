
# Esempi di configurazioni considerate.
### **Configurazioni di CPU in server specifici**:

- **Dell PowerEdge T40 (Entry-level)**: Up to 4 cores.
- **Dell PowerEdge R540 (Mid-range)**: Up to 40 cores.
- **Dell PowerEdge R740 (High-end)**: Up to 56 cores.
- **Dell PowerEdge R940 (High-performance)**: Up to 112 cores.
---

### **Configurazioni di RAM per server specifici**:

- **Dell PowerEdge T40 (Entry-level)**: 8 GB - 32 GB DDR4.
- **Dell PowerEdge R540 (Mid-range)**: 32 GB - 256 GB DDR4.
- **Dell PowerEdge R740 (High-end)**: 128 GB - 3 TB DDR4.
- **Dell PowerEdge R940 (High-performance)**: 512 GB - 12 TB DDR4.
---

### **Configurazioni di banda per server specifici**:
- **Dell PowerEdge T40 (Entry-level)**: Up to 1 Gbps - 1 Gbps.
- **Dell PowerEdge R540 (Mid-range)**: Up to 10 Gbps - 10 Gbps.
- **Dell PowerEdge R740 (High-end)**: Up to 25 Gbps - 25 Gbps.
- **Dell PowerEdge R940 (High-performance)**: Up to 100 Gbps - 100 Gbps.
---
<br />

# Dubbi

Durante la scrittura del codice mi sono sorti i seguenti dubbi per quanto riguarda il calcolo del SCI.
- **Misura di tempo**: Utilizzare i mesi risulterebbe problematico, gli anni forse troppo vagi. 

- **Quali sono i parametri che dobbiamo considerare come "obbligatori"**: Alcuni parametri come l'embodied carbon non sono scontati, non è detto che siano stati dichiarati dai produttori, per questo  motivo si può quindi valutare di fornire una stima. <br />Mentre, un valore come il PUE è un valore da fornire e di cui ha poco senso fare una stima.

- **Come calcolo il RS**: Il calcolo del `RS` è stato descitto come `RS = RR/ToR` ovvero "il numero di risorse riservate per l'uso dal software" diviso "il numero di risorse totali disponibili", il dubbio è come questo si trasformi in una formula matematica dato che ToR e RR non sono direttamente numeri.

## Test interessanti:

Query: `?- sortNodesByLowestSCI(entry_microservice, SortedNodes).`   
Risultato: `SortedNodes = [good_mid-range_node, average_high-range_node, average_mid-range_node, very_bad_high-range_node, good_entry_node, bad_entry_node].`

Query:
`?- sortNodesByLowestSCI(mid-range_microservice, SortedNodes). `
<br />
Risultato:
`SortedNodes = [average_high-range_node, good_mid-range_node, average_mid-range_node, very_bad_high-range_node].`

_Nota_: è interessante notare che per il microservizio di tipo entry good_mid-range_node abbia uno SCI più basso rispetto a average_high-range_node, cosa non vera però per il microservizio mid-range_microservice dove avviene la situazione opposta.
