
---
### Esempi di configurazioni di CPU in server:

- **Intel Xeon E3-1230 v6 (Entry-level)**: Core -> 4.
- **AMD EPYC 7302 (Mid-range)**: Core -> 16.
- **Intel Xeon Gold 6230R (High-end)** -> Core -> 26.
- **AMD EPYC 7742 (High-performance)**: Core -> 64.
---

### Esempi di configurazioni di RAM per server specifici:

- **Dell PowerEdge T40 (Entry-level)**: 8 GB - 32 GB DDR4.
- **Dell PowerEdge R540 (Mid-range)**: 32 GB - 256 GB DDR4.
- **Server di fascia alta: Dell PowerEdge R740 (High-end)**: 128 GB - 3 TB DDR4.
- **Dell PowerEdge R940 (High-performance)**: 512 GB - 12 TB DDR4.
---

### Esempi di configurazioni di banda per server specifici:
- **Dell PowerEdge T40 (Entry-level)**: Up to 1 Gbps - 1 Gbps.
- **Dell PowerEdge R540 (Mid-range)**: Up to 10 Gbps - 10 Gbps.
- **Dell PowerEdge R740 (High-end)**: Up to 25 Gbps - 25 Gbps.
- **Dell PowerEdge R940 (High-performance)**: Up to 100 Gbps - 100 Gbps.
---
<br />
<br />

# Dubbi

Durante la scrittura del codice mi sono sorti i seguenti dubbi per quanto riguarda il calcolo del SCI.
- **Misura di tempo**: Utilizzare i mesi risulterebbe problematico, gli anni forse troppo vagi. 

- **Quali sono i parametri che dobbiamo considerare come "obbligatori"**: Alcuni parametri come l'embodied carbon non sono scontati, non è detto che siano stati dichiarati dai produttori, per questo  motivo si può quindi valutare di fornire una stima. <br />Mentre, un valore come il PUE è un valore da fornire e di cui ha poco senso fare una stima.

