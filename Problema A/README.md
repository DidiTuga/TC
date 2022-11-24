# ProblemaA_TC 2022/2023

Para correr os inputs, basta fazer:
```
./run.sh
```
Verificando se os inputs estão certos e dando output dos mesmos.

## Problema

Considere um autómato A, eventualmente com transições є. Mais formalmente, sejam Σ um alfabeto, S um conjunto de estados, S0 e F dois subconjuntos de S (respectivamente conjunto de estados iniciais e conjunto de estados finais) e Rδ uma relação de transição sobre Σ e S. Temos assim A={Σ∪ {є}, S, S0, F, Rδ}.

Considere igualmente uma palavra t de Σ∗.

Escreva um programa que leia A e t e que determine se A é determinista ou não e se reconhece t.

No caso em que tiver que optar entre 2 estados para continuar a execução, deverá sempre optar pelo estado de índice mais baixo.
## Input

Para simplificar o formatos dos dados em entrada admitiremos aqui que o conjunto S é sempre da forma {1..n} (n inteiro), Σ é o alfabeto português e є o caracter _. Assim A={Σ∪ {є}, S, S0, F, Rδ} e a palavra t podem ser introduzidos por:

* uma linha com o inteiro n, especificando o conjunto S={1..n};
* uma linha com o numero s0 (cardinalidade do conjunto S0 dos estados iniciais);
* uma linha com s0 inteiros distinctos que formam o conjunto dos estados iniciais;
* uma linha com o numero f (cardinalidade do conjunto F dos estados finais);
* uma linha com f inteiros distinctos que formam o conjunto dos estados finais;
* uma linha com o número m de transições (a cardinalidade de Rδ));
* m linhas em que cada uma delas introduz uma transição sob a forma de i c j, i sendo o inteiro representando o estado de partida da transição, c o caracter no rótulo da transição (c∈Σ ∪ {є}, isto é, c pode ser _) e j o inteiro que representa o estado de chegada;
* finalmente a última linha contém uma string representando a palavra t por reconhecer.

## Output
O Output é organizado em duas ou três linhas:
Na pimeira linha:
* a palavra "NDFA" se o autómato seja não determinsita
* a palavra "DFA" no outro caso.
Na segunda linha:
* a palavra YES se o automato reconhece a palavra;
* a palavra NO caso contrário.
Na terceira linha:
* O caminho percorrido desde o estado inicial até ao estado final em caso de a palavra t ser reconhecida pelo autómato.
* Nada caso a palavra não seja aceite.
