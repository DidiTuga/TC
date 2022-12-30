
# Problema B - Teoria da Computação 2022/2023


## Algoritmo CYK (de Cocke, Younger e Kasami)

## Input
Na primeira linha é introduzida a palavra por reconhecer. Esta palavra só é constituída por caracteres do alfabeto Σ={a,…, z} e tem por comprimento máximo 50 caracteres. Assume-se que o conjunto de não-terminais é N={A,B, … S , …, Z} em que se destaca o símbolo S que assumiremos como sendo sempre o símbolo inicial.

A segunda linha apresenta o número m de regras de produções da gramática considerada. As restantes m linhas introduzem as m regras de produção. Cada uma destas produções tem o formato seguinte N -> a1 a2 a3 … an com N não terminal e ai ∈ (N ∪ Σ) e cada ai é separado do aj seguinte por um espaço único.

## Output
O output é constituído por uma linha contendo:

A palavra "YES" se a palavra é gerada pela gramática.
A palavra "‘NO" se a palavra não é gerada (reconhecida) pela gramática
Seguida da matriz triangular inferior, usada para reconhecer (ou não) a palavra.
```
NOTAS:

cada célula da matriz é separada da célula adjacente à direita por 2 tabs (\t\t).
Sempre que uma célula contém mais do que um não terminal, estes são separados por um só espaço, e estes estão ordenados por ordem alfabética.
```
## Sample Input 1
```
abbabba
7
S -> S F
S -> a
A -> C C
A -> S S
A -> C S
C -> b
F -> A S
```
## Sample Output 1
```
YES
S               
  F             
    A           
S       S         
  F       F       
  A   A     A   A     
S   C   C   S   C   C   S   
a   b   b   a   b   b   a   
```
