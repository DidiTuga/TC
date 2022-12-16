#!/bin/bash

# variables
max_i=0
score_i=0
start=`date +%s000`
# This script is used to run the problem A of TC
# print in the terminal date and time of the start of the execution
echo "------------$(date)--------------------"
date > out.txt
echo "------------------------------------------------------------" >> out.txt
echo "TC - Problema A"
# Compile
ocamlopt probB.ml
for f in tests/*.txt; do
    # somar um ao max_i
    max_i=$((max_i+1))
    # expected output - remove the .txt extension and add .out
    expected=tests/`basename $f .txt`.out
    rm -f out

    if (./a.out < $f) > out; then
        echo "Teste $max_i: $f:" >> out.txt
        ./a.out < $f >> out.txt
	    if cmp out $expected; then
        end=`date +%s000`
        elapsed=$(($end-$start))
        echo "$f - Time elapsed: $elapsed"
	    score_i=`expr $score_i + 1`;
	    else
	    echo "  FALHA : saida errada para $f"
	    fi
    else
	echo "  FALHA da interpretação para $f"
    fi
done

# remove the file out and others to help compiling
rm -f out
rm *.o *.cmx *.cmi
echo "------------------------------------------------------------" >> out.txt

echo 
# if the result is the expected is 100%
percent=`expr 100 \* $score_i / $max_i`;
echo "Score: $score_i / $max_i testes =  $percent%"
echo "File out.txt:"
cat out.txt