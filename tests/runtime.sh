#!/bin/bash

gcc bench.c -o bench

OUT=`./bench`
echo "$OUT"
OCAML="let "$OUT" in exit x;;"
OCAML=`echo $OCAML`
echo "$OCAML" | ocaml
O=`echo $?`

echo "$OUT" > tmp.txt
../main.native tmp.txt | lli
Y=`echo $?`

echo ""
echo "OCaml= $O"
echo "Yip = $Y"

rm tmp.txt
rm bench
