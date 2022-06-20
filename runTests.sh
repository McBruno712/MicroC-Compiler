#!/bin/bash
printf "En los archivos .myopt y .mymc se encuentran las salidas de las flags -o y -m, respectivamente.\n"
printf "En los archivos .diff se encuentra la diferencia entre los .opt y los .myopt.\n"
printf "Si se cuelga interrumpir con Ctrl+C y verificar manualmente el caso que provoca el cuelgue."
rm -rf diff
mkdir diff
for i in {1..9}
do
   touch casos/caso0$i.myopt
   touch casos/caso0$i.mymc
   ./MicroC -o casos/caso0$i.mc < casos/caso0$i.in > casos/caso0$i.myopt
   ./MicroC -m casos/caso0$i.mc < casos/caso0$i.in > casos/caso0$i.mymc
   touch diff/caso0$i.diff
   diff casos/caso0$i.opt casos/caso0$i.myopt > diff/caso0$i.diff
   if ! [ -s diff/caso0$i.diff ]; then
      #Si el .diff esta vacio lo borra
      rm diff/caso0$i.diff
   fi
   printf "\n0$i ejecutado con exito\n"
done
for i in {10..39}
do
   touch casos/caso$i.myopt
   touch casos/caso$i.mymc
   ./MicroC -o casos/caso$i.mc < casos/caso$i.in > casos/caso$i.myopt
   ./MicroC -m casos/caso$i.mc < casos/caso$i.in > casos/caso$i.mymc
   touch diff/caso$i.diff
   diff casos/caso$i.opt casos/caso$i.myopt > diff/caso$i.diff
   if ! [ -s diff/caso$i.diff ]; then
      #Si el .diff esta vacio lo borra
      rm diff/caso$i.diff
   fi
   printf "\n$i ejecutado con exito\n"
done
printf "Se ejecutaron todos los casos de prueba.\n"
