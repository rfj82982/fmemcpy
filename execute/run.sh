#!/bin/bash
nx=( 512 1024 512 256)
ny=( 512 256 1024 512)
nz=( 512 512 256 1024)
nt= 100
for fld in $( ls -d ../build*); do
  echo $fld
  for index in ${!nx[@]}; do
    n1="${nx[index]}"
    n2="${ny[index]}"
    n3="${nz[index]}"
    ${fld}/bin/evaluate_fmemcpy $n1 $n2 $n3 $nt
  done 
done
