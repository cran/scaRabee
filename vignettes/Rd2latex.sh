#!/bin/bash

for ii in ../man/*.Rd
  do
    name=$(basename $ii .Rd)
    R CMD Rdconv -t 'latex' $ii > $name.tex
  done
