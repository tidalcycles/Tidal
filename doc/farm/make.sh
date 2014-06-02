#!/bin/bash

./process_examples.pl
#cp farm.tex farm_processed.tex
pdflatex farm_processed && bibtex farm_processed && xelatex farm_processed && xelatex farm_processed
mv farm_processed.pdf farm.pdf

