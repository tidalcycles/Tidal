all: tutorial.pdf

tutorial.pdf: tutorial.md process_examples.pl
	./process_examples.pl
	pandoc -H preamble.tex --latex-engine=xelatex --number-sections tutorial_processed.md -o tutorial.pdf

