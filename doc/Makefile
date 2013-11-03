CONFIG=
HTML_CONFIG=$(CONFIG) --standalone
PDF_CONFIG=$(CONFIG) --latex-engine=xelatex --include-in-header=preamble.tex --listings --variable mainfont="Linux Libertine O" --variable sansfont="Linux Biolinum O" --variable geometry="a4paper"

all: tidal.pdf tidal.html

tidal.pdf: tidal.md
	pandoc $(PDF_CONFIG) tidal.md -o tidal.pdf

tidal.html: tidal.md
	pandoc $(HTML_CONFIG) tidal.md -o tidal.html
