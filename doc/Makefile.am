
dist_doc_DATA=ikarus-scheme-users-guide.pdf
EXTRA_DIST = ikarus-scheme-users-guide.tex ikarus-scheme-users-guide.bib ikarus-scheme-users-guide.pdf

TEX=xelatex -output-driver="xdvipdfmx -q -E" 

ikarus-scheme-users-guide.pdf: ikarus-scheme-users-guide.tex ikarus-scheme-users-guide.bib
	if test -x "`which xelatex`" ; \
	then \
	  $(TEX) ikarus-scheme-users-guide && \
	  makeindex ikarus-scheme-users-guide && \
	  bibtex ikarus-scheme-users-guide && \
	  $(TEX) ikarus-scheme-users-guide && \
	  $(TEX) ikarus-scheme-users-guide ; \
	fi 

all:

docs: ikarus-scheme-users-guide.pdf

fast:
	$(TEX) ikarus-scheme-users-guide

clean: 
	rm -f *.aux *.log *.toc *.out *.idx *.ind *.ilg *.blg *.bbl
