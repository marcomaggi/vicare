
EXTRA_DIST=README bench.ss benchall.ss rn100 parsing-data.ss \
  summarize.pl rnrs-benchmarks.ss bib \
  rnrs-benchmarks/slatex-data/test.tex \
  rnrs-benchmarks/slatex-data/slatex.sty \
  rnrs-benchmarks/ack.ss \
  rnrs-benchmarks/array1.ss \
  rnrs-benchmarks/bibfreq.ss \
  rnrs-benchmarks/boyer.ss \
  rnrs-benchmarks/browse.ss \
  rnrs-benchmarks/cat.ss \
  rnrs-benchmarks/compiler.ss \
  rnrs-benchmarks/conform.ss \
  rnrs-benchmarks/cpstak.ss \
  rnrs-benchmarks/ctak.ss \
  rnrs-benchmarks/dderiv.ss \
  rnrs-benchmarks/deriv.ss \
  rnrs-benchmarks/destruc.ss \
  rnrs-benchmarks/diviter.ss \
  rnrs-benchmarks/divrec.ss \
  rnrs-benchmarks/dynamic.src.ss \
  rnrs-benchmarks/dynamic.ss \
  rnrs-benchmarks/earley.ss \
  rnrs-benchmarks/fft.ss \
  rnrs-benchmarks/fib.ss \
  rnrs-benchmarks/fibc.ss \
  rnrs-benchmarks/fibfp.ss \
  rnrs-benchmarks/fpsum.ss \
  rnrs-benchmarks/gcbench.ss \
  rnrs-benchmarks/gcold.ss \
  rnrs-benchmarks/graphs.ss \
  rnrs-benchmarks/lattice.ss \
  rnrs-benchmarks/matrix.ss \
  rnrs-benchmarks/maze.ss \
  rnrs-benchmarks/mazefun.ss \
  rnrs-benchmarks/mbrot.ss \
  rnrs-benchmarks/nbody.ss \
  rnrs-benchmarks/nboyer.ss \
  rnrs-benchmarks/nqueens.ss \
  rnrs-benchmarks/ntakl.ss \
  rnrs-benchmarks/nucleic.ss \
  rnrs-benchmarks/paraffins.ss \
  rnrs-benchmarks/parsing.ss \
  rnrs-benchmarks/perm9.ss \
  rnrs-benchmarks/peval.ss \
  rnrs-benchmarks/pi.ss \
  rnrs-benchmarks/pnpoly.ss \
  rnrs-benchmarks/primes.ss \
  rnrs-benchmarks/puzzle.ss \
  rnrs-benchmarks/quicksort.ss \
  rnrs-benchmarks/ray.ss \
  rnrs-benchmarks/sboyer.ss \
  rnrs-benchmarks/scheme.ss \
  rnrs-benchmarks/simplex.ss \
  rnrs-benchmarks/slatex.ss \
  rnrs-benchmarks/string.ss \
  rnrs-benchmarks/sum.ss \
  rnrs-benchmarks/sum1.ss \
  rnrs-benchmarks/sumfp.ss \
  rnrs-benchmarks/sumloop.ss \
  rnrs-benchmarks/tail.ss \
  rnrs-benchmarks/tak.ss \
  rnrs-benchmarks/takl.ss \
  rnrs-benchmarks/trav1.ss \
  rnrs-benchmarks/trav2.ss \
  rnrs-benchmarks/triangl.ss \
  rnrs-benchmarks/wc.ss 

benchall: 
	date +"NOW: %Y-%m-%d %H:%M:%S" >>timelog
	../src/ikarus -b ../scheme/ikarus.boot --r6rs-script benchall.ss 2>>timelog
	rm -f z*.scm z*.tex

clean:
	rm -f z*.scm z*.tex
