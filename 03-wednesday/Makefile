01-basic-ordination.pdf: 01-basic-ordination.Rmd
	Rscript -e "rmarkdown::render(\"01-basic-ordination.Rmd\")"

02-constrained-ordination.pdf: 02-constrained-ordination.Rmd
	Rscript -e "rmarkdown::render(\"02-constrained-ordination.Rmd\")"

purl: 01-basic-ordination.Rmd  02-constrained-ordination.Rmd
	Rscript -e "knitr::purl(\"01-basic-ordination.Rmd\")"
	Rscript -e "knitr::purl(\"02-constrained-ordination.Rmd\")"

handout-01-basic-ordination.pdf: 01-basic-ordination.pdf
	pdfnup 01-basic-ordination.pdf --frame true --outfile handout-01-basic-ordination.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95

handout-02-constrained-ordination.pdf: 02-constrained-ordination.pdf
	pdfnup 02-constrained-ordination.pdf --frame true --outfile handout-02-constrained-ordination.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95

03-basic-ordination-prac.pdf: 03-basic-ordination-prac.Rnw
	R CMD Sweave 03-basic-ordination-prac.Rnw
	R CMD texi2pdf --build=clean 03-basic-ordination-prac.tex
	rm 03-basic-ordination-prac-*.pdf

04-constrained-ordination-prac.pdf: 04-constrained-ordination-prac.Rnw
	R CMD Sweave 04-constrained-ordination-prac.Rnw
	R CMD texi2pdf --build=clean 04-constrained-ordination-prac.tex
	rm 04-constrained-ordination-prac-*.pdf Rplots.pdf
