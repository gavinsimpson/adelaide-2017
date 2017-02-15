01-time-series.pdf: 01-time-series.tex
	R CMD texi2pdf --build=clean 01-time-series.tex

02-stratigraphic-data.pdf: 02-stratigraphic-data.Rmd
	Rscript -e "rmarkdown::render(\"02-stratigraphic-data.Rmd\")"

03-time-series-prac.pdf: 03-time-series-prac.Rnw
	R CMD Sweave 03-time-series-prac.Rnw
	R CMD texi2pdf --build=clean 03-time-series-prac.tex
	rm 03-time-series-prac-*.pdf Rplots.pdf

04-stratigraphic-prac.pdf: 04-stratigraphic-prac.Rnw
	R CMD Sweave 04-stratigraphic-prac.Rnw
	R CMD texi2pdf --build=clean 04-stratigraphic-prac.tex
	rm Rplots.pdf

purl: 01-time-series.Rmd  02-stratigraphic-data.Rmd
	Rscript -e "knitr::purl(\"01-time-series.Rmd\")"
	Rscript -e "knitr::purl(\"02-stratigraphic-data.Rmd\")"

handout-01-time-series.pdf: 01-time-series.pdf
	pdfnup 01-time-series.pdf --frame true --outfile handout-01-time-series.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95

handout-02-stratigraphic-data.pdf: 02-stratigraphic-data.pdf
	pdfnup 02-stratigraphic-data.pdf --frame true --outfile handout-02-stratigraphic-data.pdf --delta "0.2cm 0.2cm" --nup 2x2 --scale 0.95
