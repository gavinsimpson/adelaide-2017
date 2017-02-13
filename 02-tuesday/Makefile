all: 01-glms.pdf 02-gams.pdf 03-glm-and-gam-prac.pdf

01-glms.pdf: 01-glms.Rmd
	Rscript -e 'rmarkdown::render("01-glms.Rmd")'

02-gams.pdf: 02-gams.Rmd
	Rscript -e 'rmarkdown::render("02-gams.Rmd")'

03-glm-and-gam-prac.pdf: 03-glm-and-gam-prac.Rnw
	R CMD Sweave 03-glm-and-gam-prac.Rnw
	R CMD texi2pdf --build=clean 03-glm-and-gam-prac.tex
	rm 03-glm-and-gam-prac-gam-check.pdf 03-glm-and-gam-prac-gam-plot.pdf 03-glm-and-gam-prac-plot-fitted-gam-darlingtonia.pdf Rplots.pdf
