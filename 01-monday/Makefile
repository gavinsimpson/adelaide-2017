all: 01-intro.pdf 02-learning-r.pdf 03-linear-models.pdf 04-linear-models-prac.pdf

01-intro.pdf: 01-intro.Rmd
	Rscript -e 'rmarkdown::render("01-intro.Rmd")'

02-learning-r.pdf: 02-learning-r.Rmd
	Rscript -e 'rmarkdown::render("02-learning-r.Rmd")'

03-linear-models.pdf: 03-linear-models.Rmd
	Rscript -e 'rmarkdown::render("03-linear-models.Rmd")'

04-linear-models-prac.pdf: 04-linear-models-prac.Rnw
	R CMD Sweave --clean 04-linear-models-prac.Rnw
	R CMD texi2pdf --build=clean 04-linear-models-prac.tex
	rm 04-linear-models-prac.aux 04-linear-models-prac.log

