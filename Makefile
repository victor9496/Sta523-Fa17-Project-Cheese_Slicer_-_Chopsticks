all: final_proj.html

hw4.html: hw4.Rmd data/lq.Rdata data/dennys.Rdata
	Rscript -e "library(rmarkdown);render('hw4.Rmd')"
	
data/lq.Rdata: parse_lq.R data/lq/*.html
	Rscript parse_lq.R
	
	
clean:
	rm -f hw4.html
	rm -rf data/
	
.phony: all cleanmake
