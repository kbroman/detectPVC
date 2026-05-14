all: doc README.md docs/detectPVC.html

.PHONY: doc

# build package documentation
doc:
	R -e 'devtools::document()'

# run tests
test:
	R -e 'devtools::test()'

# build README.md
README.md: README.Rmd
	R -e "knitr::knit('$<')"

docs/detectPVC.html: vignettes/detectPVC.Rmd docs/detectPVC_logo.png
	cd $(<D);R -e "rmarkdown::render('$(<F)')"
	mv $(<D)/$(@F) $@

docs/detectPVC_logo.png: figures/detectPVC_logo.png
	cp $< $@
