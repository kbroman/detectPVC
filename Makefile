all: doc README.md docs/detectPVC.html

.PHONY: doc

# build package documentation
doc:
	R -e 'devtools::document()'

# run tests
test:
	R -e 'devtools::test()'

# build README.md
README.md: README.Rmd man/figures/logo.png
	R -e "knitr::knit('$<')"

docs/detectPVC.html: vignettes/detectPVC.Rmd man/figures/logo.png docs/badges.html docs/paste_badges.R
	cd $(<D);R -e "rmarkdown::render('$(<F)')"
	mv $(<D)/$(@F) $@
	cd $(@D);paste_badges.R $(@F)

docs/detectPVC_logo.png: man/figures/logo.png
	cp $< $@
