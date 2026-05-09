all: doc README.md

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
