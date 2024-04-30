.PHONY: doc vignette

# build package documentation
doc:
	R -e 'devtools::document()'
