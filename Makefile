README.html: README.Rmd
	R -e "rmarkdown::render('README.Rmd', "html_document")"

README.pdf: README.Rmd
	R -e "rmarkdown::render('README.Rmd', "pdf_document")"

README.docx: README.Rmd
	R -e "rmarkdown::render('README.Rmd', "word_document")"
