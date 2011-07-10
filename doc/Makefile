# A very stupid makefile xD

images  = $(shell find . -name '*\.png') $(shell find . -name '*\.svg')
sourcefiles = $(shell find .. -name '*\.lisp')

# Not stop on errors.
texflags = -interaction=nonstopmode

# Uncomment to supress the fu***** pdflatex command.
#quiet = yes

ifdef quiet
texflags += > /dev/null
endif 

.PHONY = all clean veryclean

all : informe.pdf

informe.pdf : informe.tex $(images) $(sourcefiles)
	pdflatex $(texflags) informe.tex
	pdflatex $(texflags) informe.tex

clean :
	@rm -vf *.aux *.toc *.log *.nav *.out *.snm *.vrb

veryclean: clean
	@rm -vf informe.pdf
