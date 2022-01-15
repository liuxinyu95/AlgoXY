all: cn en

BOOK-CN := $(wildcard *-zh-cn.tex)
BOOK-EN := $(wildcard *-en.tex)

cn en: pdf
cn: $(BOOK-CN:.tex=.pdf)
en: $(BOOK-EN:.tex=.pdf)

DOTS = $(shell find -name '*.dot')
FIGS = $(shell find -name '*.fig')
PDFS = $(DOTS:.dot=.pdf) $(FIGS:.fig=.pdf)

pdf: $(PDFS)

%.ps:  %.dot; dot -Tps  -o $@ $<
%.eps: %.dot; dot -Teps -o $@ $<
%.pdf: %.dot; dot -Tpdf -o $@ $<
%.pdf: %.fig; fig2dev -L pdf $< $@
%.pdf: %.tex; latexmk -cd -xelatex $<

CHAPTERS-CN := $(shell egrep -l documentclass $$(find -name '*-zh-cn.tex' -a \! -name 'algoxy-*.tex'))
CHAPTERS-EN := $(shell egrep -l documentclass $$(find -name '*-en.tex' -a \! -name 'algoxy-*.tex'))
chapters: chapters-cn chapters-en
chapters-cn chapters-en: pdf
chapters-cn: $(CHAPTERS-CN:.tex=.pdf)
chapters-en: $(CHAPTERS-EN:.tex=.pdf)

.PHONY: all cn en pdf chapters chapters-cn chapters-en
