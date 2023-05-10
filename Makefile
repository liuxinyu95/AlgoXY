all: cn en

BOOK-CN := $(wildcard *-zh-cn.tex)
BOOK-EN := $(wildcard *-en.tex)

cn en: pdf
cn: $(BOOK-CN:.tex=.pdf)
en: $(BOOK-EN:.tex=.pdf)

TEX_FLAGS =

%.pdf: %.tex; latexmk -cd -lualatex $(TEX_FLAGS) $<

CHAPTERS-CN := $(shell egrep -l documentclass $$(find . -name '*-zh-cn.tex' -a \! -name 'algoxy-*.tex'))
CHAPTERS-EN := $(shell egrep -l documentclass $$(find . -name '*-en.tex' -a \! -name 'algoxy-*.tex'))
chapters: chapters-cn chapters-en
chapters-cn chapters-en: pdf
chapters-cn: $(CHAPTERS-CN:.tex=.pdf)
chapters-en: $(CHAPTERS-EN:.tex=.pdf)

FORCE-FLAGS = -g -use-make $(TEX_FLAGS)
force: force-cn force-en
force-cn:
	latexmk -cd -lualatex $(FORCE-FLAGS) algoxy-zh-cn.tex

force-en:
	latexmk -cd -lualatex $(FORCE-FLAGS) algoxy-en.tex

clean:
	git clean -fdx

.PHONY: all cn en pdf chapters chapters-cn chapters-en
