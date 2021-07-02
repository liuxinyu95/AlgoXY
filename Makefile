all: cn en
cn: algoxy-zh-cn.pdf
en: algoxy-en.pdf
pdf: $(patsubst %.dot,%.pdf,$(shell find -name '*.dot'))
.PHONY: all cn en pdf

algoxy-en.pdf algoxy-zh-cn.pdf: pdf
%.ps:  %.dot; dot -Tps  -o $@ $<
%.pdf: %.dot; dot -Tpdf -o $@ $<
%.pdf: %.tex; latexmk -xelatex $<
