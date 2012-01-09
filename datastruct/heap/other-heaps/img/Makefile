IMG_OBJECTS = bheap-pop
DOT_OBJECTS = b0tree b1tree b2tree b3tree b4tree link-bitree \
bheap2 link-bitree-xy link-bitree-lcrs lcrs bheap-merge-1 \
bheap-merge-2 cut-fib-tree fib-meld-01 fib-meld-02 fib-meld-03 \
fib-meld-04 fib-meld-05 fib-meld-06 fib-meld-07
DOT_SOURCES = $(foreach file, $(DOT_OBJECTS), $(file).dot)

#suffix replacement, replace .dot with .png
PNG_OBJECTS = $(DOT_SOURCES:.dot=.png)

.SUFFIXES: .eps .jpg .png .dot .ps

.PHONY: all

all: convert dot

convert: $(IMG_OBJECTS)

# for details, refer to: 
#    http://www.gnu.org/software/autoconf/manual/make/
# Makefile syntax, replace each '%' to 'eps
$(IMG_OBJECTS): % : %.eps

# pattern rule matching
# Makefile auto variable: $< first prerequisite, $@ target
%.eps : %.png
	convert $< $@

dot: $(DOT_OBJECTS)

$(DOT_OBJECTS): % : %.ps

# Makefile auto variable: $< first prerequisite, $@ target
%.ps : %.dot
	dot -Tps -o $@ $<

png: $(PNG_OBJECTS)

%.png : %.dot
	dot -Tpng -o $@ $<

clean:
	rm -f *.ps *.eps trie-en* *~
	rm -f $(PNG_OBJECTS)
