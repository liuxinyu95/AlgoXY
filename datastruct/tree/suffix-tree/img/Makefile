IMG_OBJECTS = implicit-end-point ep-ap
DOT_OBJECTS = stree-banana strie-banana stree-bananas \
stree-bananas-label strie-cacao strie-caca strie-cac strie-ca strie-c \
strie-empty stree-cacao stree-caca stree-cac stree-ca stree-c \
stree-mississippis
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
	dot -Gsize="8,10.5" -Tps -o $@ $<

png: $(PNG_OBJECTS)

%.png : %.dot
	dot -Tpng -o $@ $<

clean:
	rm -f *.ps *.eps trie-en* *~
	rm -f $(PNG_OBJECTS)
