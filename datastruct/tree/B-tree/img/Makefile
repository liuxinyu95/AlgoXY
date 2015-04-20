IMG_OBJECTS = btree-del-case2a btree-del-case2b btree-del-case2c \
btree-del-case3a btree-del-case3b btree-del-fp-merge btree-del-fp-make \
btree-del-fp-fixlow
DOT_OBJECTS = btree-example btree-node split-node-before split-node-after \
btree-insert-2-3-4 btree-insert-3 insert-before insert-after \
btree-insert-fp-234 btree-insert-fp-3 btree-del-before btree-del-F \
btree-del-M btree-del-G btree-del-D btree-del-B btree-del-U \
btree-del-fp-before btree-del-fp-E btree-del-fp-G btree-del-fp-A \
btree-del-fp-M btree-del-fp-U btree-insert-simple1 btree-insert-simple2
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
