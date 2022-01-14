IMG_OBJECTS = kids-inline merge burn-candle-2-ends
DOT_OBJECTS = partition-1-way partition-1-way-start partition-1-way-finish qsort-best partition-2-way \
partition-2-way-inner partition-3-way partition-3-way-end partition-3-way-lumoto unbalanced \
unbalanced-2 unbalanced-3 unbalanced-zigzag merge-in-place-naive merge-workarea merge-in-place-start \
merge-in-place-quater merge-in-place-setup merge-in-place-merged-quater nature-merge-sort \
nature-msort-invariant bottom-up-msort merge-with-sentinel
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
	dot -Gsize="8,10.5" -Tpng -o $@ $<

clean:
	rm -f *.ps *.eps trie-en* *~
	rm -f $(PNG_OBJECTS)
