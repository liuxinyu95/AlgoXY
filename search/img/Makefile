IMG_OBJECTS = saddleback-xx-yy maze maze-loop queen queens-example leapfrogs leapfrog1 \
leapfrog2 leapfrog3 solitaire draught-board hop-over wgc-puzzle klotski-cn1 klotski-cn2 \
klotski-jp
DOT_OBJECTS = max-sum-invariant strstr kmp kmp-fallback kmp-prefix-func fp-strstr fallback-tr \
lazy-fallback-tr good-char good-char-2 good-char-3 good-char-4 bad-char-1 bad-char-2 \
good-suffix-case1-1 good-suffix-case2-1 good-suffix-case2-2 dfs-stack directed-graph \
water-jugs weighted-dag huffman-tr huffman-tr1 huffman-tr2 huffman-tr3 huffman-tr4 \
huffman-tr5 huffman-tr6 huffman-tr-v1 huffman-tr-v3
DOT_SOURCES = $(foreach file, $(DOT_OBJECTS), $(file).dot)

#suffix replacement, replace .dot with .png
PNG_OBJECTS = $(DOT_SOURCES:.dot=.png)
EPS_OBJECTS = $(foreach file, $(IMG_OBJECTS), $(file).eps)

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
	rm -f *.ps *~
	rm -f $(PNG_OBJECTS)
	rm -f $(EPS_OBJECTS)
