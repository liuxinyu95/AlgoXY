IMG_OBJECTS =
DOT_OBJECTS = tree-array-map-tree tree-array-map-array \
heapify-1 heapify-2 heapify-3 build-heap-array build-heap-1 \
build-heap-2 build-heap-3 build-heap-4 build-heap-5 build-heap-6 \
decrease-key-a decrease-key-b decrease-key-c decrease-key-d \
lvr rank leftist-tree unbalanced-leftist-tree skew-tree \
zig-zig-a zig-zig-b zig-zag-a zig-zag-b zig-a zig-b splay-tree
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
