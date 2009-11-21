IMG_OBJECTS = radix-tree
DOT_OBJECTS = int-trie big-endian-trie little-endian-patricia \
int-patricia-insert-a int-patricia-insert-b int-patricia-insert-c \
int-patricia-insert-d int-patricia-haskell-insert trie-of-26 \
patricia-tree patricia-insert-a

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

clean:
	rm -f *.ps *.eps trie-en* *~
	rm -f $(foreach file, $(DOT_OBJECTS), $(file).png)