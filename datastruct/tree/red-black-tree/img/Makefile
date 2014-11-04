IMG_OBJECTS = del-case1 del-case2 del-case3 \

DOT_OBJECTS = unbalanced insert-haskell clrs-fig-13-4 python-insert \
db-fix unbalanced-2 unbalanced-3 unbalanced-zigzag rotate-l rotate-r \
rbt-example insert-ll insert-lr insert-rr insert-rl insert-fixed \
db-fix-1-nil-before db-fix-1-nil-after db-fix-2-nil-before \
db-fix-2-nil-after case2-a case2-a1 case2-b case2-b1
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
