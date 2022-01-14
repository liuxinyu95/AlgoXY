IMG_OBJECTS = ring-buffer
FIG_OBJECTS = horseshoe-magnet
DOT_OBJECTS = empty-list enq-list-before enq-list-after deq-list-before \
deq-list-after ring-buf-1 ring-buf-2 ring-buf-3 ring-buf-4 ring-buf-5 \
list-queue-head list-queue-tail front-rear-queue front-rear-array-queue
DOT_SOURCES = $(foreach file, $(DOT_OBJECTS), $(file).dot)

#suffix replacement, replace .dot with .png
PNG_OBJECTS = $(DOT_SOURCES:.dot=.png)
EPS_OBJECTS = $(IMG_OBJECTS:.png=.eps)

.SUFFIXES: .eps .jpg .png .dot .ps

.PHONY: all

all: convert dot

convert: $(IMG_OBJECTS)

# If there is xfig installed
#    user can run 'make xfig' to generate eps file from fig source.
#    However, in order to minimize the tool-chain, we exclude xfig.
xfig: $(FIG_OBJECTS)

$(FIG_OBJECTS): % : %.eps

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

%.eps : %.fig
	fig2dev -L eps -m 1.00 -c $< $@

clean:
	rm -f *.ps *.pdf *~
	rm -f $(PNG_OBJECTS)
	rm -f $(EPS_OBJECTS)
