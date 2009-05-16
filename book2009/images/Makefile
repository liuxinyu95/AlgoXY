IMGS = peace

SRCS = peace.gif

.SUFFIXES: .eps .jpg .png .gif

.PHONY: images

images: $(IMGS)
	@echo 'build eps files ok.'

$(IMGS):%:%.eps

.gif.eps:
	@echo 'building target: $@...'
	convert $< $@

.jpg.eps:
	@echo 'building target: $@...'
	convert $< $@

.png.eps:
	@echo 'building target: $@...'
	convert $< $@
clean:
	rm -f *.eps
