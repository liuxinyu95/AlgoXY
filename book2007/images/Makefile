IMGS = archimedes beach clouds essay6_counter essay6_mvc essay6_mvc_list huaisu mount_tai plis_env plum_blossom qmsht syntax_tree wuyue yellow_river

SRCS = archimedes.jpg beach.png clouds.jpg essay6_counter.png essay6_mvc.png essay6_mvc_list.png huaisu.jpg mount_tai.jpg plis_env.gif plum_blossom.gif qmsht.jpg syntax_tree.gif wuyue.jpg yellow_river.jpg

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
