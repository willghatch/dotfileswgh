all: build install
build: dactyl/downloads scrollhighlight/scrollhighlight.xpi adblockplus/abp.xpi
scrollhighlight/scrollhighlight.xpi:
	cd scrollhighlight && \
	rm -f scrollhighlight.xpi && \
	zip scrollhighlight.xpi install.rdf bootstrap.js README.md
dactyl/downloads:
	touch dactyl/melodactyl/config.json
	cd dactyl && make
adblockplus/abp.xpi:
	cd adblockplus && \
	python2 build.py build && \
	mv *.xpi abp.xpi
install: build
	firefox dactyl/downloads/pentadactyl*.xpi scrollhighlight/scrollhighlight.xpi adblockplus/abp.xpi
clean:
	rm -rf dactyl/downloads/
	rm -f scrollhighlight/*.xpi

