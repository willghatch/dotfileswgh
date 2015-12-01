all: build install
build: dactyl/downloads scrollhighlight/scrollhighlight.xpi adblockplus/built-marker.xpi
scrollhighlight/scrollhighlight.xpi:
	cd scrollhighlight && \
	rm -f scrollhighlight.xpi && \
	zip scrollhighlight.xpi install.rdf bootstrap.js README.md
dactyl/downloads:
	touch dactyl/melodactyl/config.json
	cd dactyl && make
adblockplus/built-marker.xpi:
	cd adblockplus && \
	python2 build.py build && \
	touch built-marker.xpi
install: build
	firefox dactyl/downloads/pentadactyl*.xpi scrollhighlight/scrollhighlight.xpi adblockplus/adblockplus*.xpi
clean:
	rm -rf dactyl/downloads/
	rm -f scrollhighlight/*.xpi
	rm adblockplus/*.xpi
	rm -rf adblockplus/buildtools
	rm -rf adblockplus/adblockplusui

