all: build install
build: dactyl/downloads scrollhighlight/scrollhighlight.xpi
scrollhighlight/scrollhighlight.xpi:
	cd scrollhighlight && \
	rm -f scrollhighlight.xpi && \
	zip scrollhighlight.xpi install.rdf bootstrap.js README.md
dactyl/downloads:
	touch dactyl/melodactyl/config.json
	cd dactyl && make
install: build
	firefox dactyl/downloads/pentadactyl*.xpi scrollhighlight/scrollhighlight.xpi
clean:
	rm -rf dactyl/downloads/
	rm -f scrollhighlight/*.xpi

