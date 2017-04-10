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
	# The URLs are ublock-origin (607454) and https-exerywhere (229918)
	firefox dactyl/downloads/pentadactyl*.xpi \
		scrollhighlight/scrollhighlight.xpi \
		https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-607454-latest.xpi \
		https://addons.mozilla.org/firefox/downloads/latest/session-manager/addon-2324-latest.xpi?src=dp-btn-primary \
		https://addons.mozilla.org/firefox/downloads/latest/self-destructing-cookies/addon-415846-latest.xpi?src=dp-btn-primary \
		https://addons.mozilla.org/firefox/downloads/latest/tree-style-tab/addon-5890-latest.xpi?src=dp-btn-primary \
		https://addons.mozilla.org/firefox/downloads/latest/https-everywhere/addon-229918-latest.xpi?src=dp-btn-primary
	# other add-on notes:
	# "Find Bar Tweaks" - makes a circle animation around `find` results, but is incompatible with pentadactyl
clean:
	rm -rf dactyl/downloads/
	rm -f scrollhighlight/*.xpi

