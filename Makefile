all: configuration.el

configuration.el: tangle.el configuration.org
	@emacs -Q --batch -l "tangle.el"

clean:
	rm -f init.elc configuration.el configuration.elc
