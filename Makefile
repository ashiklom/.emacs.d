all: compile

clean:
	rm -f init.elc configuration.el configuration.elc

compile: init.el configuration.org
	emacs -Q --batch -l 'lisp/compile.el'
