.POSIX:

LISP       ?= 'sbcl'
FLAGS_LOAD ?= '--load'
FLAGS_EVAL ?= '--eval'

all: style.css base64-style.org

.PHONY: clean test

style.css: style.lisp Makefile
	$(LISP) $(FLAGS_LOAD) style.lisp   \
	  $(FLAGS_EVAL) '(generate)'       \
	  $(FLAGS_EVAL) '(quit)'

base64-style.org: style.lisp Makefile
	$(LISP) $(FLAGS_LOAD) style.lisp   \
	  $(FLAGS_EVAL) '(generate)'       \
	  $(FLAGS_EVAL) '(quit)'

clean:
	@rm -f style.css base64-style.org

test: style.css
	csslint --ignore=ids,bulletproof-font-face style.css
