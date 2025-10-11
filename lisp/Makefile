unexport JAVA_TOOL_OPTIONS

NAME=generator

all:     $(NAME) test documentation
.PHONY:: $(NAME) test tests documentation

SYSTEM               := com.informatimago.games.stonedge.generator
VERSION              := 1.0.0
COPYRIGHT            := Copyright 2024 - 2025 Pascal Bourguignon  --  License: Proprietary
SOURCES              := $(SYSTEM).asd *.lisp


# default values:
ART_DIR              := tests/artifacts/
THIS_DIR             := $(abspath $(CURDIR))/
DEP_DIR              := $(HOME)/src/public/lisp/

BIN_DIR              := bin-$(shell uname -m|tr 'A-Z' 'a-z')

LOAD_SYSTEM = \
		--eval '(load \#P"~/quicklisp/setup.lisp")' \
		--eval '(push "$(DEP_DIR)"    ql:*local-project-directories*)' \
		--eval '(push \#P"$(THIS_DIR)" asdf:*central-registry*)' \
		--eval '(ql:quickload :$(SYSTEM) :verbose nil :silent t)'
GET_SOURCES = \
		--eval '(format t "~{~A~^ ~}" (mapcar (lambda (c) (namestring (asdf:component-relative-pathname c))) (remove-if-not (lambda (c) (typep c (quote asdf:source-file))) (asdf:required-components (asdf:find-system :$(SYSTEM))))))'

# SOURCES   := $(shell sbcl --noinform --no-userinit --non-interactive $(LOAD_SYSTEM) $(GET_SOURCES) )

test tests:
# 	sbcl --noinform --no-userinit --non-interactive \
# 		--eval '(load #P"~/quicklisp/setup.lisp")' \
# 		--eval '(push "$(DEP_DIR)"   ql:*local-project-directories*)' \
# 		--eval '(push #P"$(THIS_DIR)" asdf:*central-registry*)' \
# 		--eval '(ql:quickload :$(SYSTEM).tests)' \
# 		--eval '(sb-ext:exit :code (if (cl-naive-tests:report) 0 200))'

# --eval '(cl-naive-tests:run)' \
# --eval '(cl-naive-tests:run)' \
# --eval '(cl-naive-tests:write-results cl-naive-tests:*suites-results* :format :text)' \
# --eval '(cl-naive-tests:save-results cl-naive-tests:*suites-results* :file "$(ARTDIR)junit-results.xml" :format :junit)' \
#

documentation:
#	make -C docs pdfs


$(NAME): $(NAME)-lisp-ccl
	mv $(NAME)-lisp-ccl $(BIN_DIR)/generator


ALL_PROGRAMS=       \
	$(NAME)-c        \
	$(NAME)-ecl       \
	$(NAME)-java       \
	$(NAME)-pascal      \
	$(NAME)-haskell      \
	$(NAME)-lisp-ccl      \
	$(NAME)-lisp-ecl       \
	$(NAME)-lisp-sbcl       \
	$(NAME)-lisp-clisp       \
	$(NAME)-lisp-clisp-fas


ECL_INCS=-I/opt/local/include
ECL_LIBS=-L/opt/local/lib -lecl
ECL_RUN=DYLD_LIBRARY_PATH=/opt/local/lib:$(DYLD_LIBRARY_PATH) LD_LIBRARY_PATH=/opt/local/lib:$(LD_LIBRARY_PATH)

FPC=fpc
CLISP=clisp
CCL=ccl
ECL=ecl
SBCL=sbcl
CC=cc
HASKELL=ghc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)

define compile
	@echo "$2" >  $1
	@$2        >> $1 2>&1 || cat $1
endef

$(NAME)-c:$(NAME).c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	$(call compile,$(NAME)-c.log,$(CC) -o $@ $(NAME).c)

$(NAME)-c-static:$(NAME).c
	@printf "// Generating Static Executable from %s source: %s\n" "C" $@
	$(call compile,$(NAME)-c-static.log,$(CC) -static -o $@ $(NAME).c)

$(NAME)-java:$(NAME).java
	@printf "// Generating Executable from %s source: %s\n" "Java" $@
	$(call compile,$(NAME)-java.log,javac $(NAME).java && jar cf $(NAME).jar HelloWorld.class && printf '#!/bin/sh\nexec java HelloWorld $(NAME).jar\n' > $@ && chmod 755 $@)

$(NAME)-pascal:$(NAME).pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	$(call compile,$(NAME)-pascal.log,$(FPC) -o$(NAME)-pascal $(NAME).pas)

$(NAME)-lisp-ccl:generate-$(NAME).lisp generate.lisp $(SOURCES)
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	$(call compile,$(NAME)-lisp-ccl.log,$(CCL) -n < generate-$(NAME).lisp)
	-@mv $(NAME) $(NAME)-lisp-ccl

$(NAME)-lisp-clisp:generate-$(NAME).lisp generate.lisp $(NAME).lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	$(call compile,$(NAME)-lisp-clisp.log,$(CLISP) -norc < generate-$(NAME).lisp)
	-@mv $(NAME) $(NAME)-lisp-clisp

$(NAME)-lisp-clisp-fas:Makefile $(NAME).fas $(NAME)-clisp-fas-rt.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@(echo '#!/usr/local/bin/clisp -norc -ansi -q -E utf-8' ;\
	  cat $(NAME).fas $(NAME)-clisp-fas-rt.fas ) > $@
	@chmod 755 $@

$(NAME)-clisp-fas-rt.fas:$(NAME)-clisp-fas-rt.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,$(NAME)-lisp-clisp-fas-rt.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

$(NAME).fas:$(NAME).lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,$(NAME)-lisp-clisp-fas.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

$(NAME)-lisp-sbcl:generate-$(NAME).lisp generate.lisp $(NAME).asd $(NAME).lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	$(call compile,$(NAME)-lisp-sbcl.log,$(SBCL) --no-userinit < generate-$(NAME).lisp)
	-@mv $(NAME) $(NAME)-lisp-sbcl

$(NAME)-lisp-ecl:generate-$(NAME).lisp generate.lisp $(NAME).asd $(NAME).lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,$(NAME)-lisp-ecl.log,$(ECL) -norc < generate-$(NAME).lisp)
	-@mv $(NAME) $(NAME)-lisp-ecl

$(NAME)-ecl-lisp:$(NAME)-ecl-lisp.c $(NAME).lisp
	@printf "// Generating Object from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,$(NAME)-ecl-lisp-lisp.log,$(ECL) --norc --eval '(compile-file "$(NAME).lisp")' --eval '(quit)')
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,$(NAME)-ecl-lisp-c.log,$(CC) -o $(NAME)-ecl-lisp $(NAME)-ecl-lisp.c $(NAME).fas $(ECL_INCS) $(ECL_LIBS))

# ecl-hello-r-lisp: ecl-hello-r-lisp.c ecl-hello-r-lisp.a
# 		$(CC) `ecl-config --cflags` -o $@ ecl-hello-r-lisp.c ecl-hello-r-lisp.a `ecl-config --ldflags` -lecl
#
# ecl-hello-r-lisp.a: constants.h ecl-hello-r-lisp.lisp
# 		# HACK: Force recompilation of ecl-hello-r-lisp.lisp
# 		# when header file changes. There's likely a better
# 		# way to do this via asdf:make-build ...
# 		touch ecl-hello-r-lisp.lisp
# 		ecl -norc \
# 		-eval '(require :asdf)' \
# 		-eval '(push "./" asdf:*central-registry*)' \
# 		-eval '(asdf:make-build :ecl-hello-r-lisp :type :static-library :move-here "./" :init-name "init_lib_ECL_HELLO_R_LISP")' \
# 		-eval '(quit)'


$(NAME)-ecl:$(NAME)-ecl.c
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,$(NAME)-ecl-c.log,$(CC) -o $(NAME)-ecl $(NAME)-ecl.c $(ECL_INCS) $(ECL_LIBS))

$(NAME)-haskell:$(NAME).hs
	@printf "// Generating Executable from %s source: %s\n" "Haskell" $@
	-@rm -f *.o
	$(call compile,$(NAME)-haskell.log,$(HASKELL) $^)
	-@mv $(NAME) $(NAME)-haskell

# test:$(ALL_PROGRAMS)
# 	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$(case $$p in (*ecl*) $(ECL_RUN) ./$$p ;; (*) ./$$p ;; esac)" ; done
# 	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f *.o *.fas *.lib *.log *.hi *.jar *.class *.[dlw]x64fsl
	-rm -f $(ALL_PROGRAMS)
