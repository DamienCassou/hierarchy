CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
EMACSFLAGS   = --batch -Q
EMACSBATCH   = $(EMACS) $(EMACSFLAGS)

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PROJ_ROOT   := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(wildcard test/*.el)
TAR          = $(DIST)/hierarchy-$(VERSION).tar


.PHONY: all test unit ecukes deps install uninstall reinstall clean-all clean
all : deps $(TAR)

deps :
	$(CASK) install

install : $(TAR)
	$(EMACSBATCH) -l package -f package-initialize \
	--eval '(package-install-file "$(PROJ_ROOT)/$(TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/hierarchy-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean :
	rm -f *.elc
	rm -rf $(DIST)
	rm -f *-pkg.el

$(TAR) : $(DIST) $(SRCS)
	$(CASK) package $(DIST)

$(DIST) :
	mkdir $(DIST)

test: unit

unit: $(PKG_DIR)
	${CASK} exec ert-runner
