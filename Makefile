ELPA_DEPENDENCIES	= package-lint let-alist buttercup
ELPA_ARCHIVES		= melpa-stable gnu
TEST_BUTTERCUP_OPTIONS  = .
LINT_CHECKDOC_FILES	= $(wildcard *.el) $(wildcard test/*.el)
LINT_PACKAGE_LINT_FILES	= ${LINT_CHECKDOC_FILES}
LINT_COMPILE_FILES	= ${LINT_CHECKDOC_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitea.petton.fr/DamienCassou/makel/raw/v0.5.3/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
