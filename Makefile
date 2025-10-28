# Makefile for codesnap.el

# Package information
PACKAGE_NAME = codesnap
VERSION = 0.0.1
PACKAGE_DIR = $(PACKAGE_NAME)-$(VERSION)

# Emacs configuration
EMACS ?= emacs
EMACS_CMD = $(EMACS) -Q -batch

# Files
EL_FILES = codesnap.el
ELC_FILES = $(EL_FILES:.el=.elc)
SOURCES = $(EL_FILES)

# Distribution files
DIST_FILES = $(EL_FILES) README.md LICENSE Makefile

# Load path for compilation
LOAD_PATH = -L .

.PHONY: all compile clean check lint test package install uninstall help

# Default target
all: compile

# Compile Emacs Lisp files
compile: $(ELC_FILES)

%.elc: %.el
	$(EMACS_CMD) $(LOAD_PATH) -f batch-byte-compile $<

# Clean compiled files
clean:
	rm -f *.elc
	rm -rf $(PACKAGE_DIR)
	rm -f $(PACKAGE_NAME)-$(VERSION).tar

# Check package for issues
check: compile
	$(EMACS_CMD) $(LOAD_PATH) \
	--eval "(require 'package-lint)" \
	--eval "(setq package-lint-main-file \"$(PACKAGE_NAME).el\")" \
	-f package-lint-batch-and-exit $(EL_FILES)

# Lint Emacs Lisp code
lint:
	$(EMACS_CMD) $(LOAD_PATH) \
	--eval "(require 'checkdoc)" \
	--eval "(setq checkdoc-force-docstrings-flag nil)" \
	--eval "(setq checkdoc-arguments-in-order-flag nil)" \
	-f checkdoc-file $(EL_FILES)

# Run tests (placeholder for future test implementation)
test: compile
	@echo "No tests defined yet"

# Create package directory and tarball
package: clean
	mkdir -p $(PACKAGE_DIR)
	cp $(DIST_FILES) $(PACKAGE_DIR)/
	tar -cf $(PACKAGE_NAME)-$(VERSION).tar $(PACKAGE_DIR)
	@echo "Package created: $(PACKAGE_NAME)-$(VERSION).tar"

# Install package locally
install: compile
	$(EMACS_CMD) \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(package-initialize)" \
	--eval "(package-install-file \"$(PWD)/$(PACKAGE_NAME).el\")"

# Uninstall package
uninstall:
	$(EMACS_CMD) \
	--eval "(require 'package)" \
	--eval "(package-initialize)" \
	--eval "(package-delete (cadr (assq '$(PACKAGE_NAME) package-alist)))"

# Show help
help:
	@echo "Available targets:"
	@echo "  all       - Compile all Emacs Lisp files (default)"
	@echo "  compile   - Compile Emacs Lisp files to bytecode"
	@echo "  clean     - Remove compiled files and build artifacts"
	@echo "  check     - Run package-lint on the package"
	@echo "  lint      - Run checkdoc on Emacs Lisp files"
	@echo "  test      - Run tests (placeholder)"
	@echo "  package   - Create a package tarball for distribution"
	@echo "  install   - Install package locally"
	@echo "  uninstall - Uninstall package"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS     - Path to Emacs binary (default: emacs)"
	@echo ""
	@echo "Examples:"
	@echo "  make compile EMACS=/usr/local/bin/emacs"
	@echo "  make package"
	@echo "  make check"