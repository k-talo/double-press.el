EMACS ?= emacs
EMACSFLAGS ?= -Q --batch -L .
# Optional package directory (for package.el); can be overridden:
#   make package-lint PKGDIR="$HOME/.emacs.d/elpa"
PKGDIR ?=

EL  := double-press.el
ELC := $(EL:.el=.elc)
TESTS := $(wildcard test-*.el)
TMPDIR := .tmp

.PHONY: all help compile byte-compile test check checkdoc package-lint lint clean

all: compile test

help:
	@echo "Targets:"
	@echo "  compile        Byte-compile $(EL)"
	@echo "  test           Run ERT tests (loads $(TESTS))"
	@echo "  checkdoc       Run checkdoc on $(EL)"
	@echo "  package-lint   Run package-lint on $(EL)"
	@echo "  check          Run tests, checkdoc, and package-lint"
	@echo "  clean          Remove .elc and temporary files"
	@echo "Variables:"
	@echo "  EMACS          Emacs binary (current: $(EMACS))"
	@echo "  EMACSFLAGS     Common flags (current: $(EMACSFLAGS))"

compile: $(ELC)
byte-compile: compile

$(ELC): $(EL)
	$(EMACS) $(EMACSFLAGS) -f batch-byte-compile $<

$(TMPDIR):
	mkdir -p $(TMPDIR)

test: $(TMPDIR)
	$(EMACS) $(EMACSFLAGS) \
	  -eval '(setq temporary-file-directory (expand-file-name "$(TMPDIR)/" default-directory))' \
	  -l $(EL) $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) $(EMACSFLAGS) -l checkdoc \
	  --eval "(checkdoc-file \"$(EL)\")"

package-lint lint:
	@$(EMACS) $(EMACSFLAGS) \
	  --eval "(let ((dir \"$(PKGDIR)\")) \
                (when (and dir (not (string= dir \"\"))) \
                  (setq package-user-dir (expand-file-name dir))))" \
	  --eval "(package-initialize)" \
	  --eval "(if (require 'package-lint nil t) \
                   (package-lint-batch-and-exit) \
                 (progn (princ \"[skip] package-lint not installed; \") \
                        (princ \"run M-x package-install RET package-lint RET\\n\") \
                        (kill-emacs 0)))"  $(EL)

lint: checkdoc package-lint

check: test lint

clean:
	rm -f $(ELC)
	rm -rf $(TMPDIR)
