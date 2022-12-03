SHELL:=/bin/bash

RACO?=raco
FMT?=fmt
REVIEW?=review
EXE?=exe
DISTR?=distribute
TEST?=test

RKTDIR=.
DISTDIR=dist
EXECUTABLE=rackapprox

racket-format:
	diff -u <(cat $(RKTDIR)/*.rkt ) <($(RACO) $(FMT) $(RKTDIR)/*.rkt )


racket-format-fix:
	$(RACO) $(FMT) -i $(RKTDIR)/*.rkt


racket-lint:
	find $(RKTDIR) -type f -name *.rkt -print0 | xargs -0 -n1 $(RACO) $(REVIEW)

lint: racket-lint


$(EXECUTABLE):
	$(RACO) $(EXE) -o $@ $(RKTDIR)/*.rkt

package: $(EXECUTABLE)
	$(RACO) $(DISTR) $@ $<


racket-test:
	$(RACO) $(TEST) $(RKTDIR)

test: racket-test


clean:
	rm -rf package
	rm -f $(EXECUTABLE)

.PHONY: test lint racket-lint racket-format racket-format-fix racket-test clean
