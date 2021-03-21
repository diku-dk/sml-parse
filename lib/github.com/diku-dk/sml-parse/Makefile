MLCOMP ?= mlkit

.PHONY: all
all:
	$(MLCOMP) -output parse.exe parse.mlb

.PHONY: test
test:
	$(MAKE) -C test test

.PHONY: clean
clean:
	$(MAKE) -C test clean
	rm -rf MLB *~ parse.exe
