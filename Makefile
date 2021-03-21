.PHONY: all
all:
	$(MAKE) -C lib/github.com/diku-dk/sml-parse all

.PHONY: test
test:
	$(MAKE) -C lib/github.com/diku-dk/sml-parse test

.PHONY: clean
clean:
	$(MAKE) -C lib/github.com/diku-dk/sml-parse clean
	rm -rf MLB *~ .*~
