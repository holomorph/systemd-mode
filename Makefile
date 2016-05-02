SRC = systemd.el
DATA = unit-directives.txt network-directives.txt
DISTFILES := Makefile $(SRC) $(DATA) LICENSE README systemd-pkg.el tests

VERSION := $(shell awk '/^;; Version:/ {print $$3}' $(SRC))

PREFIX = /usr/local
datarootdir := $(PREFIX)/share
emacsdir := $(datarootdir)/emacs/site-lisp

EMACS = emacs

all: $(SRC:.el=.elc)

systemd-pkg.el: $(SRC)
	printf "(define-package \"systemd\" \"%s\" " $(VERSION) > $@
	echo "\"Major mode for editing systemd units\")" >> $@

systemd.elc: $(DATA)

check: tests/systemd-tests.el systemd.elc
	@$(EMACS) -Q --batch -L . --eval "(progn \
		(load-file \"tests/systemd-tests.el\") \
		(ert-run-tests-batch-and-exit))"

clean:
	$(RM) $(SRC:.el=.elc)

dist: clean systemd-pkg.el
	mkdir systemd-$(VERSION)
	cp -r $(DISTFILES) systemd-$(VERSION)
	tar cf systemd-$(VERSION).tar systemd-$(VERSION)
	rm -rf systemd-$(VERSION)

install:
	install -d $(DESTDIR)$(emacsdir)/systemd
	install -m644 $(SRC) $(SRC:.el=.elc) -t $(DESTDIR)$(emacsdir)/systemd
	install -m644 $(DATA) -t $(DESTDIR)$(emacsdir)/systemd

.el.elc:
	$(EMACS) -L . --batch -f batch-byte-compile $<

.PHONY: all check clean dist install
