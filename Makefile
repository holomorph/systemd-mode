SRC = systemd.el systemd-company.el

PREFIX = /usr/local
datarootdir := $(PREFIX)/share
emacsdir := $(datarootdir)/emacs/site-lisp

EMACS = emacs

all: $(SRC:.el=.elc)

clean:
	$(RM) $(SRC:.el=.elc)

install:
	install -d $(DESTDIR)$(emacsdir)/systemd
	install -m644 $(SRC) $(SRC:.el=.elc) -t $(DESTDIR)$(emacsdir)/systemd

.el.elc:
	$(EMACS) -L . --batch -f batch-byte-compile $<

.PHONY: all clean install
