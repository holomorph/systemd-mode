NAME = systemd-mode

PREFIX = /usr/local
datarootdir := $(PREFIX)/share
emacsdir := $(datarootdir)/emacs/site-lisp

EMACS = emacs

all: $(NAME).elc

clean:
	$(RM) $(NAME).elc

install:
	install -d $(DESTDIR)$(emacsdir)/$(NAME)
	install -m644 $(NAME).{el,elc} $(DESTDIR)$(emacsdir)/$(NAME)

.el.elc:
	$(EMACS) --batch -f batch-byte-compile $<

.PHONY: all clean install
