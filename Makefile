INSTALL_DIR = /usr/local/bin

ifneq ($(shell command -v chicken-csc),)
CSC = chicken-csc
else
CSC = csc
endif

all: sdrow

sdrow: sdrow.scm
	$(CSC) sdrow.scm -output-file $@ -postlude '(main (command-line-arguments))'

install: all
	install sdrow $(INSTALL_DIR)

uninstall:
	$(RM) $(INSTALL_DIR)/sdrow

clean:
	$(RM) sdrow sdrow.c

.PHONY: clean all install uninstall
