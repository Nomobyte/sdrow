INSTALL_DIR = /usr/local/bin

all: sdrow

sdrow: sdrow.scm
	cp sdrow.scm $@
	chmod +x $@

install: all
	install sdrow $(INSTALL_DIR)

uninstall:
	$(RM) $(INSTALL_DIR)/sdrow

clean:
	$(RM) sdrow

.PHONY: clean all install uninstall
