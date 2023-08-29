all: install add-path
install:
	@if which stack >/dev/null; then \
		stack build; \
		mkdir $(HOME)/.mpl; \
		mkdir $(HOME)/.mpl/bin; \
		cp `find . -name mpl-client | grep install` $(HOME)/.mpl/bin; \
		cp `find . -name mpl | grep install` $(HOME)/.mpl/bin; \
		echo "CaMPL was installed Successfully!"; \
	else \
		echo "Installation failed. Please make sure that you have 'stack' installed"; \
	fi;
add-path:
	@echo "You need to add $(HOME)/.mpl/bin to your path"