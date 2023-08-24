
all: mpl-client mpl

mpl-client:
	if which stack >/dev/null; then \
		cd MPLCLIENT && stack build; \
		cd ~ && mkdir .mpl; \
		cd ~/.mpl && mkdir bin; \
		cp `find . -name mpl-client | grep install` ~/.mpl/bin; \
	else \
		echo "Installation failed. Please make sure that you have 'stack' installed"; \
	fi;
mpl:
	if cd which stack >/dev/null; then \
		cd MPLCLIENT && stack build; \
		cd MPLCLI && stack build; \
		cp `find . -name mpl | grep install` ~/.mpl/bin; \
	else \
		echo "Installation failed. Please make sure that you have 'stack' installed"; \
	fi;