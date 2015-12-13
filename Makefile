BINARY=twexe
LPI=src/twexe.lpi
SRCFILES=src/*.pas src/*.pp src/version.pas src/twexe.lpr
SRC=src/twexe.pas
EMPTY_TW5=src/empty.html

X86_64_LINUX=-B --os=linux --cpu=x86_64
PROD=yes

THISARCH=x86_64-linux
ARCHES=x86_64-linux #win32
BLDDIR=build/$(ARCH)
EXES= $(addsuffix /$(BINARY),$(addprefix build/,$(ARCHES)))
CURRVER:=$(shell git describe --tags --abbrev=4)

.PHONY: $(ARCHES) rel version

checkversion:=$(shell if ! grep -Fqs `git describe --tags --abbrev=4` "src/version.pas"; then \
	   sed -e 's/___VERSION___/$(CURRVER)/' src/version.skel > "src/version.pas"; \
	fi)

all: $(EXES)

build/x86_64-linux/$(BINARY): $(SRCFILES)
	@mkdir -p $(@D)
	if [ "$(PROD)" = "yes" ]; then \
		lazbuild $(X86_64_LINUX) --bm=Release $(LPI); \
	else \
		lazbuild $(X86_64_LINUX) --bm=Debug $(LPI); \
	fi
	@echo Version: $(CURRVER)

rel:
	mkdir -p rel/$(THISARCH)
	build/$(THISARCH)/$(BINARY) -s src/tw5editions/empty.html && \
		mv src/tw5editions/empty rel/$(THISARCH)
	@echo empty at: rel/$(THISARCH)/empty

relold: $(EXES)
	@make clean
	@make PROD=yes all
	@for i in $(ARCHES); do \
		mkdir -p "rel/$$i"; \
		cp "build/$$i/$(BINARY)" "rel/$$i/"; \
	done

version:
	@echo $(CURRVER)

clean:
	rm -rf build
