BINARY=twexe
SRCFILES=src/*.pas src/*.pp src/version.pas
SRC=src/twexe.pas
EMPTY_TW5=src/empty.html

PRODOPTS=
DBGOPTS=#-g

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
	if [ "$$PROD" = "yes" ]; then \
		fpc -FE$(@D) $(PRODOPTS) -Tlinux $(SRC); \
	else \
		fpc -FE$(@D) $(DBGOPTS) -Tlinux $(SRC); \
	fi

rel:
	mkdir -p rel/$(THISARCH)
	build/$(THISARCH)/$(BINARY) -n src/tw5editions/empty.html && \
		mv src/tw5editions/empty rel/$(THISARCH)

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
