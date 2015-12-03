THISARCH:=$(shell gcc -dumpmachine)
BLDDIR=build/$(ARCH)
ARCHES=linux64 #win32
BINARY=tw2exe
SRCFILES=src/*.pas src/*.pp src/version.pas
SRC=src/tw2exe.pas
EXES= $(addsuffix /$(BINARY),$(addprefix build/,$(ARCHES)))
CURRVER:=$(shell git describe --tags --abbrev=4)

.PHONY: $(ARCHES) rel

checkversion:=$(shell if ! grep -Fqs `git describe --tags --abbrev=4` "src/version.pas"; then \
	   sed -e 's/___VERSION___/$(CURRVER)/' src/version.skel > "src/version.pas"; \
	fi)

all: $(EXES)

build/linux64/$(BINARY): $(SRCFILES)
	@mkdir -p $(@D)
	$(checkversion)
	fpc -FE$(@D)  -Tlinux $(SRC)

checkversion:

rel: $(EXES)
	@for i in $(ARCHES); do \
		mkdir -p "rel/$$i"; \
		cp "build/$$i/$(BINARY)" "rel/$$i/"; \
	done

clean:
	rm -rf build
