THISARCH:=`gcc -dumpmachine`
BLDDIR=build/$(ARCH)
ARCHES=linux64 #win32
BINARY=tw2exe
SRCFILES=src/*.pas
SRC=src/tw2exe.pas

.PHONY: $(ARCHES) rel

all: $(addprefix build/,$(ARCHES))

build/linux64/$(BINARY): $(SRCFILES)
	@mkdir -p $(@D)
	fpc -FE$(@D)  -Tlinux $(SRC)

rel: $(addsuffix /$(BINARY),$(addprefix build/,$(ARCHES)))
	@for i in $(ARCHES); do \
		mkdir -p "rel/$$i"; \
		cp "build/$$i/$(BINARY)" "rel/$$i/"; \
	done
