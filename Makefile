BINARY=twexe
LPI=src/twexe.lpi
SRCFILES=src/*.pas src/*.pp src/version.pas src/twexe.lpr
SRC=src/twexe.pas
EMPTY_TW5=src/empty.html

X86_64_LINUX=-B --os=linux --cpu=x86_64
I386_WIN32=-B --os=win32 --cpu=i386
PROD=yes

THISARCH=x86_64-linux
ARCHES=x86_64-linux i386-win32
BLDDIR=build/$(ARCH)
EXES= build/x86_64-linux/$(BINARY) build/i386-win32/$(BINARY).exe
CURRVER:=$(shell git describe --tags --abbrev=0)
WINLAZBUILD=wine "$$WINEAPPS/lazarus/lazbuild.exe"

.PHONY: $(ARCHES) version clean

checkversion:=$(shell if ! grep -Fqs `git describe --tags --abbrev=4` "src/version.pas"; then \
	   sed -e 's/___VERSION___/$(CURRVER)/' src/version.skel > "src/version.pas"; \
	fi)

BUILD= if [ "$(PROD)" = "yes" ]; then \
		$(1) $(2) --bm=Release $(LPI); \
	else \
		$(1) $(2) --bm=Debug $(LPI); \
	fi
DOEDITION=mkdir -p rel/$(1) && \
	  $(3) build/$(1)/$(2) -s -o rel/$(1) src/tw5editions/twexe.html;


all: $(EXES)

build/x86_64-linux/$(BINARY): $(SRCFILES)
	@mkdir -p $(@D)
	$(call BUILD,lazbuild,$(X86_64_LINUX))
	@echo Build linux version: $(CURRVER)

build/i386-win32/$(BINARY).exe: $(SRCFILES)
	@mkdir -p $(@D)
	$(call BUILD,$(WINLAZBUILD),$(I386_WIN32))
	@echo Built windows version: $(CURRVER)

rel: rel/x86_64-linux/$(BINARY) rel/i386-win32/$(BINARY).exe
	echo $(CURRVER) > rel/VERSION

rel/x86_64-linux/$(BINARY): build/x86_64-linux/$(BINARY)
	$(call DOEDITION,x86_64-linux,$(BINARY),)

rel/i386-win32/$(BINARY).exe: build/i386-win32/$(BINARY).exe
	$(call DOEDITION,i386-win32,$(BINARY).exe,wine)

version:
	@echo $(CURRVER)

clean:
	rm -rf build rel
