BINARY=tw2exe
SRCFILES=src/*.pas src/*.pp src/version.pas
SRC=src/tw2exe.pas
EMPTY_TW5=src/empty.html

PRODOPTS=
DBGOPTS=#-g

ARCHES=linux64 #win32
BLDDIR=build/$(ARCH)
EXES= $(addsuffix /$(BINARY),$(addprefix build/,$(ARCHES)))
CURRVER:=$(shell git describe --tags --abbrev=4)

.PHONY: $(ARCHES) rel

checkversion:=$(shell if ! grep -Fqs `git describe --tags --abbrev=4` "src/version.pas"; then \
	   sed -e 's/___VERSION___/$(CURRVER)/' src/version.skel > "src/version.pas"; \
	fi)

all: $(EXES)

build/linux64/$(BINARY): $(SRCFILES)
	@mkdir -p $(@D)
	if [ "$$PROD" = "yes" ]; then \
		fpc -FE$(@D) $(PRODOPTS) -Tlinux $(SRC); \
	else \
		fpc -FE$(@D) $(DBGOPTS) -Tlinux $(SRC); \
	fi

rel: $(EXES)
	@make clean
	@make PROD=yes all
	@for i in $(ARCHES); do \
		mkdir -p "rel/$$i"; \
		cp "build/$$i/$(BINARY)" "rel/$$i/"; \
	done

clean:
	rm -rf build
