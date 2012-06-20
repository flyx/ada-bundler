ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?=
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GPRBUILD = gprbuild ${GNATFLAGS} -p

PACKAGING := Windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
	PACKAGING := MacOSX
endif
ifeq ($(UNAME), Linux)
	PACKAGING := Linux_Universal
endif


all: bundle

compile:
	mkdir -p bin
	mkdir -p obj
	${GPRBUILD} -P example.gpr -Xpackaging=${PACKAGING}

bundle: compile
	python ../src/tool/ada-bundler.py

clean:
	rm -rf ./obj ./bin