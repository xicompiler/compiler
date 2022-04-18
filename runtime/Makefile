CFLAGS=-g -Wall -O2 -m64 -fno-stack-protector
#CFLAGS=-g -Wall -O2 -fno-stack-protector
CC=gcc

# Boehm-Demers-Weiser collector
GCLIBDIR = gc-7.6.4
GCLIBZIP = gc-7.6.4.tar.gz
GCLIB = $(GCLIBDIR)/.libs/libgc.a

# libatomic_ops (dependency of Boehm GC)
LIBATOMIC = libatomic_ops-7.6.2
LIBATOMICZIP = $(LIBATOMIC).tar.gz

ABI_FLAG = $(shell ../platform-flags.sh)
CINCS = -I./libxi
CFLAGS_EXAMPLES = -masm=intel -O0 -fno-stack-protector -fno-asynchronous-unwind-tables $(ABI_FLAG) $(CINCS)

ABI_FLAG=$(shell ./platform-flags.sh)

all: xifilt libxi.a examples/arr examples/fact

xifilt: demangle/demangle.o
	gcc $(ABI_FLAG) -o $@ $^

libxi.a: $(GCLIB) libxi/libxi.o
	rm -f $@
	cp $(GCLIB) $@
	ar rcs $@ libxi/libxi.o

%.s: %.c
	$(CC) $(CFLAGS_EXAMPLES) -o $@ -S $<
	perl -ni -e 'if (!/InlineAsm/) { print $$_ }' $*.s

%.o: %.c
	gcc $(CFLAGS) $(ABI_FLAG) -c -o $@ $<

$(GCLIB): $(GCLIBDIR)/configure
	cd $(GCLIBDIR); ./configure; make; make check

$(GCLIBDIR)/configure:
	# extract and install dependencies
	rm -f $(GCLIB)
	rm -rf $(GCLIBDIR)
	tar -xzvf deps/$(GCLIBZIP)
	tar -xzvf deps/$(LIBATOMICZIP)
	mv $(LIBATOMIC) $(GCLIBDIR)/libatomic_ops
	if [ -f ../gcconfig.h ] ; \
	then cp ../gcconfig.h $(GCLIBDIR)/include/private/ ; \
	fi;

examples/fact: examples/fact.s
	./linkxi.sh examples/fact.s -o examples/fact
	gcc -E examples/fact.s -o examples/fact.sx

examples/arr: examples/arr.s
	./linkxi.sh examples/arr.s -o examples/arr
	gcc -E examples/arr.s -o examples/arr.sx

clean:
	rm -f libxi.a xifilt
	rm -f libxi/libxi.o
	rm -f demangle/demangle.o
	rm -f examples/fact examples/arr examples/fact-init examples/*.s examples/*.sx
	rm -f *~

clobber: clean
	$(MAKE) -C $(GCLIBDIR) clean
	cp ../gcconfig.h $(GCLIBDIR)/include/private/

dust: clobber
	rm -f $(GCLIBZIP)
	rm -rf $(GCLIBDIR)

FILES = runtime/demangle runtime/libxi \
        runtime/linkxi.sh runtime/Makefile runtime/README.txt runtime/$(GCLIBDIR)\
        runtime/examples/*.c runtime/examples/*.s runtime/include \
        runtime/ChangeLog.txt runtime/platform-flags.sh

tarballs: $(GCLIBDIR)/configure
	make clean
	cd .. && tar cvz --exclude=CVS --exclude="*~" ${FILES} > runtime.tar.gz
	cd .. && zip runtime.zip -r ${FILES} -x '*CVS*' -x "*~"
