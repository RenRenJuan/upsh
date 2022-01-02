# create the executable at build time

all::
	swipl -g 'ensure_loaded(library(upsh)),upsh_make,halt' -t 'halt(1)'

check::
	echo "TESTING upsh"
	upsh scripts/say tara
	echo "VERSION is upsh"
	upsh v

install::

checko::
	LD_LIBRARY_PATH=/usr/local/users/na11/local/git/lib/:/lib64/:/usr/lib64/:/lib/:/usr/lib/ swipl $(PLPATHS) -g test_rocksdb,halt -t 'halt(1)' test/test_rocksdb.pl

distclean: clean
	rm -f $(PACKSODIR)/rocksdb4pl.$(SOEXT)

clean:
	rm -f *~

