.DEFAULT_GOAL = all
SHELL = /bin/bash

#We depend on some R packages, so we set the library path in an environment vairable
R_LIBS_USER := $(realpath .)/Rlibs:$(R_LIBS_USER)

Rlibs/install.packages.DONE:
	echo ${R_LIBS_USER}
	mkdir -p Rlibs
	Rscript install_packages.R $@
	touch -t 197001010000 $@

#R scripts depend on having the packages installed, but don't rebuild
#everything in account of installing R packages.
.OLD: Rlibs/install.package.DONE

#start by listing all of our data files out of SVN. Twiddle the modification times so that it doesn't check more than once a half hour.
filelist.txt: datafiles/filelist.txt.NEXT
	svn ls 'svn+ssh://peterm@herbie.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

#backdate this to a half hour ago. That way filelist.txt only gets refreshed every half hour...
datafiles/filelist.txt.NEXT:
	touch -t $$(date -v-30M +%C%y%m%d%H%M.%S) $@

#If you can't connect to the network, proceed anyway.
.IGNORE: filelist.txt

#some data files get excluded (because they were empty or malformed, usually)
unexcluded.txt: exclusions.txt filelist.txt
	comm -2 -3 <(sort $(word 2,$^)) <(sort $<) > $@


Makefile.makemake.gen: makemake.py Makefile.makemake unexcluded.txt
	./makemake.py @Makefile.makemake --files @unexcluded.txt > $@

include Makefile.makemake.gen

.PRECIOUS: database.sqlite

all: Makefile.makemake.gen discrimination.sqlite.DONE adjustment.sqlite.DONE graphs


