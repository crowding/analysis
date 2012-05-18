.DEFAULT_GOAL = all
SHELL = /bin/bash

#Set the environment variable for local package installation.
R_LIBS_USER := $(realpath Rlibs):$(R_LIBS_USER)

Rlibs/install.packages.DONE: install_packages.R
	mkdir -p Rlibs
	Rscript install_packages.R $@

#R scripts depend on having the packages installed, but don't rebuild
#everything in account of installing R packages.
.OLD: Rlibs/install.package.DONE

#start by listing all of our data files.
filelist.txt:
	svn ls 'svn+ssh://peterm@herbie.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

#always check the data and R installation before making.
.ALWAYS: filelist.txt # install.packages.R

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


