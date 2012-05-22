.DEFAULT_GOAL = all
SHELL = /bin/bash

#We depend on some R packages, so we set the library path in an environment vairable
R_LIBS_USER := $(realpath .)/Rlibs:$(R_LIBS_USER)

Rlibs/install.packages.DONE:
	mkdir -p Rlibs
	Rscript install_packages.R $@
	touch -t 197001010000 $@

#R scripts depend on having the packages installed, but don't rebuild
#everything in account of installing R packages.
.OLD: Rlibs/install.package.DONE

#start by listing all of our data files out of SVN. I don't want it to
#keep checking if I'm rapidly iterating, so only update it if it's a
#half hour old...
$(shell touch -t $$(date -v-30M +%C%y%m%d%H%M.%S) filelist.txt.NOW)

filelist.txt: filelist.txt.NOW
	svn ls 'svn+ssh://peterm@herbie.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

#some data files get excluded (because they were empty or malformed, usually)
unexcluded.txt: exclusions.txt filelist.txt
	comm -2 -3 <(sort $(word 2,$^)) <(sort $<) > $@


Makefile.makemake.gen: makemake.py Makefile.makemake unexcluded.txt
	./makemake.py @Makefile.makemake --files @unexcluded.txt > $@

include Makefile.makemake.gen

.PRECIOUS: database.sqlite

all: Makefile.makemake.gen discrimination.sqlite.DONE adjustment.sqlite.DONE graphs links


