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
$(shell mkdir -p datafiles)
$(shell touch -t $$(date -v-30M +%C%y%m%d%H%M.%S) datafiles/filelist.txt.DONE)

datafiles/filelist.txt: datafiles/filelist.txt.DONE
	mkdir -p datafiles
	svn ls 'svn+ssh://peterm@herbie.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

#some data files get excluded (because they were empty or malformed, usually)
unexcluded.txt: exclusions.txt datafiles/filelist.txt
	comm -2 -3 <(sort $(word 2,$^)) <(sort $<) > $@

#We also match against script files...
scripts.txt: 
	echo $(wildcard *.R) > $@

Makefile.makemake.gen: makemake.py Makefile.makemake unexcluded.txt scripts.txt
	./makemake.py @Makefile.makemake --files @unexcluded.txt @scripts.txt > $@

include Makefile.makemake.gen

.PRECIOUS: discrimination.sqlite adjustment.sqlite

discrimination.sqlite: discrimination.sqlite.DONE

adjustment.sqlite: adjustment.sqlite.DONE

all: Makefile.makemake.gen discrimination.sqlite.DONE adjustment.sqlite.DONE graphs links descriptions series collections


