.DEFAULT_GOAL = all
SHELL = /bin/bash

.DELETE_ON_ERROR:

#We depend on some R packages, so we set the library path in an environment vairable
R_LIBS_USER := $(realpath .)/Rlibs:${R_LIBS_USER}

#start by listing all of our data files out of SVN. I don't want it to
#keep checking if I'm rapidly iterating, so only update it if it's a
#half hour old...
$(shell mkdir -p datafiles)
$(shell touch -t $$(date -v-30M +%C%y%m%d%H%M.%S) datafiles/filelist.txt.DONE)

datafiles/filelist.txt: datafiles/filelist.txt.DONE
	mkdir -p datafiles
	svn ls 'file:///Users/peter/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@
#	svn ls 'svn+ssh://peterm@pat.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

#some data files get excluded (because they were empty or malformed, usually)
unexcluded.txt: exclusions.txt datafiles/filelist.txt
	comm -2 -3 <(sort $(word 2,$^)) <(sort $<) > $@

#We also match anything that's checked into version control.
FILES := $(filter-out $(MAKEFILE_LIST),$(shell git ls-files --cached))

##why the hell is this not getting regenerated on changes?
monk.makefile: monk/monk.py Monkfile monk/autodep/dependencies.monkfile import.monkfile writing/Monkfile database.monkfile unexcluded.txt
	./monk/monk.py @Monkfile --files $(FILES) @unexcluded.txt  > $@ || rm $@

#make the subproject recursively (ugh)
logfile-reader/readR.R: logfile-reader/logfile

logfile-reader/logfile:
	cd logfile-reader; $(MAKE) $(MFLAGS)

.PRECIOUS: discrimination.sqlite adjustment.sqlite

discrimination.sqlite: discrimination.sqlite.DONE

adjustment.sqlite: adjustment.sqlite.DONE

clean:
	git clean -dfx

ifneq (,$(wildcard monk.makefile))

include monk.makefile
all:  discrimination.sqlite.DONE adjustment.sqlite.DONE graphs links descriptions series collections csv mat figures mov writing pdf

else

$(MAKEFILE_LIST): monk.makefile

endif

all: $(MAKEFILE_LIST)

