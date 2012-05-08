.DEFAULT_GOAL = all
SHELL = /bin/bash

filelist.txt:
	svn ls 'svn+ssh://peterm@herbie.shadlen.org/home/peterm/svn/eyetracking/data' | sed 's/^/virtualdatafiles\//;' > $@

unexcluded.txt: exclusions.txt filelist.txt
	comm -2 -3 <(sort $(word 2,$^)) <(sort $<) > $@

.ALWAYS: filelist.txt

Makefile.makemake.gen: makemake.py Makefile.makemake unexcluded.txt
	./makemake.py @Makefile.makemake --files @unexcluded.txt > $@

include Makefile.makemake.gen

.PRECIOUS: database.sqlite

all: Makefile.makemake.gen discrimination.sqlite.DONE adjustment.sqlite.DONE graphs