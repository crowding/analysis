#!/usr/bin/env python
from __future__ import print_function
import re, os, optparse, string, glob

graphsTemplate = string.Template("""
${outfile} ${productions}: ${infile} ${scripts} graphs.R
	mkdir -p $$(dir ${outfile})
	[ -e $outfile ] && rm `cat ${outfile}` || true
	/usr/bin/env Rscript graphs.R ${infile} ${outfile} ${scripts}

${scriptnames}: ${outfile} ${productions}

.DUMMY ${productions}: ${outfile}

.PHONY: ${scriptnames} .DUMMY

all: ${outfile} ${productions}

Makefile.pygen: ${outfile}

$$(ALL_TARGETS) += ${outfile} ${productions}

""")

#need a debugging functuion to clear everything downstream of a file?

def makegen(files, rules):
    rules = [(pattern, replacement, script)
             for (pattern, replacement, script) in rules]
    collected_rules = {}
    for infile in files:
        for (pattern, replacement, script) in rules:
            (outfile, matched) = re.subn(pattern, replacement, infile, 1)
            if (matched > 0):
                if not outfile in files:
                    files.append(outfile)
                #for unique combinations of rule and outfile...
                if (infile, outfile) in collected_rules.keys():
                    collected_rules[(infile, outfile)].append(script)
                else:
                    collected_rules[(infile, outfile)] = [script]
    ##can't produce the same output file multiple ways... check?
    ##TODO
    for ((infile, outfile), scripts) in collected_rules.iteritems():
        #outfile may itself create files that are listed,
        #in which case we read that list.
        if os.access(outfile, os.F_OK):
            #if the file exists, it lists more targets.
            with open(outfile, 'r') as prodfile:
                productions = [i.strip() for i in prodfile.readlines()]
                [files.append(f) for f in productions if not f in files]
                productions = " ".join(productions)
        else:
            productions = ""
        scriptnames = " ".join([os.path.splitext(os.path.basename(s))[0] for s in scripts])
        scripts = " ".join(scripts)
        out = graphsTemplate.substitute(locals())
        print(out)

def makeRplots(args, **kwargs):
    pass
        
def test():
    makegen(glob.glob("unpacked/*.RData"), [
        (r"unpacked/(.*CritDistance.*).RData", r"outputs/\1.out", "criticalDistance.R"),
        (r"unpacked/(.*CritDistance.*).RData", r"outputs/\1.out", "performance.R"),
        (r"unpacked/(.*Segment.*).RData",      r"outputs/\1.out", "segment.R"), 
        ] )

# The big (only?) restriction with a Makefile is that only one rule can be used to produce one output.
# So for any one output file, we collect all the arguments related to making that file into one command invocation.
# This could also be associated with several other output files and a sequence of commands ?!
#
# Typing out loud, this is sort of how I want to specify my processing pipeline.
#
# Beware premature generalization, though.

# how does getting form SVN work if not real file names?
# --command svn ls http:// --outlist listing.txt --target svn
# --command svn export --inmatch 'http://(.*).log.gz' --out 'datafiles/\1' --intermediate
# --command --in translator.R --inlist 'datafiles/.*\.log\.gz' --out "unpacked/.*.RData" --target unpacked
# --command --in split.R --in-match 'unpacked/(.*).RData' --out 'stripped/\1.RData' --out 'eyemovements/\1.RData'
# --command --in-match 'eyemovements/(.*).RData' --out 'saccades/\1.RData'
# --command --in-match 'stripped/(.*).RData' --in 'saccades/\1.RData' --out 'saccades_marked.RData'



class ruleSpec:
    components = []
    matching = False
    targetname = ''
    intermediate = False

    class ruleResult:
        produced_files = []

class ruleComponentSpec:
    direction = 'none'
    listing = False
    matching = 'true'

class generator:
    rules = []
    files = []
    commands = {}; #dictionary on output files

    def generate(self):
        for f in files:
            for r in rules:
              #  result = r.try(f)
                if result.matched:
                    rule.results.append(result)
                    files.extend([ff for ff in result.products if not ff in files])
                    #scan the command that was produced and combien it with earlier commands....
                    previous_commands = [ff for ff in result.products if ff in keys(commands)]
        for r in results:
            print(r.output)
        
if (__name__ == "__main__"):
    #hack this so that rules take a varargs and --files is the terminator?
    parser = optparse.OptionParser()
    parser.add_option('-r', '--rule', action='append', dest="rules", nargs=3,
                      help="--rule PATTERN REPLACEMENT RSCRIPT")
    parser.add_option('--command', action='callback', callback=print)
    parser.add_option('--in', action='callback', nargs=1, callback=print)
    parser.add_option('--out', action='callback', nargs=1, callback=print)
    parser.add_option('--inlis', action='callback', nargs=1, callback=print)
    parser.add_option('--outlist', action='callback', nargs=1, callback=print)
    parser.add_option('--inmatch', action='callback', nargs=1, callback=print)
    parser.add_option('--outmatch', action='callback', nargs=1, callback=print)
    (options,args) = parser.parse_args()
    rules = options.rules

    makegen(files=args, rules=rules)
