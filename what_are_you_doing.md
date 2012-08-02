----------------------------------------------------------------------
Mon Jun 18 15:32:59 PDT 2012

Went to John Palmer's class.

Played with plotting threhsolds and biases as a function of target
spacing.

It appears that the most comprehensible graphs are made not by
plotting thresholds and x-intercepts, but by plotting slopes and
y-intercepts. This way of looking at it is much less sensitive to
wwhat happens when the psychometric function flattens out, and shows
the trends better. It's also much more straightforward to compute error
bars on slope and y-intercept vs. error bars on threshold and bias.

Currently I'm only looking at data where a single carrier direction
was run for the whole session.

Now I need to look at how I'm combining data in cases where I'm
combining data from multiple sources.

Ran NJ on varying eccentricities, I had been running her on varying
sizes at constant eccentricity.

Still have to hear back form Maria about getting R on the cluster. In
the meantime I looked again at glibc documentation to see if I can
configure it to build the 32 bit libraries (required for building gcc,
which is in turn required for building gfortran, which is in turn
required for building R)

Found this:

http://stackoverflow.com/questions/8004241/how-to-compile-glibc-32bit-on-an-x86-64-machine

Various things therein didn't really help.

Tried compiling GCC w/o multilib support:

    ../gcc-4.4.7/configure --prefix=$HOME --enable-languages=fortran --disable-multilib

so far so good? the test is whether R compiles under those conditions.

Looking for software to nudge me to take journal entries.

Reading 1st chapter of Davida Teller's book.

GCC compiled and installed so I tried compiling R. Configured like this:

    ./configure --prefix=$HOME --with-readline=no --with-x=no

Seems to compile and run.

Actually I compiled Readline and redid R.

Going back to Teller book.

Threw showers' pass jacket in envelope. Need to stop at post office to send it off.

Going home.

Mon Jun 18 20:24:02 PDT 2012
----------------------------------------------------------------------

Mon Jun 18 21:41:55 PDT 2012

at home

read a little of Teller chapter, went home

----------------------------------------------------------------------

Tue Jun 19 13:16:55 PDT 2012

Finished Teller chapter.

VisCog group meeting. Maria presented on pupillary reflex to lightness
illusions.

Maria M says she can install R, in some time frame. 

Run NJ on 2 contrasts x four spacings again.

downloaded new version of LyX.

downloaded "Pro Git" book and added to Calibre.

Uh, looks like I need to reinstall TeX and reconfigure LyX. This is
due to Homebrew taking over /usr/local and me deleting everything from there....

Installed TeX again. Can run LyX. Wrote my summary for Teller chapter
for JP class, sent to JP.

Met with mike to discuss evaluations.

Ugh. 

----------------------------------------------------------------------
Wed Jun 20 14:07:15 PDT 2012

Went to JP class.

NJ canceled on stimulus due to sickness.

Went to group therapy. Insight: what we are trying to do is not think
our way through problems (system 2) but change our attitudes so that
system 1 has a more desirable response to problems.

Bought new shoes at Recycled.

Went home.

----------------------------------------------------------------------
Wed Jun 20 23:25:20 PDT 2012

Emailed Kayla about JP course.

Three hours disappear, like every time I decide to go home.

Read more DT chapter.

Installed cleats on new cycling shoes.

Went ahead and wrote commentary for DT chapter and sent to JP.

----------------------------------------------------------------------
Thu Jun 21 12:52:12 PDT 2012

Making figures again.

generic function for making these figures turns out to be hard to wrap
head around.

Actually looking at it now, I like the x-intercept as a measure, and
hte sensitivity as well. I think the sensitivity measurement will
change a bit when I do the compensation from motion energy leakage.

Went home, sleep at a normal hour.

Mess around with grid in order to get figure captions working.

----------------------------------------------------------------------
Fri Jun 22 10:09:04 PDT 2012

Wake up earlyish.

Lab meeting was discussing Mark Churchland reaching / dynamics paper. 

Went to JP class.

Discuss with MS about promises for next comm meeting. Resolve to work
on some sketches of figures ASAP.

Ping everyone based on meeting times.

Get figure with captions working.

Send figure (bias/sensitivity) to advs. Hope it makes sense despite
some ugliness.

----------------------------------------------------------------------
Wed Jun 27 23:49:05 PDT 2012

Okay, it lqooks like I was working on a contrast series.

I should do that, then work up a number/density summary

then refine figures into more publication worthy graphs

then then

----------------------------------------------------------------------
Thu Jun 28 15:06:09 PDT 2012

finish up/tone down email to mike

need to check how often "monster effect" really happens




----------------------------------------------------------------------
Fri Jul 27 00:21:52 PDT 2012

well well. what have I done?

got a new iPad.

Have been looking at how to get ipad reading workflow working.

Started reading Spering and Gegenfurtner article on induced motion and
pursuit. This pointed me at Duncker (1929) which I tried to get a hold
of but it's in German (same problem as Wertheimer I suppose.) Also
pointed to the book A Natural History of Vision (Wade, 1998) which was
MIT press but isn't on CogNet. Not so worth tracking down.

Anyway the Spering and Gegenfurtner article is weird because the
target ad field start out having hte same motion alwyas, then are
purturbed. And I'm not ure whether the perceptual judgement of speed
is based on contrast, or on retinal movement (interestingly, it's not
based on eye velocity.)

Playing with Git a bit, going to try going git-svn to work locally on
my analysis scripts and psychophysics repository. I would like to
exploit Git to pare down a "distribution" of my motion demo and
perhals a separate distribution of my psychophysics library.

Disabled antialiasing in Emacs.

Messed up my dotfile Git repo today, so I should get it back from Time
Machine (and set up a server to push to.) (

----------------------------------------------------------------------
Sat Jul 28 11:05:07 PDT 2012

fixed dotfile repo.

Migrating my analysis working directory over to svn-git. Spent ages
trying to work out the right unix incantations to also transfer the
untracked files. ( https://gist.github.com/dd92eabc247a0e4f7807 )

----------------------------------------------------------------------
Wed Aug  1 15:27:00 PDT 2012

Talked to ione about draft figure captions. talked to ione and geoff
about modeling (it's pretty simple, I'm just flabbergasted by the
whole process of getting here.

Started writing an introduction/tutorial about Git.

Trying to debug some weird path issues. chmod is not working and bash
tab-completion is putting slashes at the end of filenames that don't
correspond to directories. Suspect Enthought Python 64bit package but
need to narrow down what's in the path from it.

Spent a few hours writing a stupid shell script that didn't work.

Then found out it was about the ~ in my path. Bash expands ~ but which
does not. Derp!

Now the task is to make my makefile work again.

Looks like something was hardcoded to my old directory! Good catch! It was the matlab dependencies script. I then need to recode it to use relative directories.

Also, include "dependencies" in this project, it seems.

Also, fuck! Go ahead and delete the "unused" files. After I fix the build.
