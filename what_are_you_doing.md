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

Looks like something was hardcoded to my old directory! Good catch! It
was the matlab dependencies script. I then need to recode it to use
relative directories.

Also, include "dependencies" in this project, it seems.

Also, fuck! Go ahead and delete the "unused" files. After I fix the build.

----------------------------------------------------------------------
Thu Aug  2 16:41:14 PDT 2012

Now I will create Ione's data files. They will exclude the trials
which don't make trials.

Also, let's rename "makemake" to "monk" and its input "Monkfile" and
its output "Monkfile.makefile" to make the typing easier.

Gah, it's trying to remake every-damn-thing.

Eliminate circular dependencies for all the matlab files. (good thing
I normalized the relative paths...)

But it also wants to re-import all the database files. What is
marked new that isn't? Guess I should re-install "remake" and seem

Should I make a Homebrew formula for it? Did, it was straightforward
enough.

Decided to go ahead and let it rebuild the database. Except it didn't
re-build the database? Fucking make, now it's showing actions in -n
mode that it isn't performing in real mode!

Turn out that was due to some problem in my options(error). WHen
non-interactive it should call exit().

But it still makes numdensity series calculations over and over again,
why?

TODO: make Monk automatically handle dependencies of the Makefile
(esp. when using @-includes in Monk)

Prepared CSV export of individual trials. Next, rename "contrast" to
something better.

----------------------------------------------------------------------
Fri Aug  3 15:34:56 PDT 2012

Okay, now let's rename "contrast" to "content." Done.

Now let's take a look at those CSV files and send them to Ione. Done.

Now let's make Ione's plotting code run as part of the system.

And let's see what's up with the pse/bias figures.

----------------------------------------------------------------------
Tue Aug  7 15:04:32 PDT 2012

Had to write an entire CSV parser to get [fucking CSV files into matlab.](http://abandonmatlab.wordpress.com/2012/08/07/matlab-cant-read-plain-text-data-out-of-a-wet-paper-bag/)

Now that I can have that, I can reimplement Ione's script.  Made it
run under Makefile, then fixed it to use read_csv. Done.

Installed a Matlab mode for Emacs. No debugger support though, but
syntax highlighting and indentation is nice.

Now I want to find out what is up with psychometric functions that
have positive PSE but negative bias...? I think the best way to start
is to plot these values on a graph for each function I fit.

stack item: But first let's do ten or so steps on the Git Immersion website.

stack item: push forward on either number/density data or
motion-energy calculation.

I think that preparing a publication quality FIgure 4 is higher
priority priority than a Figures 1 or 2. I am just so frustrated that
every time I sit down to try and work through what will be my thesis:
	* I recapitulate all of Figure 1 and 2 in a walkthrough
	* the other person, after working through their stuckness, decides
      Figures 1 and 2 must have been all that I've done so far, and
      proceeds planning my future on that basis
	* we never even fucking talk about number/density or eye movements

Though I guess I can work on Figure 2, particularly the measurement
illustration...

First, I am going to try setting up my 25-minutely Growl notification
and EMacs-buffer-maximization.

Might as well make it a Gist folder...

And I have gotten it mostly working.

Now starting in plotting each psychometric function fit. Plotted
points, now plotting the fitted line. Have to consider what happens
when using folded, unfolded, or whatnot data. todo: test this with
average_bias data.

----------------------------------------------------------------------
Thu Aug  9 21:07:13 PDT 2012

I have noticed that ESS autocompletion does not work well at all in
browser() or debugger() situations.

How do I do multiple layers with different datasets in ggplot? I want
both horizontal and vertical geom\_pointranges on the same plot. Is
there a way to do that? I think the easy way out is just plot a point
and a line. Ah well.

Also, my R is outdated, but whatever.

I think about what to write for the final essay in JP's vision
class. I think I will write about a summation-threshold experiment for
visual motion with two cues. I'll have to think about the kinds of
mechanisms that might motivate the experiment though. That I can also
present as a 'crazy idea' later on (I have another crazy idea about
channel mapping, etc.)

Hold on, I don't necessarily want to plot pointranges, I might want to
plot intervals.

Had a silly idea about how to reduce the repitition in ggplot
constructions. `with_arg` which i sketched in `helper_functions.R`.

But I'm not sure it helps yet...

Ah, it looks like I'm not getting the right intercept! Let me work on
this graph some more and make it better.

Now I've got myself stuck on how units are specified in ggplot. The
pointrange and geom_point are plotting their points at different
sizes.

`with_arg` to the rescue! That made it much less tadious to compute
pointrange type things.

----------------------------------------------------------------------
Fri Aug 10 04:06:33 PDT 2012

I wish there was a way to easily indicate a range. A rect with open
sides would do.

Placing labels on my illustrative graph, need to guess which side to
put the labels for best whitespace.

Then work on why the intercept measurement is wrong. And the
threshold, while I'm at it, since that is another intercept
measurement.

Then need to sketch out FIg. 2. Next plot is the "illustrative
psychometric function." I want two, one for large spacing and one for
small spacing.  And they will be plotted with un-flipped data.

Let's animate this before packing on more labels.

----------------------------------------------------------------------
Sun Aug 12 23:42:54 PDT 2012

Okay, looking at the first graph where the sign problem exists, I find
there's a large coefficient for is_folded. That might be related to
the problem.

Let's add curves for the separate values if is_folded... Ah, yes, the
"xint" is tracking only one of the folded values.

So fix that, and add a new column to my CSV files. The x-intercept
calculation will reflect an average between the two cases.

Slight problem: why is "trials_i" getting pulled back from the db with
extra quotes? Maybe because it isn't actually the right column
name. Yup.

Looks like I also need to check the intercepts for `loaded_from`. Once
in the `data_functions.r` and another time in the
`graphics_functions.R`.

----------------------------------------------------------------------
Mon Aug 13 15:38:50 PDT 2012

Still having the problem where options("error") resets itself when I
load my code. Annoying.  

Let's think about setting up flymake or flyparse for R. Later.

I'm working on the "averaging over multiple sessions" problem.  And
being a perfectionist. How it will work is, when I average over
something, I'll make predictions then average those predictions. Dunno
how good a technique that is for intercepts, but intercepts are a
fucked measurement anyway.

I really need to write my essay for JP class though.

Okay. I think I fixed intercept-finding for the multiple-sessions and
bias cases.

Need to think more about how to rebuild conditinal on a script's
dependencies changing. This is currently completely disabled and I
have to manually clean some things before they will rebuild.

I notice that some of my super-high sensitivities look like bad
fits. Because of the too-low asymptote? That's interesting.

----------------------------------------------------------------------
Tue Aug 14 16:28:13 PDT 2012

Time to write for JP class.

Also time to captions and make mockups of figures.

----------------------------------------------------------------------
