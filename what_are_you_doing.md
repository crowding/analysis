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

Oh fucking fuck it.

----------------------------------------------------------------------
Wed Aug 15 14:52:00 PDT 2012

Opened up Lyx and started formatting figure captions.

I decided to put placeholders in tables, then I'll play with a trial
of OmniGraphSketcher. If I'm ging to plan out these graphs for
critique then I downloaded a copy of OmniGraph Sketcher. Interesting
app -- you use it to draw out things that _look_ like graphs, and then
only optionally attach data to them. There is an iPad app as well.

TODO: I think that my "what are you doing" app needs to hold off on
firing until there have been ten seconds of keyboard inactivity and
two seconds of mouse inactivity, or something.

TODO: I want something that periodically reminds me of any code
repositories on my system that:
    * have files modified but not checked in
	* are ahead or behind of their upstreams
	* don't have an upstream

----------------------------------------------------------------------
Thu Aug 16 13:22:45 PDT 2012

Finish up some prelim figure captions and layouts. By 'layout' i mean
i imaginge the components of hte figure in my head and make a table in
the LyX document.

Going to sketch some figures. First import what I can from
already-done figures, my cheatsheet, demo movies, etc.

TODO: I wonder if I can auto generate PDFs from
omnigraffles/omnigraphsketches. Wait, I'm just going to replace them
with autogenerated content anyway. Never mind.

Mocking up some things in OmniGraphSketcher. This program is pretty
limited, and doesn't export to omnigraffle even?

Put everything relevant to JP class in its repo.

Oh, right, time to finish setting up remote git repos.
Made upstream for dotfiles on Pat.
And ConcentricDirectionQuest (I'm also still pushing to SVN on that one)
And psy424, which was then removed from my disk.
I wonder if there is a good way to push to my 
time machine (cross platform, w/o mounting?) Git over AFP?

----------------------------------------------------------------------
Fri Aug 17 18:36:48 PDT 2012

More graph sketching.

Started writing email to mike & ione but wanted to sketch more graphs first.


----------------------------------------------------------------------
Mon Aug 20 21:51:14 PDT 2012

Sent some sketches to mike and ione. met with ione. seemd to go pretty
well. Going to play with models, but first let myself go through the
git immersion course [http://gitimmersion.com]

Have dispensation to play with models and fucking relax and get work
done.

----------------------------------------------------------------------
Tue Aug 21 16:49:00 PDT 2012

Finished Git immersion course.

Thinking seriously about cranking open Seven Languages in Seven Days.

First, going to look into setting up a Git web hosting deal, via
'gitolite'

Why is (pbm-remind-log-entry) not working after a reboot? Argh.

----------------------------------------------------------------------
Wed Aug 22 18:07:12 PDT 2012

(pbm-remind-log-entry) still not working

Looking at gitweb instructions.

Got new monitor. And new keyboard and trackpad.

Setting up gitweb is confusing. I retreated to trying to set it up on
my local laptop.

----------------------------------------------------------------------
Thu Aug 23 09:20:42 PDT 2012

Need to set up dotfiles, etc. Then the interweb. Trying to merge
dotfiles between my two environments. Here we go.

I can use git --cherry-pick to selectively merge commits between
branches? (is there a better way?)

Okay, now trying to merge the website repo.

I might want to have a pre-commit and post-checkout stuff to track
permissions, if I'm going to track dotfiles and web directories. I
should check if there's already scripts for that.

Got gitweb cgi to run on Shadlen lab. I suspect that my home directory
is not visible from the web server at all. Running some CGI
experiments to test.... actually it is visible, it's git that's
missing. Totally different /usr/local.

----------------------------------------------------------------------
Fri Aug 24 19:06:23 PDT 2012

I want jist to automatically list and checkout my gists for me. OR
some command line tool that can list gists?

Need to remind myself to give up in the git web interface, my
requirements are too weird.

Huh, rebooting emacs solved my reminder issue. Score one for today.

Now I'm messing with emacs. I want a "remote open" functionality.

Never mind that for now. I was editing the gitolite.rc, wasn't I?

Ooh, git comes in colors now.

Enabled creation and deletion under "classes". I think you can only
create and delete under a subdirectory.

Exporting a bunch of existing repos to Git. (Git's compression should
take care of early stuff)

Interesting problem: when I ssh from Pat to Pat supposedly using the
Git key, my .bashrc is evaluated.

Went ahead and migrated the rest of repos.

TODO: want to filter out early "analysis" and "data" from the
psychophysics code project. And probably fix up the external
Psychtoolbox.

As for gitweb, I think that the way to do it is to actually modify
gitweb to use a git-shell instead of git directly. Ah, here's the
problem: gitolite-shell only does authentication and pack sending and
receiving. All the repo browsers use a lot more than git-shell
provides. And git command itself isn't careful about security. I think
this means I give up for now.

Here's my next yak-shave project though: check all repos on my
computer, see it they are out of date, see if things have been
committed, and nag me via some appropriate channel (growl could work.)

matlab-mode doesn't invoke the editor because matlab tries to "which
emacsclient -n". Also, HTML links don't work, but I'm not sure what
that has to do with anything.

Now cleaning up Geoff's code in the process of reading it.  For
example, shorten structure-catting, and fix tile.m so that it actually
works on multiple monitors. Holy shit, I forgot how screwed the figure
placement deal is in MATLAB, so I started a rant on the subject.

Fixed Geoff's tile.m. Sort of.

Fixing up MeilstrupBoyntonModel so that it makes sense.

----------------------------------------------------------------------
Sun Aug 26 13:38:16 PDT 2012

Gee, this Geoff code has a lot of 12-years-ago-isms in it . MATLAB has
improved a bit since then with struct field references and function
handles, and those are easy to fix up.

Try setting up KeePass across my devices. Dropbox seems more natural
for this; since the keepass file is encrypted, there is no way for Git
to do merging.

I want to build this into the Makefile, now, and then translate it
into R.

Monk (my makefile generator) is giving a weird error. I want to add
tracing to it but I can't fire up ipython from Emacs.

As, the problem was that it was looking for ipython in the Emacs
variable exec-path, rather than path. So another munge to init.

Fixing up the makefile process.

somehow writing a one'liner to answer the toast/stoat question???? At
least I put it in gist.

Fixing it up so it will run on other machines, whee. This takes a lot.

Fixing up UnpackPeter so that it will work with more "series" types.

Made the build work.

Had the thought that a file-content-based build system would alleviate
a lot of my headaches around building.

----------------------------------------------------------------------
Monday.

My auto-remind script wasn't working. I did work on modeling, setting
up a drop box to share modeling code, cleaning up the code further,
etc.

----------------------------------------------------------------------
Tuesday.

Auto-remind script still isn't working.

Want to write a "splat" for matlab to hlep with fitting (as Geoff's
code is still weirdly dependent on argument order.)

I managed to extricate the early repository and git info from
There's probably some more stuff I can get rid of.

Let's put notes about the data into a separate file.

I started writing something like "splat" but then realized it was well
nigh impossible.

I'm looking at fit.m and how it is used. I think I get it now, the
'freeList' pulls out of 'params' and the "fitting function" is
expected to take arguments of the form (params, datacolumn,
datacolumn, datacolumn, datacolumn) the backwards of how I'd do it.

So let's wrap up the positional arguments of the motion model in a
struct, so that it's more amenable to changes in the future.

Also let's get the appropriate fields into eccentrcity_series
and numdensity_series.

And let's get some plotting parameters going here. That is,
plotting for each subject what's going on.

----------------------------------------------------------------------
Wed Aug 29 15:42:10 PDT 2012

trying to set up long-lines mode. White-space mode is supposed to do
that but hasn't, yet. got that working after a reboot of Emacs.

Okay, got whitespace mode working appropriately.

Now I want to fix my logging reminder. After a reboot I find that the
logging reminder is not working.

I still don't have a good command line Gist client. One that well let
me list. Maybe an automatic sybchronization would do the trick.

So, let me visualize what's going on with Geoff's model.

And work on an R, translation, and work on a dataset translation.

First pull out the full fitted parameters for all the parameters that
were fit.

Then have a function to illustrate that model.

I think what is really needed is a function to illustrate the model
deviance, as well as way to capture the variance of the model fit.

God I hate Matlab. Un

----------------------------------------------------------------------
Thu Aug 30 20:14:58 PDT 2012

somehow started altering getversion to get info from git.

Try ing to compute residuals, again, via creating an approximation to
ddply.

Wrote a ddply implementation. Now making it into a graph.

Ate lunch. Poking through flymake.el.

Looking at this pandas example. Probably going to translate to R.

Debugging my multiple output groupfun. cellfun responds "dataset tyep
is not currently implemented" when run on a cell array of datasets,
wtf?

Computing residuals. Now I need to finish making the plot, and maybe
make it not split over so many arguments.

Wired up my old speakers and wired airport express to them.

----------------------------------------------------------------------
Sat Sep  1 10:55:25 PDT 2012

The goal is to have a framework to compare models. Actually the goal
is to compare models< and that wants a framework. I need to keep
in my mind which are the real goals :)

Fixing things to that the needed flip for modeling happens in
unpack. That way residuals don't have to repicate the flip.

Found a page with the regression diagnostics for binary data:
http://data.princeton.edu/wws509/notes/c3s8.html

Poking at Raspberry Pi website, with the ide if building a
Super-Airport-Express, with jackd, etc. And two more of those little
amplifiers, and a couple more USB audio cards.  Actually, come to
think if it, RPi would give me a little Gnu/Linux box to mess with my
Edgeport devices, wouldn't it. I've got an HDMI monitor already,
too. So that's high on my list.

Okay, so I want to calculate the chi-square-based deviance over a
residual. That means somehow per-observation, for observations that
change predicted mu. So the normal formula doesn't work.

Someone on stats.stackoverflow.com
[says](http://stats.stackexchange.com/questions/1432/what-do-the-residuals-in-a-logistic-regression-mean),
"I find the binnedplot function in the R package arm gives a very
helpful plot of residuals. It's described nicely on p.97-101 of Gelman
and Hill 2007." Luckily I have this book. Ah yes, sum the per-point
deviances across constatnt size bins.

----------------------------------------------------------------------
Mon Sep  3 14:18:16 PDT 2012

Yesterday, succeeded at a Pearson residual plot binned over dx.

Today, making a model fit under all conditions; moving logfile export
out of the home directory.

Also should do a main data plot.

Probably also doing a residual deviance plot.

matlab can't find "edit" any more. It's not on its $PATH. Okay, guess
I'll specify the full file. Where does MATLAB get its $PATH, anyway?

Except I pulled up the "preferences" screen and /Users/peter/bin/edit
was already the editor. I chose "apply" and it worked.?? Ah, it was
getting set by my init.el.

Opentoline works, clicking on links doesn't. Ah, need to remap that
to mouse-1, it is mouse-2 for some reason.

Now it clicks but it doesn't find the right files in the matlab path.

opentoline gets function names rather than file names and probably
won't work with partial paths.

----------------------------------------------------------------------
Wed Sep  5 15:41:47 PDT 2012

Wrote a function to fit all models. Looks like some models do not fit
well, so i should rewrite the model in terms of slope rather than
sigma.

Now I have to build some accumarray shit to count cases for plotting,
because groupfun is interminably slow for that purpose, spending all
sorts of time in dataset.subsref. In R I can fit and plot the whole
shebang WITH BOOTSTRAPPING faster than this nonsense.

FUCK MATLAB

MATLAB HOLDS BACK SCIENCE BY YEARS

Ahem. SO I spent a bunch of time solving specifically the problem of
counting yesses and noes per stimulus condition. FML.

----------------------------------------------------------------------
Thu Sep  6 15:31:27 PDT 2012

God, trying to recreate lattice plot from scratch, Have I mentioned
Fuck MATLAB lately?

Now making predictions.

FINALLY got the model predictions plotted on the graph. Some of the
predictions are pretty bad, need to rehash the model to fit in terms
of "slope" rather than "sigma".

So with all fits in hand, let's look at diagnostics.

----------------------------------------------------------------------
Fri Sep  7 23:22:32 PDT 2012

Some of the models fit a shift in slopes when they should be fitting a
change in sigma. Other models do the opposite. Should rehash them in
terms of a slope (i.e. turn them into a pure GLM model.)

Met with Mike, showed some of hte curve fits.

----------------------------------------------------------------------
Sat Sep  8 11:37:03 PDT 2012

Switched to R branch to catch up. Got import working, but didn't do
much else.

----------------------------------------------------------------------
Sun Sep  9 12:35:34 PDT 2012

Gonna get more R code working.

First switched to Matlab branch to make it build and make the figure
plots. Remembered that I have to do `'fullpath'` in
`fullfile(fileparts(mfilename('fullpath')))` to get paths relative to
the m-file.

Fucking neighbor is running the table saw cutting up firewood all
day. What the hell do they even do with firewood anyway.

----------------------------------------------------------------------
Mon Sep 10 11:09:10 PDT 2012

I think what is needed is an object oriented approach. A 'fit' object
should have a "fit" method that casts it around data, a 'predict'
method that shows the fit to other data, and an initialValues method
(used by the Fit method, so it needn't be another interface.)

Took a time out to put my "auto-raising if long" prompt up on a gist.

Neighbor still cutting up wood all day! What the hell.

Doing this OOP development in a separate branch.

Timeout to fix matlab.el so I can click on error references.

This is going to improve things. Although now I need to select
Predictions.

Now looking up fogures for my "crazy idea" dealie.

----------------------------------------------------------------------
Fri Sep 14 18:16:24 PDT 2012

Had this disabled for a time.

Tues. was aggravated at meeting with Ione and being told to just accept Geoff's
model without even thinking about or investigate it. I'm the one going
to be writing a lot more words about this than Geoff is. If I can't
intellectually engage with it I can't write about it.

On Weds mostly fucked off.

On Thurs. contributed a speed patch to plyr, which made me feel some
good feelings for a bit.

Friday I'm modeling, and having some success at it.

----------------------------------------------------------------------
Mon Sep 17 13:05:58 PDT 2012

Now I'm going to get knitr working and update my methods section; also
poke at modeling some more to get the bias term working.

Poked at R environments, attributs on environmentsa are set by
reference so no help there.

Trying to craft an argument that my preferred way of looking at the
problem and fitting the model is idempotent to the "cue combination"
way of looking at it. And sketched until I found that there was no
difference!

Some success with modeling. Think there needs to be something other
than a linear dependence on direction content. How about a log
dependence? After all, we usually find that long-coherence is the best
way to make responses to random dots look like a psychometric
function.

The problem with fitting JB appears to be something to do with
adaptation to the direction content used in the session. There's also
some wacky nonmonotonicity.

----------------------------------------------------------------------
Tue Sep 18 16:12:07 PDT 2012

Getting a handle on some nonmonotonicities in the data. The degree of
induced motion is a nonmonotonic function of direction content?

Trying to collect all my writing into repositories.
