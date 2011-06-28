.INCLUDE_DIRS += ..
include ../toolkit.make

#default to making all

.PHONY: all .ALWAYS

.DEFAULT_GOAL = all

LOG_DIR = ../../data
TXT_DIR = ../../data

#start with the raw data files
ALL_TARGETS := $(wildcard $(LOG_DIR)/*-ConcentricDirectionQuest*.log.gz)
ALL_TARGETS := $(wildcard $(LOG_DIR)/*-ConcentricDirectionConstant*.log.gz)
ALL_TARGETS += $(wildcard $(LOG_DIR)/*-ConcentricDirectionQuest*.txt.gz)
ALL_TARGETS += $(wildcard $(LOG_DIR)/*-ConcentricDirectionConstant*.txt.gz)

#convert each data file into matlab format
$(eval $(call TRANSFORM_EACH_WILDCARD,$(LOG_DIR)/%.log.gz,mat/%.mat,MATLAB,log2mat))

#don't dother with some files (broken or incomplete sessions)
$(eval $(call EXCLUDE_LISTED, exclusions.txt))

# draw a simple graph of the state transitions for each file, maybe.
# $(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,mat/%.mat,sessions/%.transitions.files,MATLAB,transition_graphs))

# process the quest data for each session, resulting for each session in a list
# of figures produced.
# $(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,mat/%.mat,sessions/%.files,MATLAB,session_figures))

#translate matlab files to R files, unpacking them into a format easier to work with.
$(eval $(call TRANSFORM_EACH_WILDCARD,mat/%.mat,mat6/%.mat,MATLAB,tom6))

$(eval $(call TRANSFORM_EACH_WILDCARD,mat6/%.mat,RData/%.RData,R,mtor.r))

$(eval $(call TRANSFORM_EACH_WILDCARD,RData/%.RData,unpacked/%.RData,R,unpack.R))

## $(eval $(call TRANSFORM_EACH_WILDCARD,unpacked/%.RData,session_figures/%.pdf,R,session_graphs.R))

#aggregate the individual sessions into specified blocks. Each
#.sources file in the pools directory gives the source files for a
#pool Also pool all constant data into a big file.
ALL_TARGETS += $(wildcard pools/*.sources)
ALL_TARGETS += pools/All_constant.sources
pools/All_constant.sources: $(filter-out pools/All_constant.sources,$(wildcard pools/*_constant.sources))
	cat $^ > $@

$(eval $(call AGGREGATE_EACH_WILDCARD_LISTED,pools/%.sources,pools/%.Rdata,R,aggregate.R))

#make aggregate rawdata plots for the aggregate sessions.
## $(eval $(call TRANSFORM_EACH_WILDCARD,pools/%.Rdata,pools/%.pdf,R,session_graphs.R))

#and start playing with fitting the psychometric functions.
## $(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,pools/%.Rdata,pools/%_fits.products,R,psychfits.R))

## Okay, now that I've got a handle in this, Cook through the
## method-of-constant-stimulus data, creating some files AS WELL AS
## FIGURE 1
## FIGURE 1a is constant/manually produced
## FIGURE 1B here:
figure_1/B.eps: $(call MATLAB_DEPS, illustrated_stimuli.m)
	$(call MATLAB, illustrated_stimuli)


## FIGURE 1c and 1d here:
##$(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,pools/All_constant.Rdata,pools/All_constant.products,R,constant.R))

$(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,pools/SF.Rdata,pools/SF.products,R,variations.R))
$(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,pools/TF.Rdata,pools/TF.products,R,variations.R))
$(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,pools/DX.Rdata,pools/DX.products,R,variations.R))

## FIGURE 2:
##Need to maek example stimuli for occluders. These are just constants though.


### MOTION ENERGY ANALYSIS

## scan through all the pools and find all stimuli used in all
## trials. Analyze their motion energies, and make example figures of
## them.
ALL_TARGETS += unique_stimuli.sources
$(eval $(call TRANSFORM_ALL_LISTED,unique_stimuli.sources,unique_stimuli.mat,R,unique.stimuli.R))

ALL_TARGETS += motion_energy.mat
motion_energy.mat: unique_stimuli.mat $(call MATLAB_DEPS,motion_energy)
	$(call MATLAB, motion_energy $< $@)


$(eval $(call TRANSFORM_ALL_LISTED,unique_stimuli.sources,unique_tableaux.mat,R,unique.tableaux.R))

ALL_TARGETS += tableaux.mat
tableaux.mat: unique_tableaux.mat $(call MATLAB_DEPS,tableaux)
	$(call MATLAB, tableaux $< $@)


ALL_TARGETS += grid_figure.eps
grid_figure.eps: tableaux.mat $(call MATLAB_DEPS,grid_figure)
	$(call MATLAB, grid_figure $< $@)

### EYE MOVEMENT ANALYSIS

#patch together a sample eye trace...
sample_trace.mat: mat/dt-2009-05-21__10-37-15-ConcentricDirectionQuestTF.mat $(call MATLAB_DEPS,findEyeTrace)
	$(call MATLAB, findEyeTrace $< $@)


ALL_TARGETS += sample_trace.mat

#Rules to render intermediate AVIs and convert AVIs to MOV.  Note that
#sometimes the conversion fails with "an invalid track was found in
#the movie."  in that case, perform the conversion manually, touch the
#file (as its POSIX date doesn't get set right by Quicktime Player?)
#and remove the intermediate avi.
%.mov: %.avi %.settings animation.scpt
	echo $(info making because of $?)
	osascript animation.scpt $(abspath $<) $(abspath $@) $(abspath $(basename $<).settings)
	touch $@

%.avi: %.m %.dep #should work out as an intermediate target...
	$(call MATLAB, $(basename $<) $(basename $<).avi)

%.settings: animation.settings
	cp $< $@

.SECONDEXPANSION:
%.dep: %.m $$(if $$(realpath %.dep),$$(shell cat %.dep))
	$(call MATLAB, dependencies('$(basename $<)','$(basename $<).dep'))

.PRECIOUS: %.dep %.settings %.avi

#demo_counter.avi: $(call MATLAB_DEPS,demo_counter)
#	$(call MATLAB,demo_counter $@)
#
#demo_stimuli.avi: $(call MATLAB_DEPS,demo_stimuli)
#	$(call MATLAB,demo_stimuli $@)
#
#.INTERMEDIATE: demo_counter.avi demo_stimuli.avi

#list of movies to make.
#ALL_TARGETS += demo_counter.mov demo_stimuli.mov demo_crowding.mov demo_fixational.mov demo_kitaoka.mov

all: $(ALL_TARGETS)

#$(call __BP_SET,$(ALL_TARGETS))