#.INCLUDE_DIRS += $(dir $(lastword $(MAKEFILE_LIST)))

#include gmd

ifndef NETCAT
NETCAT=nc
endif

#$(call MATLAB,line) in a rule feeds a command to matlab. 
#$(shell $(call MATLAB,line)) is used in a variable expansion.

#this used echo -e at some point for some reason? -e is not supported
#in BSD echo and no escape sequences are used, so...
#MATLAB = printf "cd('%s'); %s\n\n" '$(PWD)' '$(subst ','\'',$(1))' | $(NETCAT) $${MATLAB_HOST} $${MATLAB_PORT}

MATLAB=./runmatlab "$(1)"
R = /usr/bin/env Rscript $(1)

#this variable keeps track of automatically-generated targets.

ifndef GENERATED_TARGETS
GENERATED_TARGETS :=
endif

#this variable pools all targets made by the toolkit.
ifndef ALL_TARGETS
ALL_TARGETS := 
endif

ifndef INTERMEDIATES
INTERMEDIATES := 
endif

define make_intermediate
INTERMEDIATES += $(1)
endef

MOVE = $(addprefix $(2)/,$(notdir $(1)))

PWD = $(shell pwd)

#How to calculate and also cache matlab function dependencies.

# Call MATLAB_DEPS to calculate the dependencies for a matlab command. It will
# automatically update dependencies. Because the dependency tree needs to be
# recalculated if the dependencies of a function change, the makefiles depend
# on the dependency calculations. 
# or not: not for right now at least.
define MATLAB_DEPS
endef

# define MATLAB_DEPS
# $(if $(realpath $(1).dep), $(shell cat $(1).dep) ) $(1).dep
# $(if $(findstring $(1).dep,$(GENERATED_TARGETS)),,$(eval $(call MAKE_MATLAB_DEPS_RULE,$(1))))
# endef

define MAKE_MATLAB_DEPS_RULE
$(1).dep: $(if $(realpath $(1).dep), $(shell cat $(1).dep) )
	$(call MATLAB,dependencies('$(1)','$(1).dep'))
GENERATED_TARGETS += $(1).dep
$(call ADD_MAKEFILE_TOUCH_DEPENDENCY,$(1).dep)
endef

define ADD_MAKEFILE_TOUCH_DEPENDENCY
$(foreach MF,$(MAKEFILE_LIST),$(if $(findstring $(MF),$(GENERATED_TARGETS)),,$(call MAKEFILE_TOU#H_RULE,$(MF))))
$(call EVALINFO,
$(strip $(MAKEFILE_LIST)): $(1) #.ALWAYS

)
endef

define R_DEPS
endef

#define R_DEPS
#$(if $(realpath $(1).dep), $(shell cat $(1).dep) ) $(1).dep $(if $(findstring $(1).dep,$(GENERATED_TARGETS)),,$(eval $(call MAKE_R_DEPS_RULE,$(1))))
#


#I haven't yet found out how to compute R dependencies yet, so this is a placeholder.
define MAKE_R_DEPS_RULE
$(1).dep: $(if $(realpath $(1).dep), $(shell cat $(1).dep) )
#	echo $(1) > $(1).dep
	echo "" > $(1).dep
GENERATED_TARGETS += $(1).dep
$(call ADD_MAKEFILE_TOUCH_DEPENDENCY,$(1).dep)
endef

define MAKEFILE_TOUCH_RULE
$(call EVALINFO,
GENERATED_TARGETS += $(1)

$(1):
	touch $$@
)
endef

#TRANSFORM_EACH macro is used when each file in one directory gets transformed
#to a mile in another directory. It is used like this:
#
#$(eval TRANSFORM_EACH,session_trials,mat,session_graphs,DONE,MATLAB,make_session_graphs)
#
#The above statement transforms all files in $(session_trials_files), located
#in directory session_trials and having the suffix 'mat', to files unter
#session_graphs having the suffix "done", using the matlab function
#"make_session_graphs"
define TRANSFORM_EACH
$(3)_files = $$(patsubst %.$(2),%.$(4),$$(call MOVE,$$(mat_files),$(3)))
$(3)_dir = $3
ALL_TARGETS += $$($(3)_files)
$(3)/%.$(4): $$($(1)_dir)/%.$(2) $(call $(5)_DEPS,$(6))
	mkdir -p $(3)
	$$(call $(5),$(6) $$<S#( $$@)
endef

define TRANSFORM_EACH_WILDCARD
$(call EVALINFO,
ALL_TARGETS += $$(patsubst $(1),$(2),$$(filter $(1),$$(ALL_TARGETS)))

$(2): $(1) $$(call $(3)_DEPS,$(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$< $$@)
)
endef

#Example usage:
#$(eval $(call TRANSFORM_ALL_WILDCARD,marked/%.mat,good_trials.mat,MATLAB,good_trials))
#                                     $(1)         $(2)            $(3)   $(4)
define TRANSFORM_ALL_WILDCARD
ALL_TARGETS += $(2)
$(2): $$(filter $(1),$$(ALL_TARGETS)) $$($(3)_DEPS $(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$^ $$@)
endef

#example usage
#$(eval $(call TRANSFORM_SINGLE,metrics.mat,scatterplots.DONE,MATLAB,makeScatterplots))
#                               $(1)        $(2)              $(3)   $(4)
define TRANSFORM_SINGLE
ALL_TARGETS += $(2)
$(2): $(1) $$($(3)_DEPS $(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$< $$@)
endef

#TRANSFORM_ALL_LISTED takes a filename or names, where those files consist of
#lists of files, files, and transforms all the files listed in aggregate.
#
#example usage:
#$(eval $(call TRANSFORM_ALL_LISTED,good_sessions.txt,scatterplots.DONE,MATLAB,figure1))
#Note, if the listed file is updated, we need to refresh the makefile so as to re-read it.
define TRANSFORM_ALL_LISTED
$(call EVALINFO,
ALL_TARGETS += $(2)

$(2): $(1) $(shell cat $(1)) $(call $(3)_DEPS,$(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$(shell cat $(1)) $$@)

$(call ADD_MAKEFILE_TOUCH_DEPENDENCY,$(1))
)
endef


#if you have a collection of source files, each specifying a different aggregation, here you go.
#example:
#$(call AGGREGATE_EACH_WILDCARD_LISTED,pools/%.sources,pools/%.Rdata,R,aggregate.Rscript)
define AGGREGATE_EACH_WILDCARD_LISTED
$(foreach SOURCE,$(filter $(1),$(ALL_TARGETS)),$(call TRANSFORM_ALL_LISTED,$(SOURCE),$(patsubst $(1),$(2),$(SOURCE)),$(3),$(4)))
endef

#TRANSFORM_EACH_WILDCARD_LISTING_TARGETS takes a filename and transforms each, much
#like TRANSFORM_EACH_WILDCARD, but the output is a file that lists new targets
#produced. For example, if make_session_figures reads the file in its first
#argument, makes figures from it, and writes the list of figure files out to the
#file listed in the second argument:
# $(eval $(call TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS,sessions/%.mat,session_figures/%.figures,MATLAB,make_session_figures))
define TRANSFORM_EACH_WILDCARD_LISTING_PRODUCTS
$(eval SOURCES := $(filter $(1),$(ALL_TARGETS)))
$(eval FILE_LISTS := $(patsubst $(1),$(2),$(SOURCES)))
ALL_TARGETS += $(FILE_LISTS)
$(2): $(1) $$(call $(3)_DEPS,$(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$< $$@)
$(foreach SOURCE,$(SOURCES),$(call MAKE_PRODUCT_LIST,$(patsubst $(1),$(2),$(SOURCE)) $(if $(realpath $(patsubst $(1),$(2),$(SOURCE))),$(shell cat $(patsubst $(1),$(2),$(SOURCE)))),$(SOURCE),$(3),$(4)))
$(foreach FILE_LIST,$(FILE_LISTS),$(if $(realpath $(FILE_LIST)), $(call ADD_PRODUCT_LISTS, $(FILE_LIST))))
endef

define MAKE_PRODUCT_LIST
$(1): $(2) $$(call $(3)_DEPS,$(4))
	mkdir -p $$(dir $$@)
	$$(call $(3),$(4) $$< $$@)

endef

define ADD_PRODUCT_LISTS
ALL_TARGETS += $(shell cat $(1))
$(call ADD_MAKEFILE_TOUCH_DEPENDENCY,$(1))
endef

define EXCLUDE_LISTED
$(call EVALINFO,
ALL_TARGETS := $$(filter-out $(shell cat $(1)), $$(ALL_TARGETS))
$(ADD_MAKEFILE_TOUCH_DEPENDENCY $(1))
)
endef

define EVALINFO
$(eval $(1))
endef

define MAKE_INTERMEDIATE
$(call EVALINFO,.INTERMEDIATE: $(filter $(1),$(ALL_TARGETS))

)
$(call EVALINFO,ALL_TARGETS := $(filter-out $(1),ALL_TARGETS)

)
endef

define grep-string
$(strip                                         \
  $(foreach w, $2,                               \
    $(if $(findstring $1, $w),                   \
      $w)))
endef

define igrep-string
$(strip                                         \
  $(foreach w, $2,                               \
    $(if $(findstring $1, $w),                   \
      ,$w)))
endef

.INTERMEDIATE: $(INTERMEDIATES)