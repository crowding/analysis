function motion_energy(infile, outfile)

%
% create space-time example stimulus plots of all the stimuli.
%

%
% Also start on a motion-energy sort of calculation, for each type of trial
% used in the experiment....
%

%
% We're going to Use some of our OLD graphics code for the Cauchy wavelet.
%

% First, we load a list of unique trial parameters (sorted out for us by R
% code)

load('unique_stimuli.mat', 'stimuli')

% while that loaded OK, it has illegal field names (oops!). Fix them...
colnames = fieldnames(stimuli);
stimuli = fixfieldnames(stimuli);

stimuli = soa2aos(stimuli);

%now, to a motion-energy calculation for each stimulus.
stimuli = arrayfun(@motioncalc, stimuli);

%and the output, back to R
stimuli = aos2soa(stimuli);

save(outfile, 'stimuli');

end