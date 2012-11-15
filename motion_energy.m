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

if ~exist('infile', 'var')
    infile = 'unique_stimuli.mat';
end

if ~exist('outfile', 'var')
    outfile = 'motion_energy.mat';
end

load(infile, 'data_without_phase');

data = dataset(data_without_phase);

%since there are so many, we will attempt to be lazy about the
%construction of the things. Or not.
%if exist(outfile, 'file')
%    old = load(outfile, '-struct');
%end

n = size(data, 1)
for i = 1:size(data, 1)
    if mod(i,50) == 1
        printf('%d/%d\n', i, n);
    end
    out = motion_energy_calc(data(i,:));
    data(i,get(out, 'VarNames')) = out;
end

%make a struct. I don't have dataset2struct in this one.
s = struct();
for n = (get(data, 'VarNames'))
    s.(n{1}) = data.(n{1});
end

data = s;

save(outfile, 'data');
