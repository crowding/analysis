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
%construction of the things, and only recalculate new entries.
%or not...

if exist(outfile, 'file')
    old = load(outfile);
    old = dataset(old.data);
    data.index_ = (1:size(data, 1))';
    %keep any trials which exactly match. But why do multiple trials match now?

    %Oh of course it's a NaN matching issue.
    old.target_number_shown(isnan(old.target_number_shown)) = -1;
    data.target_number_shown(isnan(data.target_number_shown)) = -1;

    keep = join(data, old, 'Type', 'Inner', 'MergeKeys', true);
    if size(keep, 1) > size(data, 1)
        error('wrong!');
    end
    data = data(setdiff((1:size(data, 1)), keep.index_),:);
    data.index_ = [];
    keep.index_ = [];
end

n = size(data, 1)
for i = 1:size(data, 1)
    if mod(i-1,50) == 0
        printf('%d/%d\n', i-1, n);
    end
    row = data(i,:);
    if (row.target_number_shown == -1)
        row.target_number_shown = NaN;
    end
    out = motion_energy_calc(row);

    %double check that my calculation is symmetric
    % flipped = row;
    % [flipped.content_cw, flipped.content_ccw] = ...
    %     deal(row.content_ccw, row.content_cw); >
    % flipped.abs_displacement = -row.abs_displacement;
    % flipped.abs_direction_content = -row.abs_direction_content;
    % flopped = motion_energy_calc(flipped);
    % cat(1, out, flopped);

    data(i,get(out, 'VarNames')) = out;
end

if exist(outfile, 'file')
    if size(data, 1) == 0
        data = keep;
    else
        data = cat(1, data, keep);
    end
    data.target_number_shown(data.target_number_shown == -1) = NaN;
end

%make a struct. I don't have dataset2struct in this version of matlab.
s = struct();
for n = (get(data, 'VarNames'))
    s.(n{1}) = data.(n{1});
end

data = s;

save(outfile, 'data', '-v6');
