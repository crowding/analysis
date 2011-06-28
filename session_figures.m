function session_figures(infile, outfile)
    figures = {};

    data = load(infile, 'data');
    data = data.data;
   
    %in each control experiment the radius varies (axis 1) and something else
    %varies(axis 2)... first, find out what the something else is. That is
    %the thing that takes several distinct values under the quest...
    %the first parameter column is all the parameters bundled up with each
    %quest.
    rand = data{1}.beforeRun.trials.randomizers{1};
    
    %which ones vary? (skip the last one because it is nTargets and that
    %always varies)
    values = repmat({{}}, 1, numel(rand.subs)-1);
    for i = 1:numel(rand.subs)-1
        for j = cellfun(@(x)x(i), rand.values(:)')
            nextj = 0;
            for k = values{i}(:)'
                if isequal(j, k{1})
                    nextj = 1;
                end
            end
            if nextj
                continue
            end
            values{i}{end+1} = j;
        end
    end
    
    sub = Subscripter();
    its = Genitive();
    %damn, that actually worked on the first try!
    %the subsc are: (the first is radius, there should be two
    axis_subs = rand.subs(cellfun('prodofsize', values) > 1);
        
    %for whetever reason, the subs need to be de-celled.
    axis_subs = cellfun(@matify, axis_subs, 'UniformOutput', 0);
    function x = matify(x)
        %this should be a one liner, why is there not a constuct like
        %cond()...
        if iscell(x)
            x = cell2mat(x);
        end
    end

    %fake a second dimension if there was only one...
    if numel(axis_subs) == 1
        axis_subs{2} = its.extra.tf;
    end
        
    %the functions to pull out those axes from each trial are:
    axis_accessors = cellfun(@(axis_sub)@(t)subsref(t.trial, axis_sub), axis_subs, 'UniformOutput', 0);
    
    %first, pull out all the trials in a glob and compute what needs to be
    %computed. New fields are 'correctResponse', 'motionCondition',
    %'minResponseTime', 'maxResponseTime', 'responseTime.'
    trials = cellfun(@(x)x.trials,data, 'UniformOutput', 0);
    trials = cat(1,trials{:});
    trials = structcat(trials);
    trials = select(@(t)strcmp(t.trial.version__.function, 'ConcentricTrial') && ~isempty(t.result) && isfield(t.result, 'response'), trials);
    
    trials = assign(trials, its.trial.extra.correctResponse, @(t)-t.trial.extra.globalDirection - t.trial.extra.localDirection*~t.trial.extra.globalDirection);
    trials = assign(trials, its.result.correct, @(t)t.trial.extra.correctResponse == t.result.response);
    %conditions: 0 = local only, 1 = global only, 2 = local and global
    %agreeing, 3 = local and global disagreeing
    trials = assign(trials, its.trial.extra.motionCondition, ...
        @(t)sub{[2 1 3;0 NaN 0; 3 1 2]}(t.trial.extra.globalDirection+2, t.trial.extra.localDirection+2));
    trials = assign(trials, its.result.responseTime, ...
        @(t)triggerTimes('cw', t, 1, 'first') - triggerTimes('startMotion', t, 1, 'first') - t.trial.motion.process.t ...
        );
    trials = assign(trials, its.trial.extra.minResponseTime, ...
        @(t)t.trial.awaitInput - t.trial.motion.process.t);
    trials = assign(trials, its.trial.extra.spacing, @(t)t.trial.extra.r *2*pi / t.trial.extra.nTargets);
    trials = assign(trials, its.trial.extra.maxResponseTime, @maxResponseTime);
    function rt = maxResponseTime(t)
        if isfield(t.trial, 'maxResponseLatency')
            mrl = t.trial.maxResponseLatency;
        else
            mrl = Inf;
        end
        rt = t.trial.extra.minResponseTime + mrl;
    end
    
    %select only the trials the got a proper response...
    trials = select(@(t)~isempty(t.result.responseTime), trials);

    %now we can group the trials using the first two axxessible axes... (this may not
    %work for occlusion..)
    [groups, grouped] = gridgroupfn(trials, axis_accessors{1:2});
    
    %now do a response time plot of the data....
    nr = size(groups, 1);
    a1 = substruct2str(axis_subs{1});
    nc = size(groups, 2);
    a2 = substruct2str(axis_subs{2});
    
    activateFigure(1); clf;
    ax = zeros(size(groups));
    iterate({1 2}, @rtplot, groups, grouped);
    function rtplot(ix, params, trials)
        ax(ix(1),ix(2)) = subplot(nr, nc, sub2ind([nc nr], ix(2), ix(1))); %MATLAB stupidity-- column major matrix indexing with row major subplot indexing...
        responsetimeplot(ax(ix(1),ix(2)), trials);
        %label the graphs...
        title(ax(ix(1), ix(2)), sprintf('%s = %g, %s = %g', a1, params(1), a2, params(2)));
    end
    matchgraphs(ax(:));
    print('-depsc',[outfile '.fig1.eps']);
    figures{end+1} = [outfile '.fig1.eps'];

    activateFigure(2); clf;
    ax = zeros(size(groups));
    iterate({1 2}, @seqplot, groups, grouped);
    function seqplot(ix, params, trials)
        ax(ix(1),ix(2)) = subplot(nr, nc, sub2ind([nc nr], ix(2), ix(1))); %MATLAB stupidity-- column major matrix indexing with row major subplot indexing...
        sequenceplot(ax(ix(1),ix(2)), trials);
        %label the graphs...
        title(ax(ix(1), ix(2)), sprintf('%s = %g, %s = %g', a1, params(1), a2, params(2)));
    end
    matchgraphs(ax(:));
    print('-depsc',[outfile '.fig2.eps']);
    figures{end+1} = [outfile '.fig2.eps'];

    %reselect only the successful (within time limits) trials, and fit
    %psychometric functions.
    trials = select(@(t)t.result.success, trials);
    activateFigure(3); clf;
    [groups, grouped] = gridgroupfn(trials, axis_accessors{1:2});
    fits = iterate({1 2}, @fitPfun, groups, grouped);
    
    %save the figure and the fit calculations.
    print('-depsc',[outfile '.fig3.eps']);
    figures{end+1} = [outfile '.fig3.eps'];
    save([outfile '.fits.mat'], 'fits');
    figures{end+1} = [outfile '.fits.mat'];

    sprintf_to_file(outfile, '%s\n', figures{:});
end

function fit = fitPfun(index, group, trials)
    [inputs, groups] = gridgroupfn(trials, @(t)t.trial.extra.spacing);
    inputs = cell2mat(inputs);
    nYes = cellfun(@(g)sum(arrayfun(@(t)t.result.correct,g)), groups);
    nNo = cellfun(@(g)sum(arrayfun(@(t)~t.result.correct,g)), groups);
    [regression.a, regression.b, regression.thresh50] = FitLogitYN(inputs, nYes, nNo);
    regression.mu = -regression.b / regression.a;
    regression.s = log(10) / regression.a; %i use a slope and mean rather than a/b parameters in my logit, so convert this
    %But I'm just using that as a starting point for a maximum
    %likelihood fit.
%    function nl = negloglikelihood(data, cdf)
        
%    end
%    [ml.a, ml.b, ml.thresh50] = (fit.inputs, fit.nYes, 
    
    %start with a generic fit for logistic, which is NOT the maximum
    %likelihood fit.
    fit = collectWorkspace({'trials', 'groups'});
end


function p = logisticCDF(x, mu, s)
    p = 1 ./ (1 + exp(-(x - mu) .* s));
end

function st = collectWorkspace(exceptions)
    %{
    s = evalin('caller', 'whos()');
    for var = s(:)'
        if ~v.global && var.nesting.level == 0
            st.(var.name) = evalin('caller', var.name);
        end
    end
    %}
    
    %alternate method
    s = functions(evalin('caller', '@()[]'));
    st = s.workspace{end};
    st = rmfield(st, exceptions);
end
    
function sprintf_to_file(filename, varargin)
    try
        require(openFile(filename, 'w'), @(p)fprintf(p.fid,varargin{:}));
    catch e
        if exist(filename, 'file')
            system(sprintf('rm "%s"', file));
        end
        rethrow(e);
    end
end