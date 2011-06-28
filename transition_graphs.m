function transition_graphs(infile, outfile)
    figures = {};

    data = load(infile, 'data');
    data = data.data;

    %pull out all the trials.
    trials = cellfun(@(x)x.trials,data, 'UniformOutput', 0);
    trials = cat(1,trials{:});
    trials = structcat(trials);

    t = Transitions();
    
    %get the labels for each trial
    [labels, times] = arrayfun(@getLabels, trials, 'UniformOutput', 0);
    function [labels, times] = getLabels(trial)
        [labels, times] = arrayfun(@each, trial.triggers, 'UniformOutput', 0);
        function [label, time] = each(trigger)
            label = regexprep(trigger.name, '.*/', '');
            if isfield(trigger, 'triggerTime') && ~isempty(trigger.triggerTime)
                time = trigger.triggerTime;
            else
                time = trigger.next;
            end
        end
        times = cell2mat(times);
    end
    
    cellfun(t.register, labels, times);
    activateFigure(1); clf;
    t.plot(gca);
    
    sprintf_to_file(outfile, '%s\n', figures{:});
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