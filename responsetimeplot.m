function responsetimeplot(ax, trials)
    %select the trials that opposed local and global...
    trials = select(@(t)t.trial.extra.motionCondition == 3, trials);
    
    %group trials based on success and on correctness
    hold(ax, 'on');
%    [groups, grouped] = gridgroupfn(trials, @(t)t.result.success, @(t)t.result.correct);
    grouped = cell(2,2);
    grouped{1,1} = select(@(t)~t.result.success && ~t.result.correct, trials);
    grouped{1,2} = select(@(t)~t.result.success && t.result.correct, trials);
    grouped{2,1} = select(@(t)t.result.success && ~t.result.correct, trials);
    grouped{2,2} = select(@(t)t.result.success && t.result.correct, trials);
    
    plot(ax, arrayfun(@(t)t.trial.extra.nTargets, grouped{1,1}) + (-0.125 + 0.25 * rand(size(grouped{1,1}))), arrayfun(@(t)t.result.responseTime, grouped{1,1}), 'ko', 'MarkerSize', 3.5, 'Color', [0.5 0.5 0.5]);
    plot(ax, arrayfun(@(t)t.trial.extra.nTargets, grouped{1,2}) + (-0.125 + 0.25 * rand(size(grouped{1,2}))), arrayfun(@(t)t.result.responseTime, grouped{1,2}), 'k.', 'Color', [0.5 0.5 0.5]);
    plot(arrayfun(@(t)t.trial.extra.nTargets, grouped{end,1}) + (-0.125 + 0.25 * rand(size(grouped{end,1}))), arrayfun(@(t)t.result.responseTime, grouped{end,1}), 'ko', 'MarkerSize', 3.5);
    plot(arrayfun(@(t)t.trial.extra.nTargets, grouped{end,2}) + (-0.125 + 0.25 * rand(size(grouped{end,2}))), arrayfun(@(t)t.result.responseTime, grouped{end,2}), 'k.');
    %plot limits, if they exist.
    minResponse = min(arrayfun(@(t)t.trial.extra.minResponseTime, cat(1, grouped{:})));
    maxResponse = max(arrayfun(@(t)t.trial.extra.maxResponseTime, cat(1, grouped{:})));
    plot(xlim(), [minResponse minResponse], 'b:');
    plot(xlim(), [maxResponse maxResponse], 'r:');
    hold(ax, 'off');
    ylim([max(minResponse, 0.4) - 0.2, min(maxResponse, 1.0) + 0.2]);
end