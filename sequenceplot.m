function sequenceplot(ax, trials)
    %plot the sequence of what happened in each QUEST.
    %select the trials that opposed local and global...
    trials = select(@(t)t.trial.extra.motionCondition == 3, trials);
    enum = [1:numel(trials)]';
    
    %group trials based on success and on correctness
    hold(ax, 'on');
%    [groups, grouped] = gridgroupfn(trials, @(t)t.result.success, @(t)t.result.correct);
    grouped = cell(2,2); seq = cell(2,2);
    [grouped{1,1}, seq{1,1}] = select(@(t,s)~t.result.success && ~t.result.correct, trials, enum);
    [grouped{1,2}, seq{1,2}] = select(@(t,s)~t.result.success && t.result.correct, trials, enum);
    [grouped{2,1}, seq{2,1}] = select(@(t,s)t.result.success && ~t.result.correct, trials, enum);
    [grouped{2,2}, seq{2,2}] = select(@(t,s)t.result.success && t.result.correct, trials, enum);
    
    plot(ax, seq{1,1}, arrayfun(@(t)t.trial.extra.nTargets, grouped{1,1}), 'ko', 'MarkerSize', 3.5, 'Color', [0.5 0.5 0.5]);
    plot(ax, seq{1,2}, arrayfun(@(t)t.trial.extra.nTargets, grouped{1,2}), 'k.', 'Color', [0.5 0.5 0.5]);
    plot(ax, seq{2,1}, arrayfun(@(t)t.trial.extra.nTargets, grouped{2,1}), 'ko', 'MarkerSize', 3.5);
    plot(ax, seq{2,2}, arrayfun(@(t)t.trial.extra.nTargets, grouped{2,2}), 'k.');

    hold(ax, 'off');
end