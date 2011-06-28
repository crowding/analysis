% Given some experimental data for an experiment which is just fixation,
% let's fake up an eye trace that has no blinks for demonstration purposes.
%
function varargout = findEyeTrace(varargin)
    if ischar(varargin{1})
        s = load(varargin{1});
        trace = findEyeTrace(s.data, 20);
        save(varargin{2}, 'trace');
    else
        varargout{1:nargout} = findEyeTraceInner(varargin{:});
    end
end

function [x, ticks, ticksi] = findEyeTraceInner(data, length)

    % Doh! MATLAB's cell2mat "Cannot support cell arrays containing cell
    % arrays." 
    % trials = cell2mat(cellfun(@(d)d.trials, data, 'UniformOutput', 0));

    cell_array_containing_cell_arrays_of_trials = cellfun(@(d)d.trials(:), data, 'UniformOutput', 0);
    trials = cat(1, cell_array_containing_cell_arrays_of_trials{:});
    
    lengths = cellfun( @(t)numel(t.eyeData(2,:))...
                     , trials ...
                     , 'ErrorHandler', @(a,b)NaN);
                 
    nnans = cellfun( @(t)sum(isnan(t.eyeData(2,:))) ...
                   , trials, 'ErrorHandler' ...
                   , @(a,b)NaN);
               
    index = flipud(sortrows([lengths nnans (1:numel(lengths))']));
    index = index(index(:,2) == 0, :);
    
    x = zeros(3, 0);
    i = 0;
    ticks = []; ticksi = [];
    while i < size(index, 1) && (size(x, 2) == 0 || x(3,end) - x(3,1) < length)
        i = i + 1;
        ed = trials{index(i,3)}.eyeData;
        ed = ed(:,2:end-1);
        if size(x, 2) >= 1
            %splice it onto the end of the trace...
            ed = bsxfun(@plus, ed, -ed(:,1) + 2*x(:,end) - x(:,end-1));
            [x(:,end-4:end) ed(:,1:5)]
            x = [x ed];
        else
            x = ed(:,2:end-1);
            x(3,:) = x(3,:) - x(3,1);
        end
        ticks(i) = x(3,end);
        ticksi(i) = size(x, 2);
    end
    
    for i = 1:2
        adj = linspace(x(i,1), x(i,end), size(x,2)+1);
        x(i,:) = x(i,:) - adj(1:end-1);
    end
    
    plot(x(3,:), x(1,:), 'r-', x(3,:), x(2,:), 'b-');
    set(gca, 'XTick', ticks);
end