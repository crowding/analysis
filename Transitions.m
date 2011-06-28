function this = Transitions(varargin)

    %the transitions are kept in a struct of structs, this being the closest
    %thing matlab provides to an associative array.
    transitions = struct();
    latencies = struct();
    first_times = struct();
    
    persistent init__;
    this = autoobject(varargin{:});

    function reset()
        transitions = struct();
        latencies = struct();
        first_times = struct();
    end

    function register(list, times)
        %takes in (some number of lists of transitions) and counts them.
        
        if ~exist('times', 'var') || isempty(times)
            times = nan(size(list));
        end

        if ~isempty(list)
            last = list{1};
            last_time = times(1);
            if ~isfield(first_times, last)
                first_times.(last) = times(1);
            end
        end

        %cellfun(@register, list(2:end), num2cell(times(2:end)));
        cellfun(@register, list(2:end), n2c(times(2:end)));
        function c = n2c(n)
            %GAH. matlab's num2cell fucks up the sizing on empty arrays,
            %which wreaks problems with cellfun, above.
            c = reshape(num2cell(n), size(n));
        end
        
        function register(label, time)
            if ~isfield(first_times, label)
                first_times.(label) = time;
            end
            if ~isfield(transitions, last);
                transitions.(last) = struct();
                latencies.(last) = struct();
            end
            if ~isfield(transitions.(last), label)
                transitions.(last).(label) = 0;
                latencies.(last).(label) = 0;
            end
            latencies.(last).(label) = (latencies.(last).(label) * transitions.(last).(label) + last_time - time) / (transitions.(last).(label) + 1);
            transitions.(last).(label) = transitions.(last).(label) + 1;
            last = label;
            last_time = time;
        end
    end

    function [label_indices, matrix, mean_latency, first] = readout()
        %produce labels and a transition count matrix.
        fns = struct2cell(structfun(@fieldnames, transitions, 'UniformOutput', 0));
        labels = cat(1, fieldnames(transitions), fns{:});
        labels = unique(labels);
        empts = cell(size(labels));
        
        args = {labels{:}; empts{:}};
        label_indices = struct(args{:});
        %now we have a table translating field names to matrix indices.
        c = 0;
        function cc = countUp(x)
            c = c + 1;
            cc = c;
        end
        label_indices = structfun(@countUp, label_indices, 'UniformOutput', 0); 
        
        matrix = sparse(zeros(numel(labels)));
        mean_latency = sparse(numel(labels));
        for n1 = fieldnames(transitions)'
            for n2 = fieldnames(transitions.(n1{1}))'
                matrix(label_indices.(n1{1}), label_indices.(n2{1})) = transitions.(n1{1}).(n2{1});
                mean_latency(label_indices.(n1{1}), label_indices.(n2{1})) = latencies.(n1{1}).(n2{1});
            end
        end
        first = orderlike(label_indices, first_times);
    end

    function plot(ax)
        if ~exist('ax', 'var')
            ax = gca;
        else
            activateAxes(ax);
        end
        [label_indices, transition_matrix, mean_latency, first] = readout();
        labels = fieldnames(label_indices);
                
        %lay out the graph automatically.
        X = layout(transition_matrix, mean_latency, first, labels);
        
        %draw the graph. Somehow indicate direction and weight?
%        gplotdc(transition_matrix, X);
        gplotwl(transition_matrix, X, labels);
    end

    function X = layout(transition_matrix, mean_latency, first, labels)
        %begin by ordering by first appearance in Y, and by frequency of
        %appearance in X.
        X = [sum(transition_matrix, 2), cell2mat(struct2cell(first))];
        
        %compute by the average latencies...
%        X(2) = apply_latencies(X(2), transition_matrix, mean_latency);
        %linearize the steps...
        [tmp, i1] = sort(X(:,1));
        [tmp, i2] = sort(X(:,2));
        X(i1,1) = linspace(1, 0, size(X, 1));
        X(i2,2) = linspace(0, 1, size(X, 1));
        
        %X = random_graph_layout(transition_matrix);
%        X = gursoy_atun_layout(transition_matrix, 'topology', 'square');
        X = fruchterman_reingold_force_directed_layout(transition_matrix + transition_matrix', 'progressive', X, 'initial_temp', 4, 'iterations', 100);
%        X = kamada_kawai_spring_layout(transition_matrix + transition_matrix', 'progressive', X);

    end
end