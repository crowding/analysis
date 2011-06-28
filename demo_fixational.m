function demo_fixational(outfile, varargin)
    %demos that kitaoka's thing is not purely (or even substantially)
    %driven by retinal slip. Do this by moving the display
    %in-the-manner-of-eye-movements (and we do that by replaying one of my
    %recorded eye movements)
    
    %I should also play with the 
    
    %how long to display captions for.
    
    
    my_ = Genitive();
    
    persistent init__;
    this = autoobject();
    
    playDemo(this, 'aviout', outfile, varargin{:});
    %playDemo(this, varargin{:});
    
    function params = getParams() 
        params = struct...
        ( 'edfname',    '' ...
        , 'dummy',      1  ...
        , 'skipFrames', 0  ...
        , 'preferences', struct('skipSyncTests', 1, 'TextAntiAliasing', 1 ) ...
        , 'requireCalibration', 0 ...
        , 'hideCursor', 0 ...
        , 'aviout', '' ...
        , 'avistart', 2 ...
        , 'avirect', [0 0 256 256] ...
        , 'rect', [0 0 256 256]...
        , 'cal', Calibration('interval', 1/60, 'distance', 180/pi, 'spacing', [10/256, 10/256], 'rect', [0 0 256 256]) ...
        , 'priority', 0 ...
        );
    end
    
    function params = run(params)
        %how many frames to render
        s = load('sample_trace.mat');
        trace = s.trace;
        
        %lowpass filter it at the refresh rate
        [B,A] = butter(3, mean(diff(trace(3,:)))*30, 'low');
        filtered = filtfilt(B, A, trace([1 2],:)')';
        trace([1 2],:) = filtered;
            
        fixation = FilledDisk('radius', 0.2, 'loc', @lookupLoc);
        
        trigger = Trigger();
        
        main = mainLoop...
            ( 'graphics', {fixation}... %, occluder}...
            , 'input', {params.input.keyboard}...
            , 'triggers', {trigger}...
            );
        
        trigger.panic(keyIsDown('ESCAPE'), @main.stop);
        trigger.singleshot(atLeast('refresh', 0), @start);
        
        params = main.go(params);
        
        function start(k)
            fixation.setVisible(1, k.next);
            trigger.singleshot(atLeast('next', k.next + trace(3,end)), main.stop);
        end
        
        function loc = lookupLoc(time)
            loc = interp1(trace(3,:)', trace([1 2],:)', time)';
        end
    end
    
end