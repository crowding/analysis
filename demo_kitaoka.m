function demo_kitaoka(outfile, varargin)
    %demos that kitaoka's thing is not purely (or even substantially)
    %driven by retinal slip. Do this by moving the display
    %in-the-manner-of-eye-movements (and we do that by replaying one of my
    %recorded eye movements)
    
    my_ = Genitive();
    
    persistent init__;
    this = autoobject();
    
    if exist('outfile', 'var')
        playDemo(this, 'aviout', outfile, varargin{:});
    else
        playDemo(this, varargin{:});
    end
    
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
        , 'avirect', [0 0 512 512] ...
        , 'rect', [0 0 512 512]...
        , 'cal', Calibration('interval', 1/60, 'distance', 180/pi, 'spacing', [40/512, 40/512], 'rect', [0 0 512 512]) ...
        , 'priority', 0 ...
        );
    end
    
    function params = run(params)
        %how many frames to render
        s = load('sample_trace.mat');
        trace = s.trace;
        
        %lowpass filter the trace at 1/2 the refresh rate
        [B,A] = butter(3, mean(diff(trace(3,:)))*60, 'low');
        filtered = filtfilt(B, A, trace([1 2],:)')';
        trace([1 2],:) = filtered;
            
        fixationFore = FilledDisk('visible', 1, 'radius', 0.4, 'loc', [0;0]);
        fixationAft = FilledDisk('visible', 1, 'radius', 0.6, 'loc', [0;0], 'color', [255;255;255]);
        snakes = sprites('images', {'rotsnake.png'}, 'loc', @lookupLoc, 'scale', 30/512, 'antialias', 1, 'visible', 0);
        
        trigger = Trigger();
        
        main = mainLoop...
            ( 'graphics', {snakes, fixationAft, fixationFore}... %, occluder}...
            , 'input', {params.input.keyboard}...
            , 'triggers', {trigger}...
            );
        
        trigger.panic(keyIsDown('ESCAPE'), @main.stop);
        trigger.singleshot(atLeast('refresh', 0), @start);
        
        params = main.go(params);
        
        onset_ = 0;
        function start(k)
            onset_ = k.next;
            snakes.setVisible(1);
            trigger.singleshot(atLeast('next', k.next + trace(3,end)), main.stop);
        end
        
        function loc = lookupLoc(s)
            loc = interp1(trace(3,:)', trace([1 2],:)', s - onset_)';
        end
    end
    
end