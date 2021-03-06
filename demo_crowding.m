function this = demo_crowding(outfile, varargin)
    %Render demonstration stimuli for Movie 2 of the paper; shows
    %congruent, incongruent, and ambiguous, varying the number of targets
    %each time...

    my_ = Genitive;
    
    %start with a list of stimuli and captions...
    stimuli = cell2struct ... 
        ({ sprintf('Incongruent motion\n10 then 20 targets')...
         , CircularCauchyMotion( ...
              'dt', 0.1 ...
            , 'radius', 10 ...
            , 'dphase', -.5 / 10 ...
            , 'x', 0 ...
            , 'y', 0 ...
            , 'color', [0.5 0.5 0.5]' / sqrt(2) ...
            , 'velocity',     5 ... %velocity of peak spatial frequency
            , 'phase',        reshape((1:10)*2*pi/10, 1, []) ...
            , 'angle',        reshape((1:10)*360/10 + 90, 1, []) ...
            , 'wavelength', 0.75 ...
            , 'width', 0.5 ...
            , 'duration', 2/30 ...
            , 'order', 4 ...
            , 't',            zeros(1, 10)...
            , 'n', 4 ...
            ), 0 ...
         ; ' '...
         , CircularCauchyMotion( ...
              'dt', 0.1 ...
            , 'radius', 10 ...
            , 'dphase', -.5 / 10 ...
            , 'x', 0 ...
            , 'y', 0 ...
            , 'color', [0.5 0.5 0.5]' / sqrt(2) ...
            , 'velocity',     5 ... %velocity of peak spatial frequency
            , 'phase',        reshape((1:20)*2*pi/20, 1, []) ...
            , 'angle',        reshape((1:20)*360/20 + 90, 1, []) ...
            , 'wavelength', 0.75 ...
            , 'width', 0.5 ...
            , 'duration', 2/30 ...
            , 'order', 4 ...
            , 't',            zeros(1, 20)...
            , 'n', 4 ...
            ), 0 ...
         ; sprintf('With occluder\n10 then 20 targets')...
         , CircularCauchyMotion( ...
              'dt', 0.1 ...
            , 'radius', 10 ...
            , 'dphase', -.5 / 10 ...
            , 'x', 0 ...
            , 'y', 0 ...
            , 'color', [0.5 0.5 0.5]' / sqrt(2) ...
            , 'velocity',     5 ... %velocity of peak spatial frequency
            , 'phase',        reshape((1:10)*2*pi/10, 1, []) ...
            , 'angle',        reshape((1:10)*360/10 + 90, 1, []) ...
            , 'wavelength', 0.75 ...
            , 'width', 0.5 ...
            , 'duration', 2/30 ...
            , 'order', 4 ...
            , 't',            zeros(1, 10)...
            , 'n', 4 ...
            ), 1 ...
         ; ' '...
         , CircularCauchyMotion( ...
              'dt', 0.1 ...
            , 'radius', 10 ...
            , 'dphase', -.5 / 10 ...
            , 'x', 0 ...
            , 'y', 0 ...
            , 'color', [0.5 0.5 0.5]' / sqrt(2) ...
            , 'velocity',     5 ... %velocity of peak spatial frequency
            , 'phase',        reshape((1:20)*2*pi/20, 1, []) ...
            , 'angle',        reshape((1:20)*360/20 + 90, 1, []) ...
            , 'wavelength', 0.75 ...
            , 'width', 0.5 ...
            , 'duration', 2/30 ...
            , 'order', 4 ...
            , 't',            zeros(1, 20)...
            , 'n', 4 ...
            ), 1 ...
            }, {'caption', 'stimulus', 'occluderVisible'}, 2);
    
    fixation = FilledDisk('radius', 0.15);
    occluder = FilledAnnularSector(...
                  'color', [0.475 0.475 0.475]*255 ...
                , 'loc', [0;0] ...
                , 'startAngle', 4*pi/12 ...
                , 'arcAngle', 16*pi/12 ...
                , 'innerRadius', 80/27 - 1 ...
                , 'outerRadius', 12 ...
                )

    %how long to display captions for.
    captionDisplayDuration = 0.6;
    captionDisplayDurationPerCharacter = 0.07;
    interCaptionInterval = 0.5; 
    
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
        , 'avirect', [0 0 512 512] ...
        , 'rect', [0 0 512 512]...
        , 'cal', Calibration('interval', 1/60, 'distance', 180/pi, 'spacing', [24/512, 24/512], 'rect', [0 0 512 512]) ...
        , 'priority', 0 ...
        );
    end
    
    function params = run(params)
        caption = Text('centered', 1);
        sprites = CauchySpritePlayer('process', stimuli(1).stimulus);
        
        trigger = Trigger();
        
        main = mainLoop...
            ( 'graphics', {sprites, caption, fixation, occluder}...
            , 'input', {params.input.keyboard}...
            , 'triggers', {trigger}...
            );
        
        playIndex = 1;
        
        trigger.singleshot(atLeast('refresh', 0), @showCaption);
        trigger.panic(keyIsDown('ESCAPE'), @main.stop);
        
        params = main.go(params);
        
        function showCaption(s)
            %show the caption...
            fixation.setVisible(0);
            caption.setText(stimuli(playIndex).caption);
            caption.setVisible(1);
            
            sprites.setVisible(0);
            sprites.setProcess(stimuli(playIndex).stimulus);
                        
            trigger.singleshot...
                ( atLeast('next',  s.next + captionDisplayDuration ...
                    + captionDisplayDurationPerCharacter.*numel(stimuli(playIndex).caption))...
                , @hideCaption);
        end
            
        function hideCaption(s)
            caption.setVisible(0);
            fixation.setVisible(1);
            occluder.setVisible(stimuli(playIndex).occluderVisible);
            trigger.singleshot(atLeast('next', s.next + interCaptionInterval), @startDemo);
            
            %but we actually start the demo here....
            sprites.setVisible(1, s.next + interCaptionInterval);
            
            trigger.singleshot...
                ( atLeast...
                    ( 'next'...
                    , s.next + (stimuli(playIndex).stimulus.getN() + 1)...
                    .* stimuli(playIndex).stimulus.getDt() + interCaptionInterval ...
                    )...
                , @finishDemo);
        end
        
        function startDemo(s) %#ok
            fixation.setVisible(1);
        end
        
        function finishDemo(s)
            playIndex = playIndex + 1;
            
            if (playIndex > numel(stimuli))
                trigger.singleshot(atLeast('next', s.next + interCaptionInterval), main.stop);
            else
                trigger.singleshot(atLeast('next', s.next + interCaptionInterval), @showCaption);
            end
        end
    end
    
end