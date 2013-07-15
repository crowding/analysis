function this = demo_counter(outfile, varargin)
    %render the looping demo for Movie 1 of the paper
    %(counterrotating wheels)

    %based on ConcentricDemo with the adjustabiilty ripped out.
    n = 5;

    motion = CircularCauchyMotion();
    sprites = CauchySpritePlayer('process', motion);
    fixation = FilledDisk([-15 15;0 0], 0.2, 0, 'visible', 1);

    ambiguous_ = 0;
    color_ = 0;

    my_ = Genitive();

    persistent init__;
    this = autoobject...
            ( my_.n,                   5 ...  %two small locally opposed wheels
            , my_.motion.dt,           0.1 ...
            , my_.motion.radius,       3 ...
            , my_.motion.dphase,       .5 / 3 ...
            , my_.motion.x,            repmat([-10 10], 1, 5) ...
            , my_.motion.y,            0 ...
            , my_.motion.color,        [0.5;0.5;0.5]/sqrt(2) ...
              ... %velocity  of peak spatial frequency
            , my_.motion.velocity,     repmat([-5 5], 1, 5) ...
            , my_.motion.phase,        reshape([1;1]*(1:5)*2*pi/5, 1, []) ...
            , my_.motion.angle,        reshape([1;1]*(1:5)*360/5 + 90, 1, []) ...
            , my_.motion.wavelength,   0.75 ...
            , my_.motion.width,        0.5 ...
            , my_.motion.duration,     2/30 ...
            , my_.motion.order,        4 ...
            , my_.motion.t,            zeros(1, 10)...
            , my_.fixation.loc, [-10 10; 0 0] ...
            );

    %twiddle things so it makes a loop...
    distribute();

    playDemo(this, 'aviout', outfile, varargin{:});

    function params = getParams()
        params = struct...
        ( 'edfname',    '' ...
        , 'dummy',      1  ...
        , 'skipFrames', 0  ...
        , 'preferences', struct('skipSyncTests', 1) ...
        , 'requireCalibration', 0 ...
        , 'hideCursor', 0 ...
        , 'aviout', '' ...
        , 'avistart', 2 ...
        , 'avirect', [0 0 640 256] ...
        , 'rect', [0 0 640 256]...
        , 'cal', Calibration( 'interval', 1/60 ...
                            , 'distance', 180/pi...
                            , 'spacing', [40/640, 40/640] ...
                            , 'rect', [0 0 640 256]) ...
        , 'priority', 0 ...
        );
    end

    function distribute()
        %if two wheels...
        if size(fixation.getLoc(), 2) > 1
            this.property__...
                ( my_.motion.x ...
                , [ repmat(fixation.property__(my_.loc(1,[1])), 1, n) ...
                  , repmat(fixation.property__(my_.loc(1,[2])), 1, n)]...
                , my_.motion.y ...
                , [repmat(fixation.property__(my_.loc(2,[1])), 1, n) ...
                  , repmat(fixation.property__(my_.loc(2,[2])), 1, n)] ...
                , my_.motion.velocity ... %velocity of peak spatial frequency
                , [ repmat(this.property__(my_.motion.velocity(1)), 1, n) ...
                  , -repmat(this.property__(my_.motion.velocity(1)), 1, n)] ...
                , my_.motion.phase,        repmat((1:n)*2*pi/n, 1, 2) ...
                , my_.motion.angle,        repmat((1:n)*360/n + 90, 1, 2) ...
                , my_.motion.t,            zeros(1,2*n) ...
                );
        else
            this.property__...
                ( my_.motion.x ...
                , ( repmat(fixation.property__(my_.loc(1)), 1, n) ) ...
                , my_.motion.y ...
                , ( repmat(fixation.property__(my_.loc(2)), 1, n) ) ...
                , my_.motion.velocity ...
                  ... %velocity of peak spatial frequency
                  , ( repmat(this.property__(my_.motion.velocity(1)), 1, n) ) ...
                , my_.motion.phase,        (1:n)*2*pi/n ...
                , my_.motion.angle,        (1:n)*360/n + 90 ...
                , my_.motion.t,            zeros(1,n) ...
                );
        end

        %if ambiguous...
        if ambiguous_
            %clear Screen;
            this.property__...
                ( my_.motion.x, repmat(motion.getX(), 1, 2)...
                , my_.motion.y, repmat(motion.getY(), 1, 2)...
                , my_.motion.velocity, [motion.getVelocity(), - motion.getVelocity()]...
                , my_.motion.phase, repmat(motion.getPhase(), 1, 2) ...
                , my_.motion.angle, repmat(motion.getAngle(), 1, 2) ...
                , my_.motion.t, repmat(motion.getT(), 1, 2)...
                )
        end

        if color_
            if ambiguous_
                c = repmat([0.5 0.5 0 0;0 0 0.3 0.3;0.5 0.5 0.00 0.00]/ sqrt(2), 1, ceil(n/2));
                c = c(:,1:2*n);
            else
                c = repmat([0.5 0;0 0.5;0.25 0.25]/ sqrt(2), 1, ceil(n/2));
                c = c(:,1:n);
            end
            if size(fixation.getLoc(), 2) > 1
                c = [c c];
            end
            motion.setColor(c);
        else
            motion.setColor([0.5;0.5;0.5]/(sqrt(2)^(ambiguous_+1)));
        end
    end

    %twiddle the number of frames and the delta-x slightly so that
    %everything lines up after making a full revolution.
    function frames = makeLoop(params)
        steps = round(2*pi / motion.getDphase());
        frames = round(motion.getDt() * steps / params.cal.interval);

        motion.setDphase(2*pi/steps);
        motion.setDt(frames * params.cal.interval / (steps));
    end

    function params = run(params)
        %how many frames to render
        frames = makeLoop(params);

        ttd = transformToDegrees(params.cal);
        keyboardInput = params.input.keyboard;

        trigger = Trigger();
        keyboard = KeyDown();

        main = mainLoop ...
            ( 'graphics', {sprites, fixation} ...
            , 'triggers', {trigger, keyboard} ...
            , 'input', {keyboardInput} ...
            );

        trigger.singleshot(atLeast('refresh', 0), @start);

        motion.reset();

        function start(h)
            sprites.setVisible(1, h.next);
            trigger.singleshot(atLeast('refresh', h.refresh + frames), main.stop);
        end

        params = main.go(params);
    end
end