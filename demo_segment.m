function this = demo_segment(outfile, varargin)
    %render a demo  showing four example stimuli of the "segment" variety.

    %we start with the actual experiment we have configured
    e_ = ConcentricDirectionSegment('subject', 'zzz');

    %borrow its graphics objects....
    fixation = e_.trials.base.fixation;
    fixation.setVisible(1)
    motion = e_.trials.base.motion;
    process = e_.trials.base.motion.process;
    caption = Text('centered', 1, 'text', 'hello world', ...
                   'visible', 0, 'loc', [0 -1], ...
                   'points', 36, 'font', 'Myriad Pro');
    e_.trials.base.extra.r = 10;

    %remove its blocking...
    e_.trials.startTrial = [];
    e_.trials.endTrial = [];
    e_.trials.endBlockTrial = [];
    e_.trials.blockTrial = [];
    e_.trials.requireSuccess = 0;

    %and configure four specific stimuli to be used in sequence.
    %'spacing', 2*pi ./([9 12 15 18 21 25]) ...
    % 'nTargets', 3:8 ...

    r_ = e_.trials.randomizers(1);

    %select nTargets/nVisibleTargets equal to these why all this
    %rigamarole? because the
    %min_distance/max/distance/flanker-spacing is precomputed?
    select = [12 3; 12 5; 21 5; 21 8];
    selectable = cell2mat([r_.values{:}]');
    [is,selected] = ismember(selectable(:,[1 2]), select, 'rows');
    is = find(is);
    [~,order] = sort(selected(is,:));
    values_ = r_.values(is(order));

    %we'll just show the four things in order.
    e_.trials.replace(r_.subs,values_, 1, 1);
    %with only clockwise motion on the left side.

    %let's make the number nice and big.


    %the other thing to choose is the displacement and the direction content.

    %configure the experiment just to show our four stimuli. We use a
    %local displacement of -0.2 and a direction content of 0.4
    e_.trials.remove({'extra.globalDirection', 'extra.localDirection'});
    e_.trials.base.extra.localDirection = -1;
    e_.trials.base.extra.globalDirection = 1;
    e_.trials.remove('extra.globalVScalar');
    e_.trials.base.extra.globalVScalar = 0.2 / (20/3) / 0.1;
    e_.trials.base.extra.directionContrast = .4;
    e_.trials.remove('extra.side')
    e_.trials.base.extra.side='left';

    %what displacement and personalization to use? Look over it, but I pick...

    %and start the demo.
    persistent init__;
    this = autoobject();
    if exist('outfile', 'var')
        playDemo(this, 'aviout', outfile, varargin{:});
    else
        playDemo(this, varargin{:})
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
        , 'cal', Calibration('interval', 1/60 ...
                            , 'distance', 180/pi ...
                            , 'spacing', [24/512, 24/512]...
                            , 'rect', [0 0 512 512]) ...
        , 'priority', 0 ...
        );
        params.inputUsed = {'keyboard'};
    end

    function params = run(params)
        trigger = Trigger();

        trialNumber = 0;

        main = mainLoop ...
            ( 'graphics', {fixation, motion, caption} ...
            , 'triggers', {trigger} ...
            , 'input', {params.input.keyboard} ...
            );

        trigger.singleshot(atLeast('refresh', 0), @next);
        fixation.setVisible(1)

        x = [];

        function next(h)
            motion.setVisible(0);
            if ~isempty(x)
                e_.trials.result(x,struct('success',1));
            end

            x = e_.trials.next(params);
            if isempty(x)
                main.stop()
                return
            end

            trialNumber = trialNumber + 1;
            ext = x.getExtra();
            caption.setText(sprintf('%d', trialNumber));
            caption.setVisible(1);
            trigger.singleshot(atLeast('next', h.next + 3), @show);
        end

        function show(h)
            caption.setVisible(0);
            motion.setVisible(1, h.next);
            stopTime =  h.next + process.getT() + ...
                (process.getN() + 1) * process.getDt() + 1;
            trigger.singleshot(atLeast('next', stopTime), @next);
        end

        params = main.go(params);
    end
end