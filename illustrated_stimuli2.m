function illustrated_stimuli2(outfile)

fh = fopen(outfile, 'w');

f = figure(1); clf;
set(f, 'Position', [100 768 384 384])

%calculate some example stimuli in the 'congruent', 'incongruent', and
%'counterphase' conditions.

stim.trial_extra_r = 10 ;
stim.trial_extra_tf = 10.00000 ;
stim.trial_extra_wavelengthScalar = 0.075 ;
stim.trial_extra_dt = 0.1 ;
stim.trial_extra_globalVScalar = .75 ;
stim.trial_extra_widthScalar = 0.075 ;
stim.trial_extra_durationScalar = 0.66667 ;
stim.trial_extra_nTargets = 1;
stim.trial_motion_process_order = 4 ;
stim.trial_motion_process_n = 4 ;
stim.motionCondition = cell(1,1);
stim.motionCondition{1,1} = 'carrier';
stim.visibilityCondition = cell(1,1);
stim.visibilityCondition{1,1} = sprintf('full');

colormap(gray(256));

ax1 = subplot(1,2,1);
stim.motionCondition{1,1} = 'carrier';
[im, x, y] = mkstim(stim);
imagesc(x, y, im, [-1 1]); axis square;
set(gca, 'ytick', []);
set(gca, 'xtick', []);
xlabel x;
ylabel t;
%title('First-order');

ax2 = subplot(1,2,2);
stim.motionCondition{1} = 'envelope';
[im, x, y] = mkstim(stim);
imagesc(x, y, im, [-1 1]); axis square;
set(gca, 'ytick', []);
set(gca, 'xtick', []);
xlabel x;
ylabel t;
%title('Position-defined');

[path, name, ext] = fileparts(outfile);
fig1_file = fullfile(path,[name '.eps']);
fprintf(fh, '%s\n', fig1_file);
print('-depsc',fig1_file)

%close(f);

    function [im, xs, ts] = direction(trial)
        leftward = linspace(-1,1,5);
        rightward = linspace(1,-1,5);
        x = linspace(-10,10,5);
        components = cellfun(@component, leftward, rightward, x, 'UniformOutput', 0);
        function c = component()
            c = CauchyPatch...
                ( 'size', [trial.trial_extra_wavelengthScalar*r ...
                           trial.trial_extra_widthScalar*r ...
                           trial.trial_extra_durationScalar*trial.trial_extra_dt]...
                , 'order', trial.trial_motion_process_order ...
                , 'velocity', trial.trial_extra_wavelengthScalar*r*trial.trial_extra_tf ...
                , 'phase', 0 ...
                );
        end
    end

    function [im, xs, ts] = mkstim(trial)
        r = trial.trial_extra_r;
        nt = trial.trial_extra_nTargets;

        CP = CauchyPatch...
            ( 'size', [trial.trial_extra_wavelengthScalar*r ...
                       trial.trial_extra_widthScalar*r ...
                       trial.trial_extra_durationScalar*trial.trial_extra_dt]...
            , 'order', trial.trial_motion_process_order ...
            , 'velocity', trial.trial_extra_wavelengthScalar*r*trial.trial_extra_tf ...
            , 'phase', 0 ... %a test...
            );

        motion = ApparentMotion...
            ( 'primitive', CP ...
            , 'dx', trial.trial_extra_globalVScalar * trial.trial_extra_dt * r ...
            , 'dt', trial.trial_extra_dt ...
            , 'n', trial.trial_motion_process_n + 1 ...
            );

        %then replicate the apparent motion around the circle...
        s = ApparentMotion ...
            ( 'primitive', motion ...
            , 'dx', 2*pi*r / trial.trial_extra_nTargets ...
            , 'dt', 0 ...
            , 'n', nt ...
            );

        %now let's deal with the motion condition (congruent, incongruent, etc.)

        switch trial.motionCondition{1}
            case 'carrier'
              contrast = 1/sqrt(2);
              s.primitive.dx = 0;
              s.Patch.center(1) = 1.125;
              s2 = s;
              s2.primitive.primitive.velocity = -s.primitive.primitive.velocity;
              s2.Patch.center(3) = ...
                  s.primitive.dt * (s.primitive.n);
              %s2 = s;
              %s2.primitive.primitive.velocity = -s.primitive.primitive.velocity;
              %s2.
            case 'envelope'
              contrast = 0.5;
              s2 = s;
              s2.primitive.primitive.velocity = -s.primitive.primitive.velocity;
              s3 = s;
              s3.Patch.center(1) = ...
                  s3.primitive.dx * (s3.primitive.n - 2);
              s3.Patch.center(3) = ...
                  s.primitive.dt * (s.primitive.n);
              s3.primitive.dx = -s3.primitive.dx;
              s4 = s3;
              s4.primitive.primitive.velocity = -s4.primitive.primitive.velocity;
            otherwise
                %numeric...
                error('figure me out')
                contrast = NaN;
        end

        x = [-1.5 3.75]
        y = [-1.125 1.125]
        t = [-0.1 1]
        xs = linspace(x(1), x(2), 512);
        ts = linspace(t(1), t(2), 512);
        z = evaluate(s, xs, 0, ts);
        z = z + evaluate(s2, xs, 0, ts);
        if isequal('envelope', trial.motionCondition{1})
            z = z + evaluate(s3, xs, 0, ts);
            z = z + evaluate(s4, xs, 0, ts);
        end
        z = z * contrast;
        im = squeeze(z)';
    end
end