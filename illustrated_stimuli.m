function illustrated_stimuli(outfile)

fh = fopen(outfile, 'w');

f = figure();
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
stim.motionCondition{1,1} = 'ambivalent';
stim.visibilityCondition = cell(1,1);
stim.visibilityCondition{1,1} = sprintf('full');

colormap(gray(256)); 

ax1 = subplot(2,2,1);
[im, x, y] = mkstim(stim);
imagesc(x, y, im, [-1 1]); axis off; axis square;
title('Counterphase')

ax2 = subplot(2,2,2); 
stim.motionCondition{1} = 'congruent';
[im, x, y] = mkstim(stim);
imagesc(x, y, im, [-1 1]); axis off; axis square;
title('Congruent')

ax3 = subplot(2,2,3);
stim.motionCondition{1} = 'incongruent';
[im, x, y] = mkstim(stim);
imagesc(x, y, im, [-1 1]); axis off; axis square;
title('Incongruent')

ax4 = subplot(2,2,4); %insert scale bars
axis off;
xlim([x(1), x(end)]);
ylim([y(1), y(end)]);
axis square;

ax4 = 0; %insert scale bars
axis off;
xlim([x(1), x(end)]);
ylim([y(1), y(end)]);
axis square;

line([x(1) x(1)+1], [y(end)-0.05 y(end)-0.05]);
text(x(1) + 1.3, y(end) - 0.05, '1° at 10° ecc.', 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Middle');
line([x(1) x(1)], [y(end)-0.15 y(end) - 0.25]);
text(x(1)+0.3, y(end) - 0.2, '100 ms', 'HorizontalAlignment', 'Left', 'VerticalAlignment', 'Middle');
%

[path, name, ext] = fileparts(outfile);
fig1_file = fullfile(path,[name '.eps']);
fprintf(fh, '%s\n', fig1_file);

print('-depsc',fig1_file)

    function [im, xs, ts] = mkstim(trial)
        r = trial.trial_extra_r;
        nt = trial.trial_extra_nTargets;
        
        CP = CauchyPatch...
            ( 'size', [trial.trial_extra_wavelengthScalar*r trial.trial_extra_widthScalar*r trial.trial_extra_durationScalar*trial.trial_extra_dt]...
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
            , 'n', nt);
        
        %now let's deal with the motion condition (congruent, incongruent, etc.)
        
        switch trial.motionCondition{1}
            case 'congruent'
                contrast = 1/sqrt(2);
            case 'incongruent'
                contrast = 1/sqrt(2);
                s.primitive.primitive.velocity = -s.primitive.primitive.velocity;
            case 'ambivalent'
                s2 = s;
                contrast = 0.5;
                s2.primitive.primitive.velocity = -s.primitive.primitive.velocity;
            otherwise
                contrast = NaN;
        end
        
        [x, y, t] = extent(s);
        xs = linspace(x(1), x(2), 512);
        ts = linspace(t(1), t(2), 512);
        z = evaluate(s, xs, 0, ts);
        if isequal('ambivalent', trial.motionCondition{1})
            z = z + evaluate(s2, xs, 0, ts);
        end
        z = z * contrast;
        im = squeeze(z)';
    end
end