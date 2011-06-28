function trial = eachtableau(trial, render)

r = trial.trial_extra_r;

interval = 1/120; 
% I really should pull this from the data but I always
% had 120. If you ask about effects because of randomized onset times on
% the order of less than 1/120 second I'll get stabbity. Although I will
% have to deal with trial by trial effects on the occluded trials.

CP = CauchyPatch...
    ( 'size', [trial.trial_extra_wavelengthScalar*r trial.trial_extra_widthScalar*r trial.trial_extra_durationScalar*trial.trial_extra_dt]...
    , 'order', trial.trial_motion_process_order ...
    , 'velocity', trial.trial_extra_wavelengthScalar*r*trial.trial_extra_tf ...
    , 'phase', 0 ... %a test...
    );

stim = ApparentMotion...
    ( 'primitive', CP ...
    , 'dx', trial.trial_extra_globalVScalar * trial.trial_extra_dt * r ...
    , 'dt', trial.trial_extra_dt ...
    , 'n', trial.trial_motion_process_n ...
    , 'center', [0 0 0] ...
    );

%now let's deal with the motion condition (congruent, incongruent, etc.)
switch trial.motionCondition{1}
    case 'congruent'
        contrast = 1/sqrt(2);
    case 'incongruent'
        contrast = 1/sqrt(2);
        stim.primitive.velocity = -stim.primitive.velocity;
    case 'ambivalent'
        stim2 = stim;
        contrast = 0.5;
        stim2.primitive.velocity = -stim2.primitive.velocity;
    otherwise
        contrast = NaN;
end

%now let's render a picture. We render it for an extent of 2*pi*r in the
%X-direction, and only at 0 in the y-direction.
if isfield(trial, 'x')
    x = trial.x;
    y = trial.y;
    t = trial.t;
else
    [x, y, t] = extent(stim);
    trial.x = x;
    trial.y = y;
    trial.t = t;
end

if (~exist('render', 'var')) || render
    %how many points to sample around the circle -- the smallest
    %wavelengthScalar is 0.05, which would mean the nyquist sampling limit is
    %at 4*pi/0.05 = 250 samples. So I'll take 1024 samples to be relatively
    %safe and nice to an FFT algorithm. This is a more than pixels on
    %the screen in the smallest eccentricities, but I don't think
    %there are any resolution problems with the display.
    np = 1024;
    nt = 256;
    
    %generate the grid. The y-coordinate is just sampled at 0.
    xs = x(1):(2*pi*r/np):x(end);
    ys = 0;
    ts = t(1):interval:t(end);
    
    %evaluate the motion.
    z = evaluate(stim, xs, ys, ts);
    if isequal('ambivalent', trial.motionCondition{1})
        z = z + evaluate(stim2, xs, ys, ts);
    end
    z = z * contrast;
    z = z./2+0.5;
    
    %ditch the useless first dimension...
    z = shiftdim(z, 1);
    xs = xs(1:size(z,1));
    
    disp(trial);
    
    %record the outputs in the struct.
    trial.xs = {xs};
    trial.ts = {ts};
    trial.tableau = {z'};
end

end