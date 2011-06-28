function grid_figure(input, output)

load(input, 'stimuli')

% locate the appropriate rows of the stimulus
uniques = structfun(@unique, rmfield(stimuli, {'xs', 'ts', 'tableau'}), 'UniformOutput', 0)

%make a grid of nine stimuli, maybe only showing one in the middle.
%Annotate some of the plots.

stimuli = struct ...
    ( 'trial_extra_r', num2cell(uniques.trial_extra_r(repmat(4, 3, 3))) ...
    , 'trial_extra_globalVScalar', num2cell(uniques.trial_extra_globalVScalar([2 2 2;2 2 2;1 2 3])) ...
    , 'trial_extra_tf', num2cell(uniques.trial_extra_tf([2 2 2;1 2 3;2 2 2])) ...
    , 'trial_extra_wavelengthScalar', num2cell(uniques.trial_extra_wavelengthScalar([1 2 3;2 2 2;2 2 2])) ...
    , 'trial_extra_dt', uniques.trial_extra_dt(1) ...
    , 'trial_extra_widthScalar', uniques.trial_extra_widthScalar(1) ...
    , 'trial_extra_durationScalar', uniques.trial_extra_durationScalar(1) ...
    , 'trial_motion_process_order', uniques.trial_motion_process_order(1) ...
    , 'trial_motion_process_n', uniques.trial_motion_process_n(1) ...
    , 'motionCondition', {{'incongruent'}} ...
)

%render images
stimuli = arrayfun(@eachtableau, stimuli, zeros(size(stimuli)))

%this gives us 'x' 'y' and 't' fields, which we expand...
for fld = {'x', 'y', 't'}
    [stimuli.(fld{1})] = deal(arrayfun(@(f, varargin)f{1}([varargin{:}]), {@min, @max}, stimuli.(fld{1})));
end

%render images
stimuli = arrayfun(@eachtableau, stimuli)

%make a figure with all x-t stimuli to scale.
stimuli = stimuli';

figure(1)
for i = 1:9
        ax(i) = subplot(3,3, i);
        imagesc(stimuli(i).xs{1}, stimuli(i).ts{1}, stimuli(i).tableau{1}, [0,1])
        axis off
end
colormap gray(256)

%annotate the figures.
l = []
for (i=1:9)
    dt = stimuli(i).trial_extra_dt
    dx = stimuli(i).trial_extra_globalVScalar * stimuli(i).trial_extra_r * dt
    wl = stimuli(i).trial_extra_r * stimuli(i).trial_extra_wavelengthScalar
    axes(ax(i))
    if i <= 3
        % wavelength
        l(i) = line([dx-wl/2 dx+wl/2], [dt/2 dt/2], 'color', 'w')
        text(dx, dt/2, sprintf('%4g\\circ', wl), 'color', 'w',  'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom', 'FontSize', 9)
    elseif i <= 6
        v = wl * stimuli(i).trial_extra_tf
        l(i) = line([dx+dt/2*v, dx+dt*v], [dt/2, 0], 'color', 'w')
        text(dx+dt*v, 0, sprintf('%4g^{\\circ}/_{s}', v), 'color', 'w',  'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 9)
    elseif i <= 9
        l(i) = line([0 dx], [dt/2 dt/2], 'color', 'w')
        text(dx/2, dt/2, sprintf('%4g\\circ', dx), 'color', 'w', 'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom', 'FontSize', 9)
    end
    if i == 9
        l(10) = line([0 0], [dt*2 dt*3], 'color', 'w')
        text(0, 5*dt/2, sprintf('%4g s', dt), 'color', 'w', 'HorizontalAlignment', 'left', 'VerticalAlignment', 'middle', 'FontSize', 9)
    end
end

set(1, 'PaperPosition', [0 0 6 5], 'PaperSize', [6 5])

figure(1)
print('-dpdf', output)