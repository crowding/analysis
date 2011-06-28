function motion_demo()

fh = figure();
set(gcf, 'Position', [0 1024 1024 768]);


%add a panel and sliders...

%in the bottom third of the figure, make up some sliders to tweak the
%stimulus. Eccentricity, Delta-X, WavelengthScalar, motionCondition, n,
%nTargets, so seven sliders/menus.

%create six elements 0n the bottom...

uicontrol(fh, 'Style', 'text', 'String', 'Phase', 'Position', [10, 250, 140, 20], 'HorizontalAlignment', 'right');
phaseSlider = uicontrol(fh, 'Tag', 'phase', 'Style', 'slider', 'Max', 2*pi, 'Min', 0, 'Value', 0, 'SliderStep', [0.001 0.01], 'Position', [160, 250, 200, 16], 'Callback', @update);
phaseReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 250, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'Eccentricity (deg)', 'Position', [10, 220, 140, 20], 'HorizontalAlignment', 'right');
eccentricitySlider = uicontrol(fh, 'Tag', 'eccentricity', 'Style', 'slider', 'Max', 10, 'Min', 10 * (2/3)^3, 'Value', 10, 'SliderStep', [0.1 0.1], 'Position', [160, 220, 200, 16], 'Callback', @update);
eccentricityReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 220, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'delta-X (deg)', 'Position', [10, 190, 140, 20], 'HorizontalAlignment', 'right');
deltaXSlider = uicontrol(fh, 'Tag', 'deltax', 'Style', 'slider', 'Max', 2, 'Min', 0, 'Value', 0.75, 'SliderStep', [0.1 0.1], 'Position', [160, 190, 200, 16], 'Callback', @update);
deltaXReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 190, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'delta-t (s)', 'Position', [10, 160, 140, 20], 'HorizontalAlignment', 'right');
deltaTSlider = uicontrol(fh, 'Tag', 'deltat', 'Style', 'slider', 'Max', 0.2, 'Min', 0, 'Value', 0.1, 'SliderStep', [0.1 0.1], 'Position', [160, 160, 200, 16], 'Callback', @update);
deltaTReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 160, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'Spatial period (deg)', 'Position', [10, 130, 140, 20], 'HorizontalAlignment', 'right');
wavelengthSlider = uicontrol(fh, 'Tag', 'wavelength', 'Style', 'slider', 'Max', 0.1688, 'Min', 0.05, 'Value', 0.075, 'SliderStep', [0.1 0.1], 'Position', [160, 130, 200, 16], 'Callback', @update);
wavelengthReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 130, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'Temporal Freq. (Hz)', 'Position', [10, 100, 140, 20], 'HorizontalAlignment', 'right');
temporalFreqSlider = uicontrol(fh, 'Tag', 'temporalf', 'Style', 'slider', 'Max', 20, 'Min', 0, 'Value', 10, 'SliderStep', [.1 .1], 'Position', [160, 100, 200, 16], 'Callback', @update);
temporalFreqReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 100, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', '# targets', 'Position', [10, 70, 140, 20], 'HorizontalAlignment', 'right');
nTargetsSlider = uicontrol(fh, 'Tag', 'nTargets', 'Style', 'slider', 'Max', 25, 'Min', 1, 'Value', 10, 'SliderStep', [.1 .1], 'Position', [160, 70, 200, 16], 'Callback', @update);
nTargetsReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 70, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', '# Stations', 'Position', [10, 40, 140, 20], 'HorizontalAlignment', 'right');
nStationsSlider = uicontrol(fh, 'Tag', 'nStations', 'Style', 'slider', 'Max', 10, 'Min', 1, 'Value', 4, 'SliderStep', [.1 .1], 'Position', [160, 40, 200, 16], 'Callback', @update);
nStationsReadout = uicontrol(fh, 'Style', 'text', 'String', 'xxx', 'Position', [370, 40, 60, 20]);

uicontrol(fh, 'Style', 'text', 'String', 'Visibility', 'Position', [10, 10, 70, 20], 'HorizontalAlignment', 'right');
visibilityMenu = uicontrol(fh, 'Tag', 'visibility', 'Style', 'popupmenu', 'String', {'Left', 'Full', 'Right'}, 'Value', 2, 'Position', [80, 10, 100, 16], 'Callback', @update);

uicontrol(fh, 'Style', 'text', 'String', 'Motion', 'Position', [190, 10, 50, 20], 'HorizontalAlignment', 'right');
motionConditionMenu = uicontrol(fh, 'Tag', 'condition', 'Style', 'popupmenu', 'String', {'Congruent', 'Incongruent', 'Counterphase'}, 'Value', 2, 'Position', [250, 10, 130, 16], 'Callback', @update);

resetButton = uicontrol(fh, 'Style', 'pushbutton', 'String', 'Reset', 'Position', [390, 10, 60, 20], 'Callback', @initial);
        
function update(obj, event)
    figure(fh);
    %just go through all the sliders at once.
    stim.trial_extra_phase = get(phaseSlider, 'Value');
    set(phaseReadout, 'String', num2str(stim.trial_extra_phase));

    stim.trial_extra_r = get(eccentricitySlider, 'Value');
    set(eccentricityReadout, 'String', num2str(stim.trial_extra_r));

    stim.trial_extra_dt = get(deltaTSlider, 'Value');
    set(deltaTReadout, 'String', num2str(stim.trial_extra_dt));
    
    stim.trial_extra_globalVScalar = get(deltaXSlider, 'Value');
    set(deltaXReadout, 'String', num2str(stim.trial_extra_globalVScalar * stim.trial_extra_r * stim.trial_extra_dt));

    stim.trial_extra_wavelengthScalar = get(wavelengthSlider, 'Value');
    set(wavelengthReadout, 'String', num2str(stim.trial_extra_wavelengthScalar * stim.trial_extra_r));

    stim.trial_extra_tf = get(temporalFreqSlider, 'Value');
    set(temporalFreqReadout, 'String', num2str(stim.trial_extra_tf));
    
    stim.trial_extra_nTargets = round(get(nTargetsSlider, 'Value'));
    set(nTargetsReadout, 'String', num2str(stim.trial_extra_nTargets));
    set(nTargetsSlider, 'Value', stim.trial_extra_nTargets);
    
    stim.trial_motion_process_n = round(get(nStationsSlider, 'Value'));
    set(nStationsReadout, 'String', num2str(stim.trial_motion_process_n));
    set(nStationsSlider, 'Value', stim.trial_motion_process_n);
    
    conditions = {'congruent', 'incongruent', 'ambivalent'};
    stim.motionCondition = conditions(get(motionConditionMenu, 'Value'));
    
    visibility = {'left', 'full', 'right'};
    stim.visibilityCondition = visibility(get(visibilityMenu, 'Value'));
    
    motioncalc(stim, fh);
end

%%
%synthesize some test stimuli so that we can see what they look like in
%frequency domain.

%To tweak these values, show the cell toolbar, place your cursor in a
%number, then tweak.


stim.trial_extra_phase = 0 ;
stim.trial_extra_r = 10 ;
stim.trial_extra_tf = 10.00000 ;
stim.trial_extra_wavelengthScalar = 0.075 ;
stim.trial_extra_dt = 0.1 ;
stim.trial_extra_globalVScalar = .75 ;
stim.trial_extra_widthScalar = 0.075 ;
stim.trial_extra_durationScalar = 0.66667 ;
stim.trial_extra_nTargets = 11;
stim.trial_motion_process_order = 4 ;
stim.trial_motion_process_n = 4 ;
stim.motionCondition = cell(1,1);
stim.motionCondition{1,1} = sprintf('incongruent');
stim.visibilityCondition = cell(1,1);
stim.visibilityCondition{1,1} = sprintf('full');

%let's show a GUI to tweak these values. On each change, recalc. the
%fourier transform.

%update();

%analyze it, and show.

initial();

    function initial(obj, event)
        set(eccentricitySlider, 'Value', 10);
        set(deltaXSlider, 'Value', .75);
        set(deltaTSlider, 'Value', 0.1);
        set(wavelengthSlider, 'Value', 0.075);
        set(temporalFreqSlider, 'Value', 10);
        set(nTargetsSlider, 'Value', 10);
        set(motionConditionMenu, 'Value', 2);
        set(visibilityMenu, 'Value', 2);
        update();
    end

end

