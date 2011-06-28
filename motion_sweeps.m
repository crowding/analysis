function motion_sweeps
%start the motion demo, then sweep some of the controls.

motion_demo;
set(gcf, 'Position', [0 1024 1024 768]);
set(gcf, 'PaperUnits', 'points', 'PaperSize', [1024 768], 'PaperPosition', [0 0 1024 768]);

%phase
menu = findobj(gcf, 'Tag', 'visibility');
oldvis = get(menu, 'Value');
set(menu, 'Value', 3);
slider = findobj(gcf, 'Tag', 'nTargets');
oldnt = get(menu, 'Value');
set(slider, 'Value', 8);  %so as to separate
cf = get(menu, 'Callback');

drawnow()
cf(menu, []);
make_sweep('phase', linspace(0, pi/2, 100), 'occlusion.avi');

cf(menu, []);
set(menu, 'Value', oldvis);
set(slider, 'Value', oldnt);
cf(menu, []);



%delta-x
make_sweep('deltax', linspace(-1.5, 1.5, 200), 'deltax.avi');

%tf
make_sweep('temporalf', linspace(0, 20, 100), 'temporalf.avi');

%tf
make_sweep('wavelength', linspace(0.05, .15, 100), 'wavelength.avi');

end

function make_sweep(tag, values, outfile)
tempfile = tempname();
slider = findobj(gcf, 'Tag', tag);
oldval = get(slider, 'Value');
cf = get(slider, 'Callback');
avi = avifile(outfile);
for i = values
    set(slider, 'Value', i);
    cf(slider, []);
    %GETFRAME sucks ass!!! use PRINT instead...
    %avi = addframe(avi, getframe(gcf()));
    
    drawnow;
    print('-dpng', tempfile)
    IMG = imread([tempfile '.png'], 'png');
    avi = addframe(avi, IMG);
end
set(slider,'Value', oldval);
avi = close(avi);
delete(tempfile);
end