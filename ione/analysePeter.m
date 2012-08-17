clear all
close all
filename1='spacing_series_calculations.csv';
filename2='contrast_series_calculations.csv';
[ num1, txt1, data1]=xlsread(filename1, 'A2:Q192');
[ num2, txt2, data2]=xlsread(filename2, 'A2:Q97');

data=cat(1, data1, data2);
num=cat(1, num1, num2);
txt=cat(1, txt1, txt2(:, 1:5));

% data=data2;
% num=num2;
% txt=txt2;


%
sublist= unique(txt(:, 5));
dirlist=unique(num(:, 3));
spacinglist=unique(num(:, 4));
colorlist=hot(length(dirlist)+3);
symlist={'o', 's' '*' 'x' 'd' '^' 'v' '>' '<' 'p' 'h' 'v' '>' '<' 'p' 'h'};



for s=1:length(sublist)
    for d=1:length(dirlist)
        for sp=1:length(spacinglist)
            ind=find( num(:, 3)==dirlist(d) & num(:, 4)==spacinglist(sp) & strcmp(txt(:,5),sublist{s}));
            if ~isempty(ind)
                plot3(dirlist(d), spacinglist(sp), num(ind, 6), [symlist{s}], 'MarkerSize', 5, 'Color', colorlist(d, :),'MarkerFaceColor', colorlist(d, :)); hold on
            end
        end
        
    end
    line([0 0],[0 25], [ 0 0 ])
    xlabel('carrier direction content')
    ylabel('envelope spacing')
    zlabel('envelope displacement (dx)')
    set(gca, 'XLim', [0 1])
    set(gca, 'YLim', [0 25])
    set(gca, 'ZLim',[-4 4])
%     text(.5, 20, -2, 'local motion direction biases percept')
%     text(.5, 20, 2, 'induced observer motion')
%     text(.5, 20 ,0, 'no effect of local motion on percept')
end
