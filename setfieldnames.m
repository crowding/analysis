function s = setfieldnames(s, names)

c = struct2cell(s);
s = cell2struct(c, names);