function [s, old] = fixfieldnames(s)

%fix illegal field names that come from loaded files...

n = fieldnames(s);

n = regexprep(n, '^[^a-zA-Z]|[^a-zA-Z0-9_]*', '_');
n = regexprep(n, '^(.{64}).*', '$1');

s = setfieldnames(s, n);