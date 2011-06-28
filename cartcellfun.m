function C = cartcellfun(fn, A, B);
% function C = cartcellfun(fn, A, B);
% a generalization of "cartprod" with cellfun. Takes a binary function as
% first argument.
%
% If A is of dimension (a1, a2. ... an) and B is of dimension (b1, b2 ... bm)
% then the cartesian product of A and B is C, where
% C(x1, ... xn, y1, ... ym) = A(x1, ... xn) B(y1, ... yn).
% 
% or size(C) = [size(A) size(B)]
%
% TODO: generalise this to N arguments....
dims = cat(2, size(A), size(B));

%outer product...
C = cell(numel(A), numel(B));

for i = 1:numel(A)
    for j = 1:numel(B)
        C{i,j} = fn(A{i}, B{j});
    end
end

C = reshape(C, dims);
