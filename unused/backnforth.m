function b = backnforth(n)
nn=1:n^2;
full(sparse(ceil(nn/n),abs(mod(nn-0.5+n,2*n)-n)+0.5,nn))
end