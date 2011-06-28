function z = kgrating(x, y)
    ampl1 = shiftdim([0.3 0.3 0.3], -1);
    ampl2 = shiftdim([0.2 -0.2 0.2], -1);
    
    gutter1 = 0.2;
    gutter2 = 0.4;
    
    grid1 = sign(mod(x + 0.375*floor(y), 1) - 0.5);
    grid2 = sign(mod(x + 0.375*floor(y) + 0.25,1) - 0.5);
    
    z = bsxfun(@times, ampl2, grid2)+bsxfun(@times, ampl1, grid1) + 0.5;
end