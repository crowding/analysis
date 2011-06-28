function tree = phasecong1(signal, stages)

    defaults = struct ...
        ( 'nscale', 5 ...
        , 'minWavelength', 3 ...
        , 
            
%compute phase congruency within a 1d signal according to a complex DTDWT
%Adapted from 2D phase congruency and dual tree wavelet code by Peter
%Kovesi (http://www.csse.uwa.edu.au/~pk/Research/research.html)
%
[Faf, Fsf] = FSfarras;
[af, sf] = dualfilt1;
tree = dualtree(signal, stages, Faf, af);

end









