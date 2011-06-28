function [n, cycles] = filtersize(ecc, sf)
%return the filter spatial extent (number of cycles within 
%two-sigma of the envelope) for a visual-system-matched filter at a given 
%eccentricity and spatial . Also convert the result
%into the bandwidth parameter of a Cauchy filter function.
    
%here is data on the extent of spatial summation in as a function of
%spatial frequency and eccentricity, from Banks, Sekuler and Anderson 1991.
%This is for stationary gratings, but I believe the direction-selective
%filters are similar in the direction alomg which the grating oscillates (I
%can never remember whether they call that dimension "width" or what...) 
%(Anderson, Burr and Morrone 1991.)

eccs = [0 2 5 10 20 40];

sfs = [
0.25;...
0.40;...
0.65;...
1.0;...
1.6;...
2.6;...
4.0;...
6.5;...
10.0; ...
16.0;...
26.0;...
];

widths = [...
NaN  NaN  1.00 0.80 1.70 1.70 ;...
0.80 NaN  1.21 1.00 1.70 1.70 ;...
1.05 1.00 1.51 1.49 1.81 1.70 ;...
1.20 1.30 1.71 2.00 1.90 1.70 ;...
1.35 1.70 2.20 2.39 2.00 1.70 ;...
1.50 2.40 2.50 3.02 2.00 NaN  ;...
1.75 3.20 3.40 3.99 2.00 NaN  ;...
1.90 3.21 4.19 4.50 NaN  NaN  ;...
2.03 3.79 4.80 NaN  NaN  NaN  ;...
2.05 3.99 NaN  NaN  NaN  NaN  ;...
2.05 4.22 NaN  NaN  NaN  NaN  ];

%now we need to translate these into cauchy bandwidth params. 
%let's match the fwhm in terms of cycles. two approaches to think of,
%matching bandwidth and matching fwhm. 

%i'll try the bandwidth approach.

%let's interpolate the number of "cycles" based on the logs of eccentricity
%and spatial frequency. 
cycles = interp2(log(eccs), log(sfs), widths, log(ecc), log(sf));

%now convert these Gaussian 2-sigma values into an equivalent Cauchy
%bandwidth parameter. we do this by matching the widths in frequency space.
%if the spatial frequency is 1 (in radians/unit), then the stdev in space
%is cycles*pi, and the sigma in frequency space is given by the fourier
%transform of the gabor which is a gaussian:
sigma_f = 1/(cycles*pi/4);

%now the Cauchy filter function in frequency space is (following Klein and
%Levi):
%
% f^n e^(-f) where the peak spatial frequency happens at n
%
% scaling it to a peak spatial frequency of 1 radian/unit, we have....
% cauchy ~ (n*f)^n * e^(-f*n)
%
% but also, 
%
% this is an instance of the Gamma density function,
%
% Gamma pdf ~ x^(k-1) * e^(-x/theta),
% with k = n+1 and theta = 1/n
% the variance of a Gamma is k*theta^2 = (n+1)/n^2 ,
% so matching variance, we get sigma_f^2*n^2 - n - 1 = 0

% according to Klein and Levi, the variance of the Cauchy would be (n+1)/n?
% Oh, but that's divided by another factor of n to match spatial frequency
% to 1. So this is correct.

n = (1+sqrt(1+4*sigma_f^2))/(2*sigma_f^2);

% uhhh, let's try and verify the above visually. look at that, they're a
% lot different when n is small.
%{
subplot(2, 1, 1);
fplot(@(f) (n*f)^n * exp(-f*n) / n^n / exp(-n), [0 5], 'r-'); %the Cauchy
hold on;
fplot(@(f) exp(-((f-1)/sigma_f)^2), [0 5], 'b-'); % the Gabor
hold off;

%looks to me like the lower cutoff is very similar but the higher cutoff is
%quite different (the cauchy is shallower in high frequencies and ever so
%slightly steeper in low frequencies. I think this is a fair translation,
%though let's look at it in, er, space-space:

subplot(2,1,2);
fplot(@(x) exp(-((x)/(cycles*pi))^2) * cos(x), [-20 20], 'r-'); % the Gabor
hold on;
fplot(@(x)cos(atan(x/n)).^n * cos(atan(x/n)*n), [-20 20], 'b-'); % the Cauchy
hold off;

%}
%all very weird. Perhaps a better calculation of N would be in matching the
%"envelope" width in space... after all, the measurement Banks et al. took
%was in testing the size of a spatial grating, not the bandwidth. But that
%is interesting: for filters with similarly steep lowpass cutoffs, the
%Cauchy shows significantly fewer lobes, it seems. (maybe it's the highpass
%cutoff that matters?)

%so let's instead match envelope widths in space.

%... or not, as is occurs to me that the N values here are pretty much
%right on, and the measurement technique in Banks et al was more of a "how
%much stimulus so we need to soak up all the summation" (i.e. a liberal
%estimate) rather than "how big is just the summation component (estimation
%based on where the kink is in the graph... 

%the "envelope" here is cos(atan(x/n)).^n
%let's just match half-width at half-maximum.
%
%the half width of a gabor is cycles/2

end