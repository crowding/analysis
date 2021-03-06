function this = demo_circles(varargin)

    if (nargin == 1 && ischar(varargin{1}))
        varargin = [{'aviout'} varargin]
    end

    c = CircleDemo ...
        ( 'interactive', 0 ...
        , 'stopafterloops', 4 ...
        , 'x_locations', [-10 10] ...
        , 'y_locations', [0 0] ...
        , 'e_locations', [-1 1] ...
        , 'v_locations', [1 1] ...
        , 'params.cal', Calibration ...
          ( 'interval', 1/60, 'distance', 180/pi ...
          , 'spacing', [20/512, 20/512] , 'rect', [0 0 1024 512]) ...
        , 'params.avirect', [0 0 1024 512] ...
        , 'params.rect', [0 0 1024 512] ...
        , 'params.avistart', 3 ...
        )

    playDemo(c, 'skipFrames', 0, varargin{:});
end

