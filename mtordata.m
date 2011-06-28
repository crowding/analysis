function mtordata(infile, outfile, diagnostic_interval)
    if nargin < 3
        diagnostic_interval = 1000;
    end

    s = load(infile);

%    require(openWritePipe('fid', ['r --slave']), @convert);
    require(openFile(outfile, 'w'), @convert);
    function convert(params)
        fid = params.fid;
        linecount = 0;
        dumpR(s, @printer);
        printer('save(s,file=%s)\n',outfile);
        printer('quit();\n')
        
        function printer(varargin)
            str = sprintf([varargin{1} '\n'], varargin{2:end});
            fwrite(fid, str, 'char');
            %popenw(fid,str,'char');
            linecount = linecount + 1;
            if mod(linecount,diagnostic_interval) == 0
                fprintf(2, '%d: %s...\n', linecount, str(1:min(end-1, 60)));
                printer('print(%d)\n', linecount);
            end
        end
    end

    function init = openWritePipe(name, cmd)
        init = @i;
        
        function [release, params] = i(params)
            params.(name) = popenw(cmd);
            
            release = @r;
            function r()
                popenw(params.(name), []);
            end
        end
    end
end