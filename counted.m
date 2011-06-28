function [fout, gc] = counted(fin)
    fout = @f;
    calls = 0;
    gc = @getcount;
    
    function varargout = f(varargin)
        [varargout{1:nargout}] = fin(varargin{:});
        if nargout == 0 && exist('ans', 'var')
            varargout{1} = ans; %#ok
        end
        calls = calls + 1;
    end

    function count = getcount()
        count = calls;
    end
end