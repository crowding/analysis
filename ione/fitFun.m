function err = fitFun(var,funName,params,freeList,origVarargin)
%err = fitFun(var,funName,params,freeList,origVarargin)
  
%stick values of var into params

params = var2params(var,params,freeList);

%evaluate the function

evalStr = sprintf('err = %s(params',funName);
for i=1:length(origVarargin)
  evalStr= [evalStr,',origVarargin{',num2str(i),'}'];
end
evalStr = [evalStr,');'];
eval(evalStr);

return



