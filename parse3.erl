%BUGS aa.com:path as aa.com/path 

-module(parse3).
-compile(export_all).

do_chains([],Acc)        -> Acc;
do_chains([F1|Tail],Arg) ->
  try apply(?MODULE,F1,[Arg]) of
   {error,Error} -> {error,Error};
   {Parsed,ArgTail} -> [Parsed|?FUNCTION_NAME(Tail,ArgTail)]
  catch
   Err:Reason:Stack-> {error,#{error => Err, reason => Reason, stack => Stack, arg => Arg}}
  end.

test()-> do_chains([a,c],<<"a">>).

% chains examples
% input is ({A,B}) and output too
a(<<"a">>)->{<<"a">>,<<"a">>}.

b({_,Arg})->{error,{?FUNCTION_NAME,Arg}}.
             
c({_,<<"c">>})->{<<"c">>,<<"c">>};
c(Arg)->{error,{?FUNCTION_NAME,Arg}}.

scheme(URL)when is_binary(URL)->?FUNCTION_NAME(binary_to_list(URL));
scheme(URL)when is_list(URL)-> 
              case string:str(URL,"://") of
              0-> {error,{bad_scheme,URL}};
              N-> SchemeSize=8*(N-1),
                  <<Scheme:SchemeSize,"://",Tail/bitstring>> = list_to_binary(URL),
                  {<<Scheme:SchemeSize>>,Tail}
              end.

test1()->scheme("https://aa.com").               
test2()->scheme("aa.com:8000").               

host(HOST_AND_OTHERS)when is_binary(HOST_AND_OTHERS)->?FUNCTION_NAME(binary_to_list(HOST_AND_OTHERS));
host(HOST_AND_OTHERS)when is_list(HOST_AND_OTHERS)-> 
              case {string:str(HOST_AND_OTHERS,":"),string:str(HOST_AND_OTHERS,"/")} of
              {0,0}-> {error,{bad_host,HOST_AND_OTHERS}};
              {0,N1}-> HostSize=8*(N1-1),% if not found port but exists tailing '/'
                      <<Host:HostSize,"/",Tail/bitstring>> = list_to_binary(HOST_AND_OTHERS),
                     {<<Host:HostSize>>,Tail};

              {N,_} when N>0 -> HostSize=8*(N-1),% if found port
                               %<<Host:HostSize,_/bitstring>> = list_to_binary(HOST_AND_OTHERS),
                               %io:format("Host Size = ~p ~p~n",[<<Host>>,HostSize]),
                               <<Host:HostSize,":",Tail/bitstring>> = list_to_binary(HOST_AND_OTHERS),
                               {<<Host:HostSize>>,Tail};
              _ ->   
                     {error,{bad_host,HOST_AND_OTHERS}} % bad cases
              end.



test3()->host("aa.com/path").               
test4()->host("aa.com:8000").               
test5()->host("aa.com:8000/path/?var1=value1&var2=value2").               

test6()->host("aa.com").               
test7()->host("aa.com/").               

port(PORT_AND_OTHERS)when is_binary(PORT_AND_OTHERS)->?FUNCTION_NAME(binary_to_list(PORT_AND_OTHERS));
port(PORT_AND_OTHERS)when is_list(PORT_AND_OTHERS)-> 
              case string:str(PORT_AND_OTHERS,"/") of
               0   ->     try list_to_integer(PORT_AND_OTHERS) of
                           Int when is_integer(Int) -> {list_to_binary(integer_to_list(Int)),<<>>} % "http://aa.com:8000" case
                          catch
                           _:_:_ -> {error,{bad_port,PORT_AND_OTHERS}}   
                          end;
                          
               N when N>0 -> PortSize=8*(N-1),
                               %<<Port:PortSize,_/bitstring>> = list_to_binary(PORT_AND_OTHERS),
                               %io:format("Port Size = ~p ~p~n",[<<Port>>,PortSize]),
                               <<Port:PortSize,"/",Tail/bitstring>> = list_to_binary(PORT_AND_OTHERS),
                               %{<<Port:PortSize>>,Tail};
                               %if not integer return default http port {<<"80">>,PORT_AND_OTHERS}
                               %else return port as string
                               try list_to_integer(binary_to_list(<<Port:PortSize>>)) of
                                 _Int2->{<<Port:PortSize>>,Tail} 
                               catch
                                _:_:_ ->{<<"80">>,list_to_binary(PORT_AND_OTHERS)} % not found port 
                               end 
              %_ ->  {error,{bad_port,PORT_AND_OTHERS}} % bad cases
              end.

test8()-> port("8000/path/?var1=value1&var2=value2").%passed               
test9()-> port("path/?var1=value1&var2=value2").% not passed               
test10()-> port("8000/"). % passed              
test11()-> port("8000").% passed               

path(PATH_AND_ARGS)when is_binary(PATH_AND_ARGS)->?FUNCTION_NAME(binary_to_list(PATH_AND_ARGS));
path(PATH_AND_ARGS)when is_list(PATH_AND_ARGS)-> 
              case string:str(PATH_AND_ARGS,"?") of
               0   ->  {list_to_binary(PATH_AND_ARGS),<<>>}; 
                          
               N when N>0 -> PathSize=8*(N-1),
                               %<<Path:PathSize,_/bitstring>> = list_to_binary(PATH_AND_ARGS),
                               %io:format("Path Size = ~p ~p~n",[<<Path>>,PathSize]),
                               <<Path:PathSize,"?",Tail/bitstring>> = list_to_binary(PATH_AND_ARGS),
                               {<<Path:PathSize>>,Tail} 
              %_ ->  {error,{bad_port,PATH_AND_ARGS}} % bad cases
              end.

test12()-> path("?var1=value1&var2=value2").% passed              
test13()-> path("path/some/file/?var1=value1&var2=value2"). % passed              
test14()-> path("path/some/"). %passed              
test15()-> path("path/some").  %passed             

arguments(Bin) when is_binary(Bin)-> ?FUNCTION_NAME(binary_to_list(Bin));                                          
arguments(L)   when is_list(L)-> 
                         {
                          lists:foldl(
                           fun(VarValue,Acc)->
                            try string:tokens(VarValue,"=") of % add try here
                              [Var|[Value]] -> maps:put(Var,Value,Acc);
                              [Var]         -> {error,{name_without_value,Var}} 
                            catch
                             _:_:_ -> {error,{bad_arguments,L}} 
                            end 
                           end,
                           #{},
                           string:tokens(L,"&")),
                          <<>>
                          }.

test16()-> arguments(<<"var1=value1&var2=value2">>).%passed
test17()-> arguments(<<"var1=value1&var2=">>).
test18()-> arguments(<<"var1=value1&var2">>).
test19()-> arguments(<<"var1=value1&">>).% passed
test20()-> arguments(<<"var1=value1">>). % passed

t()-> lists:foldl(fun({Key,Value},Acc)->
                          case list_to_binary(atom_to_list(Key)) of
                            <<"test",_/bitstring>> when Value == 0 -> [Key|Acc];
                            _                                      -> Acc 
                          end 
                       end,
                       [],
                       erlang:get_module_info(?MODULE,exports)
                 ).

% run all tests
run_tests()-> lists:map(fun(A)->{A,apply(?MODULE,A,[])} end,t()).

p(URL) when is_binary(URL) -> [Scheme,Host,Port,Path,Args|_]=do_chains([scheme,host,port,path,arguments],URL),
                              #{scheme=>Scheme,host =>Host,port=>list_to_integer(binary_to_list(Port)),path=>Path,args=>Args}.

test21()->p(<<"https://aa.com:8000/path/?var1=value1&var2=value2">>).
test22()->p(<<"https://aa.com:8000/path/?var1=value1&var2=">>).

parse(URL)-> L=maps:to_list(p(URL)),
             fold(L).

fold(VL)->fold(VL,[],[]).

fold([],Acc1,Acc2)->{maps:from_list(Acc1),Acc2};
fold([H|T],Acc1,Acc2) -> case H of
                          {Key,{error,Err}} -> ?FUNCTION_NAME(T,Acc1,[{Key,{error,Err}}|Acc2]);
                                    A -> ?FUNCTION_NAME(T,[A|Acc1],Acc2)  
                         end.

test23()->parse(<<"https://aa.com:8888/path/?var1=value1&var2=">>).  