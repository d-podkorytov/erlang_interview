% TODO: parsing errors handling for returning {error, Error} 
% tests&report 

-module(parse).
-compile(export_all).

-define (URL_NO_ARGS, <<"https://example.com:8000/over/there">>).
-define (URL_NO_PORT, <<"https://example.com/over/there?name=ferret&var1=value1&var2=value2">>).
-define (URL_NO_PATH, <<"https://example.com:8000/?name=ferret&var1=value1&var2=value2">>).
-define (URL_NO_HOST, <<"/path?name=ferret&var1=value1&var2=value2">>).

-define (URL, <<"https://example.com:8000/over/there?name=ferret&var1=value1&var2=value2">>).
%%                \_/   \______________/\_________/ \________________________________/
%%                 |           |            |            |        
%%              scheme     host_and_port   path        query arguments
%%                 |   _____________________|__
%%                / \ /                        \
%%                 urn:example:animal:ferret:nose
-define (HOST_PORT_AND_PATH,<<"example.com:8000/over/there?name=ferret&var1=value1&var2=value2">>).
-define (PORT_AND_PATH,<<"8042/over/there?name=ferret&var1=value1&var2=value2">>).
-define (PATH_AND_ARGS,<<"/over/there?name=ferret&var1=value1&var2=value2">>).
-define (ARGS,"name=ferret&var1=value1&var2=value2").

% it return {scheme,Scheme,Authority_path_query_fragment} 

scheme(<<"://",Rest/binary>>,Acc) -> <<Rest1/bitstring>> = Rest, % skip ://
                                     {list_to_binary(lists:reverse(Acc)),Rest1};
scheme(<<H,Rest/binary>>,Acc)   -> scheme(Rest,[H|Acc]);
scheme(<<"">>,Acc)             -> Acc.

test1()->scheme(?URL,[]).

% it return {host,"example.com","8042/over/there?name=ferret#nose"}
host(L,Acc) when is_list(L)    -> host(list_to_binary(L),Acc);
host(<<":",Rest/binary>>,Acc) -> {list_to_binary(lists:reverse(Acc)),Rest};
host(<<H,Rest/binary>>,Acc)    -> host(Rest,[H|Acc]);
host(<<"">>,Acc)              -> Acc.

test2()->host(?HOST_PORT_AND_PATH,[]).

port(L,Acc) when is_list(L)   -> ?FUNCTION_NAME(list_to_binary(L),Acc);
port(<<"/",Rest/binary>>,Acc) ->  {list_to_integer(lists:reverse(Acc)),<<"/",Rest/bitstring>>};
port(<<H,Rest/binary>>,Acc)   ->  ?FUNCTION_NAME(Rest,[H|Acc]);
port(<<"">>,Acc)              ->   Acc.

test3()->port(?PORT_AND_PATH,[]).

% it returns {raw_arguments,Path,Unparsed_arguments} like {"/over/there","name=ferret&var1=value1&var2=value2"}
raw_arguments(L,Acc) when is_list(L)    -> ?FUNCTION_NAME(list_to_binary(L),Acc);
raw_arguments(<<"?",Rest/binary>>,Acc)  -> {list_to_binary(lists:reverse(Acc)),Rest};
raw_arguments(<<H,Rest/binary>>,Acc)    -> raw_arguments(Rest,[H|Acc]);
raw_arguments(<<"">>,Acc)               -> Acc.

test4()->raw_arguments(?PATH_AND_ARGS,[]).

arguments(Bin) when is_binary(Bin)-> ?FUNCTION_NAME(binary_to_list(Bin));                                          
arguments(L)   when is_list(L)-> lists:foldl(fun(VarValue,Acc)->
                            [Var|[Value]] = string:tokens(VarValue,"="),
                            maps:put(Var,Value,Acc) 
                           end,
                           #{},
                           string:tokens(L,"&")).

test5()-> arguments(?ARGS).

% fetch from module export all function named like test*() 
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

-spec parse(URL) -> Result when
URL    :: binary() | list(),
Result :: map | {error, Error},
Error  ::  term().

%
parse(URL) when is_list(URL)   -> ?FUNCTION_NAME(list_to_binary(URL));  
parse(URL) when is_binary(URL) -> %{Scheme,Authority_path_query_fragment} = scheme(URL,[]), % fix it! case of empty host and empty scheme here
             {Scheme,Authority_path_query_fragment} = try scheme(URL,[]) of
                                                         {Scheme1,Tail1} -> {Scheme1,Tail1};
                                                          EmptyHost      -> {list_to_binary(""),list_to_binary(lists:reverse(EmptyHost))}
                                                       catch
                                                       _:_:_ -> {list_to_binary(""),list_to_binary("")}               
                                                      end,

             {Host,PortAndPathWithArgs}               = try host(Authority_path_query_fragment,[]) of
                                                               {Host0,PortAndPathWithArgs0} -> {Host0,PortAndPathWithArgs0};
                                                               RPortAndPathWithArgs -> Rev=lists:reverse(RPortAndPathWithArgs),
                                                                                       {Host1,Path1} = host_and_path(list_to_binary(Rev),[]), 
                                                                                       {Host1,Path1}
                                                              catch
                                                               Err:Reason:Stack -> io:format("~p:~p ~p ~n",[?MODULE,?LINE,{parsing_error,Err,Reason,Stack}]),
                                                                                   {{parsing_error,Err,Reason,Stack},"",""}     
                                                             end,
             {PortInt,Rest2}                             = try port(PortAndPathWithArgs,[]) of
                                                               {PortInt0,Rest0} -> {PortInt0,Rest0};
                                                                Q                    ->  {80,lists:reverse(Q)}
                                                             catch
                                                              _:_:_ -> PortAndPathWithArgsBin=list_to_binary(PortAndPathWithArgs),
                                                                       {80,<<"/",PortAndPathWithArgsBin/bitstring>>}  
                                                             end,
             {Path,Args}                                    = try raw_arguments(Rest2,[]) of
                                                               {R1,Args1} -> {R1,Args1};
                                                               L when is_list(L)        -> 
                                                                                           {Rest2,""}  % watch out here needs test
                                                             catch
                                                               _:_:_ -> {parsing_error,"",""}   
                                                             end,
                                              
             #{ arguments => arguments(Args),
                port      => PortInt, 
                host      => Host,
                scheme    => Scheme,
                %path_and_args => Rest2,
                path          => Path
                  
              }.

test6()->parse(?URL).
test7()->parse(?URL_NO_ARGS).
test8()->parse(?URL_NO_PORT).
test9()->parse(?URL_NO_PATH).
test10()->parse(?URL_NO_HOST).

host_and_path(<<"/",Rest/binary>>,Acc) -> %io:format("~p:~p Acc=~p ~n",[?MODULE,?LINE,Acc]),
                                          {lists:reverse(Acc),binary_to_list(Rest)};
host_and_path(<<H,Rest/binary>>,Acc)   -> ?FUNCTION_NAME(Rest,[H|Acc]);
host_and_path(<<"">>,Acc)              -> Acc.

%test9()->host_and_path(?URL_NO_PORT,0,[]).

path(<<"?",Rest/binary>>,Acc) -> <<Rest1/bitstring>> = Rest,
                                 {lists:reverse(Acc),binary_to_list(Rest1)};

path(<<H,Rest/binary>>,Acc)   -> ?FUNCTION_NAME(Rest,[H|Acc]);
path(<<"">>,Acc)              -> Acc.

%test10()->[path(?URL_NO_PORT,0,[])].
