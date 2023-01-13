%News: User auth form URL like https://clientjiberish:client/secretjiberish@api.example.com/users?username=tralala is supporting now
%      Add tests in EUnit forms
  
-module(parse7).
-compile(export_all).

%-define(TEST,1).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

% run all tests
run_tests()-> lists:map(fun(A)->{A,apply(?MODULE,A,[])} end,tests_functions()).
tests_functions()-> lists:foldl(fun({Key,Value},Acc)->
                          case list_to_binary(atom_to_list(Key)) of
                            <<"test_parse",_/bitstring>> when Value == 0 -> [Key|Acc];
                            _                                      -> Acc 
                          end 
                       end,
                       [],
                       erlang:get_module_info(?MODULE,exports)
                 ).

%do_chains([],Acc)        -> Acc;
%do_chains([F1|Tail],Arg) ->
%  try apply(?MODULE,F1,[Arg]) of
%   {error,Error} -> {error,Error};
%   {Parsed,ArgTail} -> [Parsed|?FUNCTION_NAME(Tail,ArgTail)]
%  catch
%   Err:Reason:Stack-> {error,#{error => Err, reason => Reason, stack => Stack, arg => Arg}}
%  end.
%
%test()-> do_chains([a,c],<<"a">>).
%
% chains examples
% input is ({A,B}) and output too
%a(<<"a">>)->{<<"a">>,<<"a">>}.
%
%b({_,Arg})->{error,{?FUNCTION_NAME,Arg}}.
%             
%c({_,<<"c">>})->{<<"c">>,<<"c">>};
%c(Arg)->{error,{?FUNCTION_NAME,Arg}}.

%return {Scheme/binary,OtherPartsOfURL/binary}
scheme(URL)when is_binary(URL)->?FUNCTION_NAME(binary_to_list(URL));
scheme(URL)when is_list(URL)-> 
              case string:str(URL,"://") of
              0-> {error,{bad_scheme,URL}};
              N-> SchemeSize=8*(N-1),
                  <<Scheme:SchemeSize,"://",Tail/bitstring>> = list_to_binary(URL),
                  {<<Scheme:SchemeSize>>,Tail}
              end.

test_scheme1()->scheme("https://aa.com").               
test_scheme2()->scheme("aa.com:8000").               

%return {UserAndClientAndSecret/binary,OtherPartsOfURL/binary}
raw_user(URL)when is_binary(URL)->?FUNCTION_NAME(binary_to_list(URL));
raw_user(URL)when is_list(URL)-> 
              case string:str(URL,"@") of
              0-> {<<>>,URL};% no user in URL
              N-> UserSize=8*(N-1),
                  <<User:UserSize,"@",Tail/bitstring>> = list_to_binary(URL),
                  {<<User:UserSize>>,Tail}
              end;
raw_user(Term) -> Term. % at error parsing just return Term with error for next handing.

test_raw_user1()->raw_user("clientjiberish:client/secretjiberish@api.example.com/users?username=tralala").
test_raw_user2()->raw_user("api.example.com/users?username=tralala").

% return {UserName/binary,ClientAndSecret/binary}
user_name(RawUser)when is_binary(RawUser)->?FUNCTION_NAME(binary_to_list(RawUser));
user_name(RawUser) when is_list(RawUser)-> 
              case string:str(RawUser,":") of
              0-> {{bad_user_client},RawUser};% no user in URL
              N-> NameSize=8*(N-1),
                  <<Name:NameSize,":",ClientAndSecret/bitstring>> = list_to_binary(RawUser),
                  {<<Name:NameSize>>,ClientAndSecret}
              end;

?FUNCTION_NAME(Term) -> Term. % at error parsing just return Term with error for next handing.

% return {Client/binary,Secret/binary}
user_client(RawClient) when is_binary(RawClient)->?FUNCTION_NAME(binary_to_list(RawClient));
user_client(RawClient) when is_list(RawClient)-> 
              case string:str(RawClient,"/") of
              0-> {{bad_user_secret},RawClient};% no user in URL
              N-> ClientSize=8*(N-1),
                  <<Client:ClientSize,"/",Secret/bitstring>> = list_to_binary(RawClient),
                  {<<Client:ClientSize>>,Secret}
              end.

user("")      ->#{};
user(<<"">>)  ->#{};

user(RawUser) when is_binary(RawUser)-> %{_Scheme,TailURL}=scheme(URL),
             %{RawUser,_PathAndOthers}=raw_user(RawUser),

             {User,UserRest}=user_name(RawUser),
             {Client,Secret}=user_client(UserRest),
 
             #{user=>User,
               user_client=>Client,
               user_secret=>Secret  
              };

?FUNCTION_NAME(Term) -> Term. % at error parsing just return Term with error for next handing.

% input like "8000/path/?var1=val2" or "path/?var1=val2"
raw_port({WPort,PortAndPath})when is_binary(PortAndPath)->?FUNCTION_NAME({WPort,binary_to_list(PortAndPath)});
raw_port({with_port,PortAndPath})when is_list(PortAndPath)-> 
              case string:str(PortAndPath,"/") of
              0-> {<<"80">>,PortAndPath};% return default HTTP port
              N-> PortSize=8*(N-1),
                  <<Port:PortSize,"/",Tail/bitstring>> = list_to_binary(PortAndPath),
                  try list_to_integer(binary_to_list(<<Port:PortSize>>)) of
                   Int when is_integer(Int) -> {<<Port:PortSize>>,Tail} % return Port/binary
                  catch
                   _:_:_ -> {error,{bad_port,<<Port:PortSize>>}} % return error and it description
                  end
              end;

raw_port(Path) when is_binary(Path)-> ?FUNCTION_NAME(binary_to_list(Path));
raw_port(Path) when is_list(Path)-> {<<"80">>,Path}.% port not parsed, set default HTTP port 

test_raw_port1()->raw_port("8000/path/?var1=val2").
test_raw_port2()->raw_port("/path/?var1=val2"). % error for cases like http://host.co//path/?var1=val2
test_raw_port3()->raw_port("path/?var1=val2").

%return {Host/binary,OtherPartsOfURL/binary}
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
                               {<<Host:HostSize>>,{with_port,Tail}};
              _ ->   
                     {error,{bad_host,HOST_AND_OTHERS}} % bad cases
              end.


test_host1()->host("aa.com/path").               
test_host2()->host("aa.com:8000").               
test_host3()->host("aa.com:8000/path/?var1=value1&var2=value2").               

test_host4()->host("aa.com").               
test_host5()->host("aa.com/").               

%return {Port/binary,OtherPartsOfURL/binary}
% rewrite it
port({With_Port,PORT_AND_OTHERS})when is_binary(PORT_AND_OTHERS)->?FUNCTION_NAME({With_Port,binary_to_list(PORT_AND_OTHERS)});
port({with_port,PORT_AND_OTHERS})when is_list(PORT_AND_OTHERS)-> 
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
                                _:_:_ ->{error,{bad_port,PORT_AND_OTHERS}} 
                                     %{<<"80">>,list_to_binary(PORT_AND_OTHERS)} % not found port 
                               end 
              %_ ->  {error,{bad_port,PORT_AND_OTHERS}} % bad cases
              end;

port(PATH_AND_OTHERS)when is_list(PATH_AND_OTHERS)-> 
 {<<"">>,list_to_binary(PATH_AND_OTHERS)}.

% all failed
test_port1()-> port("8000/path/?var1=value1&var2=value2").%passed               
test_port2()-> port("path/?var1=value1&var2=value2").% not passed               
test_port3()-> port("8000/"). % passed              
test_port4()-> port("8000").% passed               

%return {Path/binary,OtherPartsOfURL/binary}
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
              end;

path(Term)->{error,{bad_path,Term}}.

test_path1()-> path("?var1=value1&var2=value2").% passed              
test_path2()-> path("path/some/file/?var1=value1&var2=value2"). % passed              
test_path3()-> path("path/some/"). %passed              
test_path4()-> path("path/some").  %passed             

%return Arguments/map
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
                          };

?FUNCTION_NAME(Term)->{error,{bad_arguments,Term}}.

test_arguments1()-> arguments(<<"var1=value1&var2=value2">>).%passed
test_arguments2()-> arguments(<<"var1=value1&var2=">>).
test_arguments3()-> arguments(<<"var1=value1&var2">>).
test_arguments4()-> arguments(<<"var1=value1&">>).% passed
test_arguments5()-> arguments(<<"var1=value1">>). % passed
% depricated
%p(URL) when is_binary(URL) -> 
%                               L=do_chains([scheme,host,port,path,arguments],URL),
%                               %io:format("parsed arguments ~p~n",[len(L)]),
%                               %[Scheme,Host,Port,Path,Args|_]=L,
%
%                               try len(L) of
%                                      N when N >= 5 -> [Scheme,Host,Port,Path,Args|_]=L,
%                                                       #{scheme=>Scheme,host =>Host,port=>Port,path=>Path,args=>Args};
%                                      AtArg -> #{arg_name(AtArg) => {error,{bad_parsing,{n,AtArg},{l,L},URL}}} 
%                                     catch
%                                      _:_:_ -> #{error => {error ,{bad_parsing,URL}}}  
%                                     end. 
%
%test_p1()->p(<<"https://aa.com:8000/path/?var1=value1&var2=value2">>).
%test_p2()->p(<<"https://aa.com:8000/path/?var1=value1&var2=">>).

%parse_with_user(URL)-> UrlList=maps:to_list(p(URL)),
%             UserMap=user(URL),
%             UrlMap=fold(UrlList),
%             io:format("~p:~p User=~p URL=~p ~n",[?MODULE,?LINE,UserMap,UrlMap]),
%             maps:merge(UserMap,UrlMap).

%parse(URL)-> UrlList=maps:to_list(p(URL)),
%             %UserMap=user(URL),
%             UrlMap=fold(UrlList),
%             io:format("~p:~p URL=~p ~n",[?MODULE,?LINE,UrlMap]),
%             UrlMap. 

%fold(VL)->fold(VL,[],[]).
%
%fold([],Acc1,Acc2)->{maps:from_list(Acc1),Acc2};
%fold([H|T],Acc1,Acc2) -> case H of
%                          {Key,{error,Err}} -> ?FUNCTION_NAME(T,Acc1,[{Key,{error,Err}}|Acc2]);
%                                    A -> ?FUNCTION_NAME(T,[A|Acc1],Acc2)  
%                         end.


len([])->0;
len([_])->1;
len([_,T])->1+len(T);
len([_|T])->1+len(T);
len(_)->1.

arg_name(1)->scheme;
arg_name(2)->host;
arg_name(3)->port;
arg_name(4)->path;
arg_name(5)->arguments;
arg_name(_)->unknown.

% User Auth in URL is not supporting yet

test_user()-> user(<<"https://clientjiberish:client/secretjiberish@api.example.com/users?username=tralala">>).

parse(URL)-> 
 {Scheme,SchemeCDR}  = scheme(URL),
 {RawUser,UserCDR}   = raw_user(SchemeCDR),
 {Host,HostCDR}      = host(UserCDR),
 {RawPort,PortCDR}   = raw_port(HostCDR),
 {Path,RawArguments} = path(PortCDR),
 Arguments           = arguments(RawArguments),
%
% io:format("~p:~p raw_user=~p ~n",[?MODULE,?LINE,RawUser]),  
%
#{ scheme => Scheme,
%   raw_user   => RawUser,
   host       => Host,
%   raw_port   => RawPort,
   port       => try list_to_integer(binary_to_list(RawPort)) of
                   Int when is_integer(Int) -> RawPort % or do list_to_binary(integer_to_list(Int)) for more strong and safe checking
                  catch
                  _:_:_ -> <<"80">> % default HTTP Port
                 end,
   path       => Path,
   arguments  => Arguments,
   user => user(RawUser)
 }.


t1()->parse(<<"aa.com:8888/path/?v1=a&v2=b">>).
t2()->parse(<<"://aa.com:8888/path/?v1=a&v2=b">>).
t3()->parse(<<":/aa.com:8888/path/?v1=a&v2=b">>).
t4()->parse(<<"/aa.com:8888/path/?v1=a&v2=b">>).

parse_1_test_()-> ?_assert(#{arguments =>{{error,{name_without_value,"var2"}},<<>>},
                  host => <<"aa.com">>,
                  path => <<"path/">>,
                  port => <<"8888">>,
                  scheme => <<"https">>,
                  user => #{}
    }
    =:= parse(<<"https://aa.com:8888/path/?var1=value1&var2=">>)). 

parse_2_test_()-> ?_assert(#{arguments => {error, {bad_arguments,{bad_path,{bad_port,<<"$$bad_port$$">>}}}},
                 host   => <<"aa.com">>,
                 path   => error,
                 port   => <<"80">>,
                 scheme => <<"https">>,
                 user   => #{}} 
                =:= parse(<<"https://aa.com:$$bad_port$$/path/?var1=value1&var2=">>)).


parse_3_test_()-> ?_assert(#{arguments => {error,{bad_arguments,{bad_path,{bad_port,<<>>}}}},
                  host      => <<"aa.com">>,
                  path      => error,
                  port      => <<"80">>,
                  scheme    => <<"https">>,
                  user      => #{}} 
                =:= parse(<<"https://aa.com:/path/?var1=value1">>)).

parse_4_test_()-> ?_assert(#{arguments => {#{"var1" => "value1"},<<>>},
                  host      => <<"aa.com">>,
                  path      => <<"path/">>,
                  port      => <<"80">>,
                  scheme    => <<"https">>,
                  user      => #{}} 
                 =:= parse(<<"https://aa.com/path/?var1=value1">>)).



parse_5_test_()->  ?_assert(#{arguments => {#{"username" => "tralala"},<<>>}, 
                   host => <<"api.example.com">>,
                   path => <<"users">>,
                   port => <<"8888">>,
                   scheme => <<"https">>,
                   user => #{user        => <<"clientjiberish">>,
                             user_client => <<"client">>, 
                             user_secret => <<"secretjiberish">>}
                  } 
                  =:= parse("https://clientjiberish:client/secretjiberish@api.example.com:8888/users?username=tralala")).

parse_6_test_()->  ?_assert(#{arguments => {#{"username" => "tralala"},<<>>},
                   host => <<"api.example.com">>,
                   path => <<"users">>,
                   port => <<"80">>,
                   scheme => <<"https">>,
                   user => #{user => <<"clientjiberish">>,
                             user_client => <<"client">>,
                             user_secret => <<"secretjiberish">>
                            }
                  }
                  =:= parse("https://clientjiberish:client/secretjiberish@api.example.com/users?username=tralala")).


parse_7_test_()-> ?_assert(#{arguments => {#{"var1" => "tralala","var2" => "value2"},<<>>}, 
                     host => <<"api.example.com">>,
                     path => <<"users">>,
                     port => <<"80">>,
                     scheme => <<"https">>,
                     user => #{}
                    } 
                   =:= parse("https://api.example.com/users?var1=tralala&var2=value2")).

parse_8_test_()->  ?_assert(
                 #{arguments => {#{"var1" => "tralala","var2" => "value2"},<<>>},
                   host   => <<"api.example.com">>,
                   path   => <<"users">>,
                   port   => <<"8888">>,
                   scheme => <<"https">>, 
                   user   => #{}
                  }
                 =:= parse("https://api.example.com:8888/users?var1=tralala&var2=value2")).

parse_9_test_()->?_assert( #{arguments => {#{"var1" => "value1"},<<>>},
                             host   => <<"aa.com">>,path => <<"path/">>,port => <<"8888">>,
                             scheme => <<"https">>,
                             user   => #{}
                            } 
                           =:=parse("https://aa.com:8888/path/?var1=value1&")).

parse_10_test_()->?_assert( #{arguments => {#{},<<>>},
                              host   => <<"aa.com">>,path => <<"path/">>,port => <<"8888">>,
                              scheme => <<"https">>,
                              user   => #{}
                             } 
                            =:= parse(<<"https://aa.com:8888/path/?">>)).
     