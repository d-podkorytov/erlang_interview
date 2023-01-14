-module(fact_eu2).
-export([fact/1,fact_l/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-define(ERROR_INFO(ERROR_TYPE,X), {error,ERROR_TYPE,[N]}).

-spec fact(Inp) -> Result when
  Result :: integer | {error,Error,Inp},
  Error  :: binary,
  Inp    :: term().

fact(N) when is_float(N) ->?ERROR_INFO(<<"float in input">>,N);
fact(0) -> 1;
fact(1) -> 1;
fact(N) when is_integer(N) andalso N>=1  -> N*fact(N-1);
fact(N) when N <0 andalso is_integer(N)  -> ?ERROR_INFO(<<"negative input">>,N);
fact(N) -> ?ERROR_INFO(<<"bad input">>,N).

loop()->
 receive
  {FromPid,N}  -> FromPid!{FromPid,f(N)}  
 end, 
 loop().

fact(0,Acc) -> Acc;
fact(1,Acc) -> Acc;
fact(N,Acc) -> fact((N-1),N*Acc).

f(N)->fact(N,1).

c(Pid,N)-> 
 Pid!{self(),N},
 receive
  {FromPid,Res}  -> Res  
 after 1000-> timeout
 end.

c(Pid,N,ReplyTo)-> Pid!{ReplyTo,N}.


cv(PidL,N) -> L=lists:seq(N),
 lists:foldl(fun(A,Acc)->
            CpuNo = Acc rem 4,
             spawn(fun() -> c(lists:nth(CpuNo,PidL),A,self()) end),
            Acc+1
           end, L). 

% recursion free factorial based om list generation and fold it next
fact_l(N) when is_float(N) ->?ERROR_INFO(<<"float in input">>,N);
fact_l(N) when N <0 andalso is_integer(N)  -> ?ERROR_INFO(<<"negative input">>,N);
fact_l(N) when not is_integer(N)  -> ?ERROR_INFO(<<"bad input">>,N);

fact_l(0) -> 1;
fact_l(N) -> L=lists:seq(1,N),
             lists:foldl(fun(A,Acc)-> A*Acc end,1, L).

% ======= TESTS SECTION =================

-ifdef(TEST).
% Run all module's functions with names like test_* 

% tests type 1
all_tests()-> lists:foldl(fun({Func,Arity},Acc)->
                              case {Arity,string:split(atom_to_list(Func),"_")}  of 
                                {0,["test"|_]} -> [{Func,apply(?MODULE,Func,[])}|Acc];
                                _ -> %it is not test_* function and do not change Acc
                                     Acc
                              end
                          end,
                        [],
                        ?MODULE:module_info(exports)
                       ).

all_tests2(L)-> lists:map(fun({Inp,Out})-> 
                            %io:format("~p:~p ~p ~p ",[?MODULE,?LINE,?FUNCTION_NAME,{Inp,Out,fact(Inp)}]), 
                            {Inp,Out,fact(Inp)==Out} 
                          end,L
                         ).

test_2()-> all_tests2([ {0,1},
                            {1,1},
                            {2,2},
                            {3,6},
                            {-1,{error, <<"negative input">>,[-1] }},
                            {[],{error, <<"bad input">>,[[]] }},
                            {1.1,{error, <<"float in input">>,[1.1] }}
                          ]).


test_1()-> lists:map(fun(A) -> {A,fact(A)} end,[1,2,3,4,44,0,6.0,-1,[],["bad"],<<"bad">>]).

%tests type 2 based on eunit
fact_test_() ->
       [?_assert(fact(0)   =:= 1),
	?_assert(fact(1)   =:= 1),
	?_assert(fact(2)   =:= 2),
	?_assert(fact(3)   =:= 6),
	?_assert(fact(4)   =:= 24),
	?_assert(fact(-1)  =:= {error, <<"negative input">>,[-1] }),
	?_assert(fact([])  =:= {error, <<"bad input">>     ,[[]] }),
        ?_assert(fact(1.1) =:= {error, <<"float in input">>,[1.1]}), 
	?_assert(fact(10)  =:= 3628800),
        ?_assert(fact(20)  =:= 2432902008176640000),
 
        ?_assert(fact_l(0)   =:= 1),
	?_assert(fact_l(1)   =:= 1),
	?_assert(fact_l(2)   =:= 2),
	?_assert(fact_l(3)   =:= 6),
	?_assert(fact_l(4)   =:= 24),
	?_assert(fact_l(-1)  =:= {error, <<"negative input">>,[-1] }),
	?_assert(fact_l([])  =:= {error, <<"bad input">>     ,[[]] }),
        ?_assert(fact_l(1.1) =:= {error, <<"float in input">>,[1.1]}), 
	?_assert(fact_l(10)  =:= 3628800),
        ?_assert(fact_l(20)  =:= 2432902008176640000) 
       
       ].
-endif.
% ========= END TESTS ===================
