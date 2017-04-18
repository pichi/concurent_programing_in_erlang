-module(frequency).

-export([start/0, stop/0, allocate/0, deallocate/0]).

-export([init/1]).

start() ->
    spawn(?MODULE, init, [get_frequencies()]).

get_frequencies() ->
    [10, 11, 12, 13, 42].

init(L) ->
    register(?MODULE, self()),
    loop({L, #{}}).

allocate() ->
    Pid = whereis(?MODULE),
    ?MODULE ! {request, self(), allocate},
    receive
        {result, Pid, Result} -> Result
    end.

deallocate() ->
    Pid = whereis(?MODULE),
    ?MODULE ! {request, self(), deallocate},
    receive
        {result, Pid, Result} -> Result
    end.

stop() ->
    ?MODULE ! stop.

loop(State) ->
    receive
        {request, From, allocate} ->
            {Result, NewState} = allocate(From,State),
            io:format("(~p): Allocation requested from ~p: ~p~n",
                      [self(), From, Result]),
            From ! {result, self(), Result},
            loop(NewState);
        {request, From, deallocate} ->
            {Result, NewState} = deallocate(From, State),
            io:format("(~p): Deallocation requested from ~p: ~p~n",
                      [self(), From, Result]),
            From ! {result, self(), Result},
            loop(NewState);
        stop -> loop(State);
        Msg ->
            io:format(
              "~p(~p) received unknown message:~p~n",
              [?MODULE, self(), Msg]),
            loop(State)
    end.

allocate(Pid, {[H|T], M} = S) ->
    case maps:is_key(Pid, M) of
        false ->
            {{ok, H}, {T, maps:put(Pid, H, M)}};
        true  ->
            {{error, already_allocated}, S}
    end;
allocate(_, {[], _} = S) ->
    {{error, all_allocated}, S}.

deallocate(Pid, {L, M} = S) ->
    case maps:is_key(Pid, M) of 
        true  ->
            {ok, {[maps:get(Pid, M)|L], maps:remove(Pid, M)}};
        false ->
            {{error, not_allocated}, S}
    end.
