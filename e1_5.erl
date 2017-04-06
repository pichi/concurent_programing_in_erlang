-module(e1_5).

-export([server/1, proxy/1, check/2]).

-export([loop/1, proxy_init/2]).

-export([test/0]).

% 1> c(e1_5).
% {ok,e1_5}
% 2> P = e1_5:proxy(3).
% <0.67.0>
% 3> e1_5:check(P, "Madam, I'm Adam.").
% <0.68.0> got request from <0.60.0>
% true
% 4> e1_5:check(P, "Able was I ere I saw Elba").
% <0.69.0> got request from <0.60.0>
% true
% 5> e1_5:check(P, "Never odd or even.").       
% <0.70.0> got request from <0.60.0>
% true
% 6> e1_5:check(P, "xyz").               
% <0.68.0> got request from <0.60.0>
% false
% 7> e1_5:check(P, "Abba").
% <0.69.0> got request from <0.60.0>
% true
% 8> self().
% <0.60.0>

proxy(N) when is_integer(N), N > 0 ->
    Pid = spawn(?MODULE, proxy_init, [self(), N]),
    receive
        {ok, Pid} -> Pid
    end.

check(Srv, String) ->
    Srv ! {check, self(), String},
    receive
        {result, String, Result} -> Result
    end.

server(Recipient) when is_pid(Recipient) ->
    spawn(?MODULE, loop, [Recipient]).

proxy_init(Parent, N) ->
    Pids = [server(self()) || _ <- lists:seq(1, N)],
    Parent ! {ok, self()},
    proxy(Pids, []).

proxy([], Pids) -> proxy(lists:reverse(Pids) , []);
proxy([H|T] = A, B) ->
    receive
        {check, _, _} = Msg ->
            H ! Msg,
            proxy(T, [H|B]);
        stop ->
            [ Pid ! stop || Pid <- A ++ B ];
        Msg ->
            io:format("~p received unknown message: ~p~n", [self(), Msg]),
            proxy(A, B)
    end.

loop(Recipient) ->
    receive
        {check, String} ->
            Recipient ! {result, String, palindrome(String)},
            loop(Recipient);
        {check, From, String} ->
            io:format("~p got request from ~p~n", [self(), From]),
            From ! {result, String, palindrome(String)},
            loop(Recipient);
        stop -> ok;
        Msg ->
            io:format("~p received unknown message: ~p~n", [self(), Msg]),
            loop(Recipient)
    end.

palindrome(String) ->
    NS = normalise(String),
    NS =:= lists:reverse(NS).

normalise(String) ->
    normalise(String, []).

normalise([], Acc) -> Acc;
normalise([H|T], Acc) when H >= $a, H =< $z ->
    normalise(T, [H|Acc]);
normalise([H|T], Acc) when H >= $A, H =< $Z ->
    normalise(T, [H+($a-$A)|Acc]);
normalise([_|T], Acc) ->
    normalise(T, Acc).

call_server(Srv, String) ->
    Srv ! {check, String},
    receive
        {result, String, Result} -> Result
    end.

test() ->
    Srv = server(self()),
    true = call_server(Srv, "Able was I ere I saw Elba"),
    false = call_server(Srv, "Able wxs I ere I saw Elba"),
    true = call_server(Srv, "A man, a plan, a canal - Panama!"),
    false = call_server(Srv, "A an, a plan, a canal - Panama!"),
    true = call_server(Srv, "Madam, I'm Adam"),
    true = call_server(Srv, "Never odd or even"),
    true = call_server(Srv, "Doc, note: I dissent. A fast never prevents a fatness. I diet on cod"),
    true = call_server(Srv, "T. Eliot, top bard, notes putrid tang emanating, is sad; I'd assign it a name: gnat dirt upset on drab pot toilet."),
    Srv ! stop,
    ok.

