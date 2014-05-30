-module(process).
-export([start/0, loop/1, send/1]).
-record(state, {
        data
        }).


start() -> 
    State = #state{data = default},
    Pid = spawn(process, loop, [State]),
    register(destprocess, Pid).

loop(State) ->
    timer:sleep(1000),
    receive
        accessor -> io:format("Data is ~p~n", [State#state.data]);
        terminate -> exit(reason);
        Msg -> io:format("Received ~p~n", [Msg]),
            loop(State#state{data = Msg})
    end,
    loop(State).

send(Message) ->
    destprocess ! Message.


%%userId, userName, level, resources
