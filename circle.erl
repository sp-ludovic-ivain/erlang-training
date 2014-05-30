-module(circle).
-export([create/2, loop/0]).

create(Num, _) when Num < 2 ->
    io:format("Can not create a circle with less than 3 nodes~n"); 
create(_, Repeat) when Repeat < 1 ->
    io:format("Can not send less then 1 message");

create(Num, Repeat) ->
    AllPid = create_process(Num, []),
    [FirstPid | OtherPid] = AllPid,
    io:format("First node Pid : ~p~n", [FirstPid]),
    [io:format("Other Pid: ~p~n", [Pid]) || Pid <- OtherPid],
    FirstPid ! {send, FirstPid, OtherPid, Repeat}.

create_process(0, Acc) -> Acc;
create_process(Num, Acc) ->
    Pid = spawn(circle, loop, []),
    create_process(Num -1, Acc ++ [Pid]).

loop() ->
  timer:sleep(1000),
    receive
        %% case first node, receive last message -> die
        {send, FirstPid, _, 0} when self() == FirstPid ->
            io:format("Kill process ~p~n", [self()]);

        %% case first node, first to send a message to others
        {send, FirstPid, [PidToSend | OtherPid], Repeat} when self() == FirstPid ->
            io:format("First process, begin circle #~p ~n", [Repeat]),
            io:format("~p send message #~p to ~p~n", [self(), Repeat, PidToSend]),
            PidToSend ! {send, FirstPid, OtherPid ++ [self()], Repeat},
            loop();

         %% case last node last loop -> die
        {send, FirstPid, [PidToSend | OtherPid], 1} when FirstPid == PidToSend ->
          PidToSend ! {send, FirstPid, OtherPid ++ [self()], 0},
          io:format("~p send message #~p to ~p~n", [self(), 1, PidToSend]),
          io:format("Kill process ~p~n", [self()]);

         %% case last node -> loop
        {send, FirstPid, [PidToSend | OtherPid], Repeat} when FirstPid == PidToSend ->
            PidToSend ! {send, FirstPid, OtherPid ++ [self()], Repeat - 1},
            io:format("~p send message #~p to ~p~n", [self(), Repeat, PidToSend]),
            loop();

        %% case node lambda, last loop -> die
        {send, FirstPid, [PidToSend | OtherPid], 1} ->
            PidToSend ! {send, FirstPid, OtherPid ++ [self()], 1},
            io:format("~p send message #~p to ~p~n", [self(), 1, PidToSend]),
            io:format("Kill process ~p~n", [self()]);

        %% case node lambda, not first one, not last one -> loop
        {send, FirstPid, [PidToSend | OtherPid], Repeat} ->
            PidToSend ! {send, FirstPid, OtherPid ++ [self()], Repeat},
            io:format("~p send message #~p to ~p~n", [self(), Repeat, PidToSend]),
            loop();

        Other ->
            io:format("Bad args in receive loop: ~p~n", [Other])
    end.



