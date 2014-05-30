-module(circle).
-export([create/2, loop/0]).

create(Num, _) when Num < 3 -> 
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
  timer:sleep(100),
    receive
        %% case first node, last one to receive kill and die
        {kill, FirstPid, _, 0} when self() == FirstPid ->
            kill_process(self());

        %% case first node, first to send KILL cmd to others
        {_, FirstPid, [PidToSend | OtherPid], 0} when self() == FirstPid ->
            io:format("First process, begin circle #kill ~n"),
            send_to_next_process(kill, FirstPid, PidToSend, OtherPid, 0);

        %% case first node, first to send a message to others
        {Message, FirstPid, [PidToSend | OtherPid], Repeat} when self() == FirstPid -> 
            io:format("First process, begin circle #~p ~n", [Repeat]),
            send_to_next_process(Message, FirstPid, PidToSend, OtherPid, Repeat);

        %% case last node
        {Message, FirstPid, [PidToSend | OtherPid], Repeat} when FirstPid == PidToSend ->
            process_message(Message, FirstPid, PidToSend, OtherPid, Repeat - 1);

        %% case node lambda, not first one, not last one
        {Message, FirstPid, [PidToSend | OtherPid], Repeat} ->
            process_message(Message, FirstPid, PidToSend, OtherPid, Repeat);
        Other ->
            io:format("Bad args in receive loop: ~p~n", [Other])
    end,
    loop().

process_message(Message, FirstPid, PidToSend, OtherPid, Repeat) ->
    case Message of
        kill ->
          send_to_next_process(kill, FirstPid, PidToSend, OtherPid, 0),
          kill_process(self());
        send ->
          send_to_next_process(send, FirstPid, PidToSend, OtherPid, Repeat)
    end.

send_to_next_process(Message, FirstPid, PidToSend, OtherPid, Repeat) ->
    io:format("~p send '~p' message to ~p~n", [self(), Message, PidToSend]),
    PidToSend ! {Message, FirstPid, OtherPid ++ [self()], Repeat}.

kill_process(Pid) ->
  timer:sleep(1000),
  io:format("Kill process ~p~n", [Pid]),
  exit(reason).


