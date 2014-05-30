-module(message).
-export([start/2, master/0, slave/0]).

start(Message, Amount) ->
    Pid1 = spawn(message, master, []),
    Pid2 = spawn(message, slave, []),
    io:format("Process 1 pid = ~p~n", [Pid1]),
    io:format("Process 2 pid = ~p~n", [Pid2]),
    global:register_name(master, Pid1),
    global:register_name(slave, Pid2),
    send_message(Pid1, Pid2, {Message, Amount}).

send_message(FromPid, _, {_, 0}) ->
    io:format("end ~n");
    %%  FromPid ! kill;
  %%  ToPid ! kill;

send_message(FromPid, ToPid, {Message, Amount}) ->
    FromPid ! {send, ToPid, {Message, Amount}},
    send_message(FromPid, ToPid, {Message, Amount -1}).

master() ->
    receive
        {send, Receiver, {Message, Amount}} ->
            io:format("Send (~p) '~p' to process ~p~n", [Amount, Message, Receiver]),
            Receiver ! {message, self(), {Message, Amount}};
        {response, Sender, Message} ->
            io:format("Receive response from process ~p : ~p~n", [Sender, Message]);
        kill ->
            io:format("Exit process pid: ~p~n", [self()]),
            Receiver = global:whereis_name(slave),
            Receiver ! kill,
            exit(reason);
        Other -> io:format("Bad args: ~p~n", [Other])
    end,
    master().

slave() -> 
    receive
        {message, SenderPid, {Message, Amount}} ->
            io:format("Receive message (~p) from process ~p : ~p~n", [Amount, SenderPid, Message]),
            SenderPid ! {response, self(), "Ack"};
        kill ->
            io:format("Exit process pid ~p~n", [self()]),
            exit(reason);
        Other -> io:format("Bad args ~p~n",[Other])
    end,
    slave().






