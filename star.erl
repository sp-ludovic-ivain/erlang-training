-module(star).
-export([start/2, loop/0, sendMessage/2]).

start(0, _) ->
  io:format("done~n", []);

start(N, M) ->
  timer:sleep(random:uniform(10) * 100),
  LoopPid = spawn(?MODULE, loop, []),
  spawn(?MODULE, sendMessage, [M, LoopPid]),
  start(N - 1, M).

sendMessage(0, Pid) ->
  Pid ! done;
sendMessage(M, Pid) ->
  timer:sleep(random:uniform(5) * 100),
  Pid ! M,
  sendMessage(M - 1, Pid).
loop() ->
  receive
    done ->
      io:format("* Process: ~10w, Message#: terminate!~n", [self()]);
    R ->
      io:format(": Process: ~10w, Message#: ~w ..~n", [self(), R]),
      loop()
  end.