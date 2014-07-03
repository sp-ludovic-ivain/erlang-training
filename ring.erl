-module(ring).
-export([start/3, start_process/1, start_process/2]).

start(M, N, Message) ->
  Pid = spawn(ring, start_process, [N]),
  Pid ! {message, Message, M},
  ok.

start_process(Count) ->
  % This is the first spawned process - send its
  % pid down the chain so the last process knows who its
  % next pid is.
  io:format("~p: Spawned ~p~n", [self(), Count]),
  Pid = spawn(ring, start_process, [Count-1, self()]),
  loop(Pid).

start_process(0, Last) ->
  % This is the last process
  io:format("~p: Linking to last ~p~n", [self(), Last]),
  loop(Last);
start_process(Count, Last) ->
  io:format("~p: Spawned ~p~n", [self(), Count]),
  Pid = spawn(ring, start_process, [Count-1, Last]),
  loop(Pid).

loop(NextPid) ->
  receive
    {message, _,   0} -> true;
    {message, Msg, M} ->
      io:format("~p (~p) ~p~n", [Msg, self(), M]),
      NextPid ! {message, Msg, M-1},
      loop(NextPid)
  end.
