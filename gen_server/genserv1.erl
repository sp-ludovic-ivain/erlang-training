-module(genserv1).
-author("Ludo").

-behaviour(gen_server).

%% API
-export([start/1,
  allocate/0,
  deallocate/0,
  get_busy/0,
  add_frequency/1,
  ping/0,
  get_available/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    frequencies,
    allocated
}).

%%%%%%%%%%%%%%%%%%% API calls %%%%%%%%%%%%%%%%%%%%%%
start(Frequencies) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Frequencies, []).

allocate() ->
  gen_server:call(?SERVER, allocate).

deallocate() ->
  gen_server:call(?SERVER, deallocate).

get_available() ->
  gen_server:call(?SERVER, get_available).

get_busy() ->
  gen_server:call(?SERVER, get_busy).

add_frequency(Freq) ->
  gen_server:call(?SERVER, {add_frequency, Freq}).

ping() ->
  gen_server:call(?SERVER, ping).

%%%%%%%%%%%%%%%%%%%% Gen Serv callbacks %%%%%%%%%%%%%%%%%%%%%

init(Frequencies) ->
  State = #state{
    frequencies = Frequencies,
    allocated = []
  },
  {ok, State}.

handle_call(allocate, {Pid, _}, State) ->
  Freqs = State#state.frequencies,
  Allocated = State#state.allocated,
  {Reply, NewState} = allocate(Freqs, Allocated, Pid),
  {reply, Reply, NewState};

handle_call(deallocate, {Pid, _}, State) ->
  Freqs = State#state.frequencies,
  Allocated = State#state.allocated,
  {Reply, NewState} = deallocate(Freqs, Allocated, Pid),
  {reply, Reply, NewState};

handle_call(get_available, _, State) ->
  Reply = State#state.frequencies,
  {reply, Reply, State};

handle_call(get_busy, _, State) ->
  Reply = State#state.allocated,
  {reply, Reply, State};

handle_call({add_frequency, Freq}, _, State) ->
  Freqs = State#state.frequencies,
  Allocated = State#state.allocated,
  {Reply, NewState} = add_frequency(Freqs, Allocated, Freq),
  {reply, Reply, NewState};

handle_call(ping, {Pid, _}, State) ->
  Freqs = State#state.frequencies,
  Allocated = State#state.allocated,
  {Reply, NewState} = renew_frequency(Freqs, Allocated, Pid),
  {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({frequency_expire, Freq}, State) ->
  NewState = delete_if_expired_frequency(Freq, State),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%% Private internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allocate([], Allocated, _) ->
  State = #state{
    frequencies = [],
    allocated = Allocated
  },
  Reply = {error, no_frequency},
  {Reply, State};

allocate([Freq | Free], Allocated, Pid) ->
  State = #state{
    frequencies = Free,
    allocated = Allocated ++ [{Pid, Freq, false}]
  },
  Reply = {ok, Freq},
  set_frequency_expire(Freq, 5000),
  {Reply, State}.

deallocate(Freqs, [], _) ->
  State = #state{
    frequencies = Freqs,
    allocated = []
  },
  Reply = {error, no_pid_match},
  {Reply, State};

deallocate(Freqs, [{Pid, Freq, _} | Allocated], Pid) ->
  State = #state{
    frequencies = Freqs ++ [Freq],
    allocated = Allocated
  },
  {ok, State}.

add_frequency(Freqs, Allocated, Freq) ->
  State = #state{
    frequencies = Freqs,
    allocated = Allocated
  },
  case lists:keysearch(Freq, 2, Allocated) of
    false ->
        case lists:member(Freq, Freqs) of
          false -> push_frequency(Freqs, Allocated, Freq);
          _ -> {already_exists, State}
        end;
    _ -> {already_used, State}
  end.

push_frequency(Freqs, Allocated, Freq) ->
  NewFreqs = lists:append([Freq], Freqs),
  State = #state{
    frequencies = NewFreqs,
    allocated = Allocated
  },
  {added, State}.

delete_if_expired_frequency(Freq, State) ->
  Freqs = State#state.frequencies,
  Allocated = State#state.allocated,
  case lists:keysearch(Freq, 2, Allocated) of
    {value, {_, Freq, false}} ->
        AvailFreq = State#state.frequencies,
        {_, _, NewAllocated} = lists:keytake(Freq, 2, Allocated),
        NewFreqs = lists:append([Freq], AvailFreq),
        NewState = #state{frequencies = NewFreqs, allocated = NewAllocated},
        NewState;
    {value, {Pid, Freq, _}} ->
        NewAllocated = lists:keyreplace(Freq, 2, Allocated, {Pid, Freq, false}),
        set_frequency_expire(Freq, 5000),
        NewState = #state{frequencies = Freqs, allocated = NewAllocated},
        NewState;
    _ -> State
  end.

renew_frequency(Freqs, Allocated, Pid) ->
  case lists:keysearch(Pid, 1, Allocated) of
    {value, {Pid, Freq, _}} ->
      NewAllocated = lists:keyreplace(Pid, 1, Allocated, {Pid, Freq, ping}),
      State = #state{
        frequencies = Freqs,
        allocated = NewAllocated
      },
      {ok, State};
    _ ->
      State = #state{
        frequencies = Freqs,
        allocated = Allocated
      },
      {frequency_not_assigned, State}
  end.

set_frequency_expire(Freq, Ttl) ->
  erlang:send_after(Ttl, self(), {frequency_expire, Freq}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
