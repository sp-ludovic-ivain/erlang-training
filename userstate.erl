-module(userstate).
-export([new_user/1, loop/1, getId/1, getName/1, setName/2,
         info/1, setLevel/2, getLevel/1, getResources/1,
         setResources/3, kill/1]).

-record(resources, {
        gold,
        food
        }).
-record(state, {
        user_id,
        name,
        level,
        resources
        }).


new_user(UserId) ->
    State = #state{
            user_id = UserId,
            name = "Default",
            level = 1,
            resources = #resources{
                gold = 0,
                food = 0
                }
            },
    UserKey = {user, UserId},
    Pid = spawn(userstate, loop, [State]),
    io:format("New user created, id: ~p, pid: ~p~n", [UserId, Pid]),
    global:register_name(UserKey, Pid),
    UserKey.

loop(State) ->
    timer:sleep(1000),
    receive
        getId ->
            io:format("User id: ~p ~n", [State#state.user_id]);

        getName ->
            io:format("User name: ~p~n", [State#state.name]);

        {setName, Name} ->
            io:format("User rename to ~p ~n", [Name]),
            loop(State#state{name = Name});

        getLevel -> 
            io:format("User level: ~p ~n", [State#state.level]);

        {setLevel, Level} ->
            io:format("User is now level ~p ~n", [Level]),
            loop(State#state{level = Level});

        getResources -> 
            io:format("User resources: ~n- Gold: ~p~n- Food: ~p~n", [(State#state.resources)#resources.gold, (State#state.resources)#resources.food]);

        {setResources, {Type, Value}} ->
            case Type of
                g -> updateResources(State, gold, Value);
                f -> updateResources(State, food, Value);
                _ -> io:format("Unknow resources")
            end;

        display ->
            io:format("User id: ~p ~n", [State#state.user_id]),
            io:format("User name: ~p ~n", [State#state.name]),
            io:format("User level: ~p ~n", [State#state.level]),
            io:format("User resources: ~n- Food: ~p~n- Gold: ~p~n", [(State#state.resources)#resources.food, (State#state.resources)#resources.gold]);

        {terminate, _} -> exit(reason);
        _ -> io:format("unknow command ~n")
    end,
    loop(State).

updateResources(State, Type, Value) ->
    io:format("Update user ~p to ~p~n", [Type, Value]),
    Resources = State#state.resources,
    case Type of
        gold -> NewResources = Resources#resources{gold = Value};
        food -> NewResources = Resources#resources{food = Value}
    end,
    loop(State#state{resources = NewResources}).

kill(UserKey) -> send(UserKey, terminate).

info(UserKey) ->
    send(UserKey, display).

getId(UserKey) -> send(UserKey, getId).

getName(UserKey) -> send(UserKey, getName).
setName(UserKey, Name) -> send(UserKey, setName, Name).

getLevel(UserKey) -> send(UserKey, getLevel).
setLevel(UserKey, Level) -> send(UserKey, setLevel, Level).

getResources(UserKey) -> send(UserKey, getResources).
setResources(UserKey, Type, Value) -> send(UserKey, setResources, {Type, Value}).

send(UserKey, Method) ->
    sendMessage(UserKey, Method).

send(UserKey, Method, Args) ->
    Message = {Method, Args},
    sendMessage(UserKey, Message).

sendMessage(UserKey, Message) ->
    Pid = global:whereis_name(UserKey),
    Pid ! Message.

