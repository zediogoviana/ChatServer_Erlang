-module(loginManager).
-export([start/0,
    create_account/2, 
    close_account/2, 
    login/2, 
    logout/1, 
    online/0]).

% Interface Functions

start() -> 
    register(?MODULE, spawn(fun() -> loop(#{}) end)).

%create_account(Username, Passwd) ->
%    rpc({create_account, Username, Passwd}).

create_account(Username, Passwd) -> 
    ?MODULE ! {create_account, Username, Passwd, self()},
    receive {?MODULE, Res} -> Res 
    end.

close_account(Username, Passwd) -> 
    ?MODULE ! {close_account, Username, Passwd, self()},
    receive {?MODULE, Res} -> Res
    end.

login(Username, Passwd) -> 
    ?MODULE ! {login, Username, Passwd, self()},
    receive {?MODULE, Res} -> Res
    end.

logout(Username) -> 
    ?MODULE ! {logout, Username, self()},
    receive {?MODULE, Res} -> Res
    end.

online() -> 
    ?MODULE ! {online, self()},
    receive {?MODULE, Res} -> Res
    end.

% Process Functions

loop(Map) ->
    receive
        {create_account, U, P, From} -> 
            case maps:find(U, Map) of
                error -> 
                    From ! {?MODULE, ok},
                    loop(maps:put(U,{P, true}, Map));
                _ -> 
                    From ! {?MODULE, user_exists},
                    loop(Map)
            end;
        {close_account, U, P, From} ->
            case maps:find(U, Map) of
                {ok, {P, _}} ->
                    From ! {?MODULE, ok},
                    loop(maps:remove(U,Map));
                _ ->
                    From ! {?MODULE, invalid},
                    loop(Map)
            end;
        {login, U, P, From} ->
            case maps:find(U, Map) of
                {ok, {P, _}} -> 
                    From ! {?MODULE, ok},
                    loop(maps:update(U,{P,true},Map));
                _ ->
                    From ! {?MODULE, invalid},
                    loop(Map)
            end;
        {logout, U, From} ->
            case maps:find(U,Map) of
                {ok, {P,_}} ->
                    From ! {?MODULE, ok},
                    loop(maps:update(U,{P,false},Map));
                _ ->
                    From ! {?MODULE, invalid},
                    loop(Map)
            end;
        {online, From} -> 
            FilterOn = fun(_,{_,O}) -> O end, 
            Aux = maps:filter(FilterOn, Map),
            From ! {?MODULE, maps:keys(Aux)},
            loop(Map)
            %Aux = fun(K,{_,true},AccIn) -> [K | AccIn] end,
            %Users = map:fold(Aux,[],Map),
            %From ! {?MODULE, Users},
            %loop(Map)
    end.
