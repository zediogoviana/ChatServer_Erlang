-module(room_manager).
-export([change_room/2, list_rooms/0, start/1]).

start(Default) -> 
    register(?MODULE, spawn(fun() -> loop(#{"default" => Default}) end)).

change_room(Pid, RoomName) ->
    ?MODULE ! {change_room, Pid, RoomName, self()},
    receive {?MODULE, Res, Value} -> {Res, Value}
    end.

list_rooms() ->
    ?MODULE ! {list_rooms, self()},
    receive {?MODULE, Res} -> Res 
    end.

loop(Map) ->
    receive
        {change_room, Pid, RoomName, From} -> 
            case maps:find(RoomName, Map) of
                error -> 
                    NewRoom = room:start(),
                    NewRoom ! {enter, Pid},
                    From ! {?MODULE, room_created, NewRoom},
                    loop(maps:put(RoomName, NewRoom, Map));
                {ok, Value} -> 
                    Value ! {enter, Pid},
                    From ! {?MODULE, room_altered, Value},
                    loop(Map)
            end;
        {list_rooms, From} -> 
            From ! {?MODULE, maps:keys(Map)},
            loop(Map)
    end.

 
