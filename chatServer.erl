-module(chatServer).
-export([server/1]).

server(Port) ->
  DefaultRoom = room:start(),
  loginManager:start(),
  room_manager:start(DefaultRoom),
  {ok, LSock} = gen_tcp:listen(Port, [list, {packet, line}, {reuseaddr, true}]),
  acceptor(LSock, DefaultRoom).

acceptor(LSock, Room) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, Room) end),
  gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Hello\n"),
  authentication(Sock, Room).


authentication(Sock, Room) ->
  receive
      {tcp, _, Data} -> 
        case string:tokens(Data, " \n") of
          ["\\create", U, P] ->
            case loginManager:create_account(U,P) of
              user_exists -> 
                gen_tcp:send(Sock, "\e[91m [SERVER] \e[39m Already Exists\n"),
                authentication(Sock, Room);
              ok -> 
                gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Success\n"),
                Room ! {enter, self()},
                user(Sock, U, Room)
            end;
          ["\\login", U, P] ->
            case loginManager:login(U,P) of
              ok -> 
                gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Success\n"),
                Room ! {enter, self()},
                user(Sock, U,Room);
              invalid ->
                gen_tcp:send(Sock, "\e[91m [SERVER] \e[39m Error\n"),
                authentication(Sock, Room)
            end;
          ["\\close", U, P] ->
            case loginManager:close_account(U, P) of 
              ok -> 
                gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Closed\n"),
                authentication(Sock, Room);
              invalid ->
                gen_tcp:send(Sock, "\e[91m [SERVER] \e[39m Error\n"),
                authentication(Sock, Room)
            end; 
          _ -> 
            gen_tcp:send(Sock, "\e[91m [SERVER] \e[39m Invalid\n"),
            authentication(Sock, Room)
        end
  end.


user(Sock, User, Room) ->
  receive
    {line, Data, _} ->
      gen_tcp:send(Sock, Data),
      user(Sock, User, Room);
    {tcp, _, Data} ->
      case string:tokens(Data, " \n\r") of 
        ["\\logout"] -> 
          loginManager:logout(User),
          Room ! {leave, self()},
          gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Goodbye\n"),
          authentication(Sock, Room);
        ["\\online"] ->
          gen_tcp:send(Sock, "\e[32m [ONLINE] \e[39m " ++ prettyprintList(loginManager:online()) ++ "\n"),
          user(Sock, User, Room);
        ["\\rooms"] ->
          gen_tcp:send(Sock, "\e[32m [ROOMS] \e[39m " ++ prettyprintList(room_manager:list_rooms()) ++ "\n"),
          user(Sock, User, Room);
        ["\\room", "default"] -> 
          Room ! {leave, self()},
          {_, NewValue} = room_manager:change_room(self(), "default"),
          gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Room Altered\n"),
          user(Sock, User, NewValue);
        ["\\room", RoomName] -> 
          Room ! {leave, self()},
          {Res, NewValue} = room_manager:change_room(self(), RoomName),
          case Res of
            room_altered -> 
              gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Room Altered\n");
            room_created -> 
              gen_tcp:send(Sock, "\e[32m [SERVER] \e[39m Room Created\n")
            end,
          user(Sock, User, NewValue);
        _ -> 
          Room ! {line, "\e[34m[" ++ User ++ "] \e[39m" ++ Data, self()},
          user(Sock, User, Room)
      end;
    {tcp_closed, _} ->
      Room ! {leave, self()};
    {tcp_error, _, _} ->
      Room ! {leave, self()}
  end.

prettyprintList([H]) -> 
    H;
prettyprintList([H|T]) ->
    H ++ ", " ++ prettyprintList(T); 
prettyprintList([]) -> 
    "".
