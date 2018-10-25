-module(room).
-export([start/0, room/1]).

start() -> 
    spawn(fun() -> room([]) end).

room(Pids) ->
  receive
    {enter, Pid} ->
      io:format("user entered room~n", []),
      room([Pid | Pids]);
    {line, _, From} = Msg ->
      [Pid ! Msg || Pid <- Pids, From /= Pid],
      room(Pids);
    {leave, Pid} ->
      io:format("user left room~n", []),
      room(Pids -- [Pid])
  end.