# ChatServer_Erlang
A chat server with authentication and chat rooms, written in Erlang.

## How to use

1. ```erlc *.beam```
2. Enter Erlang interpreter.
3. ```chatServer:server(**PORT**).```
4. You can use ```telnet``` as a client.

## Client commands before authentication

* ```\create **Username Password** ``` : create User account;
* ```\login **Username Password** ``` : login User;
* ```\close **Username Password**``` : close User account;

## Client commands after authentication

* ```\logout ``` : logout from session;
* ```\online ``` : list of all online users;
* ```\room **RoomName**``` : change chat room (if it not exists it creates a new one);
* ```\room default``` : change chat room to the initial one;
* ```\rooms```: list of all chat rooms;

