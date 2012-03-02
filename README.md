Ding
====

An attempt at writing an IRC bot for the polyglot programmer channel I'm in on Freenode.
Currently building in Racket and Erlang, but mostly Erlang.

Feel free to comment on the code.

Goal
----

This is meant to be a learning experience. Building something I enjoy in as much languages as I can.
Hopefully this can be useful for other people too.
Not sure the other languages are going to get a lot of work any time soon since most of my time goes into the Erlang version.


Notes
-----
* You need Erlang R15 for the Erlang version to work.
* The Erlang version will also be a bot-framework. There is an example implementation but you should be able to build your own on top of girc.erl

Erlang Todo
-----------

* Config file
* Use other nick when nick is in use.
* Basicly make sure the bot can stay connected at all times.
