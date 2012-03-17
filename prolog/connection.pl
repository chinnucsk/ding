:- use_module(library(readutil)).
:- use_module(library(socket)).

connect :-
	tcp_socket(Sock),
	tcp_connect(Sock, 'irc.freenode.org':6667, In, Out).

read_from_irc(InPort, ParserThread) :-
	see(InPort), repeat,
	read_and_post(ParserThread), fail.

read_and_post(InPort, ParserThread) :-
	repeat, read_line_to_codes(InPort, Codes),
	thread_send_message(ParserThread, Codes), fail.

