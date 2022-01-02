fput_line( Stream, [] ) :-
	put( Stream, 10 ),
	!.
fput_line( Stream, [C|Cs] ) :-
	put( Stream, C ),
	fput_line( Stream, Cs ).
