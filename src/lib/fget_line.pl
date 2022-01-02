fget_line( Stream, [] ) :-
	at_end_of_line( Stream ),
	get0( Stream, _C),
	!.
fget_line( Stream, [C|Cs] ) :-
	get0( Stream, C ),
	fget_line( Stream, Cs ).
