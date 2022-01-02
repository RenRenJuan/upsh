upsh_fget_line( Stream, Cs ) :-
	get0( Stream, C),
	( C = 10 -> 
		Cs = []
		;
		( C = -1 -> 
			Cs = []
			;
			Cs = [C|Tcs],
			upsh_fget_line( Stream, Tcs )
		)
	).
