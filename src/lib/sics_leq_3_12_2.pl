sics_leq_3_12_2 :-
	upsh_version( UVer ),
	UVer = _-Prolog-_, 
	functor( Prolog, sicstus, 1 ),
	arg( 1, Prolog, PVer ),
	( upsh_pl_version_later_than(PVer,3:12:2,true) ->
		halt( 0 )
		;
		halt( 1 )
	).
