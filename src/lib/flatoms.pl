:- ensure_loaded( library(lists) ).

flatoms( Atoms, Atom ) :-
	flatoms( Atoms, [], Mota ),
	reverse( Mota, AtomCs ),
	atom_codes( Atom, AtomCs ).

flatoms( Atoms, Acc, Codes ) :-
	( Atoms = [] -> 
		Acc = Codes 
		;
		( Atoms = [H|T] -> 
			flatoms( H, Acc, Acc1 ),
			flatoms( T, Acc1, Codes )
			;
			atom_codes( Atoms, AtomCodes ),
			reverse( AtomCodes, Addition ),
			append( Addition, Acc, Codes )
		)
	).
