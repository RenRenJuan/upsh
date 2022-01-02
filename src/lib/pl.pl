:- dynamic pl/1.

pl :-
	( current_op( 900, fy, nospy) -> 
		% sicstus (3.8.6)
		retractall( pl(_OldP) ),
		prolog_flag(version,SicsVer),
		ensure_loaded( library(lists) ),
		atom_codes( SicsVer, SicsVerCs ),
		append( "SICStus ", SicsVerNoCs, SicsVerCs ),
		append( SicsVerMjCs, [0'.|SicsVerMnFxCs], SicsVerNoCs ),
		append( SicsVerMnCs, [0'.|SicsVerFxRsCs], SicsVerMnFxCs ),
		append( SicsVerFxCs, [0' |_SicsVerRsCs], SicsVerFxRsCs ),
		number_codes( Mj, SicsVerMjCs ),
		number_codes( Mn, SicsVerMnCs ),
		number_codes( Fx, SicsVerFxCs ),
		assert( pl(sicstus(Mj:Mn:Fx)) )
		;
		(  current_op( _P, _A, '#=' ) -> 
			% eclipse (4.2)
			getcwd(Cwd), 
			assert( prolog_load_context(directory,Cwd) ),
			assert( wh_prolog(eclipse(ver)) ),
			retract_all( pl(_OldP) ),
			get_flag( version, Ver ),
			assert( pl(eclipse(Ver)) )
			;
			( current_op( _P, _A, module_transparent ) ->
				% swi (4:0:10)
				current_prolog_flag( version, SwiVer ),
				Mj is SwiVer // 10000, 
				Md is SwiVer mod 10000,
				Mn is Md // 100,
				Fx is Md mod 100,
				retractall( pl(_OldP) ),
				assert( pl(swi(Mj:Mn:Fx)) )
				;
				( current_op(_P,_A,same) ->
					% yap 4.3.20
					prolog_flag( version, YapVer ),
					ensure_loaded( library(lists) ),
					atom_codes( YapVer, YapVerCs ),
					append( "Yap-", YapVerNoCs, YapVerCs ),
					append( YapVerMjCs, [0'.|YapVerMnFxCs], YapVerNoCs ),
					append( YapVerMnCs, [0'.|YapVerFxCs], YapVerMnFxCs ),
					number_codes( Mj, YapVerMjCs ),
					number_codes( Mn, YapVerMnCs ),
					number_codes( Fx, YapVerFxCs ),
					retractall( pl(_OldP) ),
					assert( pl(yap(Mj:Mn:Fx)) ),
					!
					;
					true
				)
			)
		)
	).

:- pl.

pl_call( Prolog, Call ) :-
	( pl( Prolog ) -> 
		call( Call )
		;
		true
	).
