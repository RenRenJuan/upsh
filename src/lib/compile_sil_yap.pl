compile_sil( SilMode, Files ) :-
	( SilMode == true ->
		open_null_stream( Err ),
		prolog_flag( user_error, OldErr ),
		set_prolog_flag( user_error, Err ),
		compile( Files ),
		set_prolog_flag( user_error, OldErr )
		;
		compile( Files  )
	).
