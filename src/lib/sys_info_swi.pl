sys_info( System ) :-
	shell( System, Status ),
	( Status = 0 -> StatusTerm = ok(System)
			   ;	 StatusTerm = exit(Status,System)
	),
	write( user_error, StatusTerm ), nl.
