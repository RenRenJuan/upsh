:-	ensure_loaded( library(system) ).
	% system/2.

sys_info( System ) :-
	system( System, Status ),
	( Status = 0 -> StatusTerm = ok(System)
			   ;	 StatusTerm = exit(Status,System)
	),
	print_message( help, StatusTerm ).
