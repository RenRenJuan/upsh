copy_stream_on_stream( InStream, OutStream ) :-
	peek_char( InStream, Code ),
	( at_end_of_stream( InStream ) -> 
		true
		;
		get0( InStream, Code ),
		copy_stream_on_stream( Code, InStream, OutStream )
	).

copy_stream_on_stream(-1, _In, Out) :- !,flush_output(Out).
copy_stream_on_stream(InCode, In, Out) :- 
	put(Out, InCode), 
	get0(In, OutCode), 
	copy_stream_on_stream(OutCode, In, Out).

