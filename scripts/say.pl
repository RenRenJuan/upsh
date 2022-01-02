
:- ensure_loaded( library(system) ).
:- ensure_loaded( library(lists) ).
% :- ensure_loaded( library(skata) ).

say(InArgs) :- 
      ( is_list(InArgs) -> Args = InArgs; Args = [InArgs] ),
	   debug( say, 'Args: ~w', [Args] ),
	   write_args( Args ).

write_args( [] ).
write_args( [H|T] ) :-
	write( H ), nl,
	write_args( T ).
