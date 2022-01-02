
% This is not correct, as they have different behaviour on directories.
%

file_exists( File ) :-
   exists_file( File ).

environ( Var, Val ) :-
   ground( Val ),
   !,
   setenv( Var, Val ).

environ( Var, Val ) :-
   ground( Var ),
   !,
   getenv( Var, Val ).

environ( Var, Val ) :-
   throw( mode_not_supported_on_swi(environ(Var,Val)) ).

system( Sys ) :- shell( Sys ).
