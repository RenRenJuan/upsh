:- ensure_loaded( sys_info_swi ).  		% sys_info/1.

main( Opts ) :-
     ( memberchk(pupsh(Pupsh),Opts) ->
          true
          ;
          Pupsh = pupsh
     ),
	sys_info( 'chmod 755 upsh' ),
     ( atom_concat('/',_,Pupsh) -> 
          MvPfx = 'mv upsh '
          ;
         ( memberchk(pl(Pl),Opts) -> 
               ( (atom_concat(Dir,'swipl',Pl);
                  atom_concat(Dir,'pl',Pl) ) ->
                  % naku: 
                  %  Dir = '$local/bin/'
                     atom_concat( 'mv upsh ', Dir, MvPfx )
                     ;
                     MvPfx = 'mv upsh $HOME/bin/'
               )
               ;
               MvPfx = 'mv upsh $HOME/bin/'
         )
     ),
     write( mvpfx(MvPfx) ), nl,
     atom_concat( MvPfx, Pupsh, FinMv ),
	sys_info( FinMv ).
