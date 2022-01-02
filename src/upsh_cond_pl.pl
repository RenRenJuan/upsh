% Tested with sicstus(3:9:0)
% devel:swi(5:9:11)
% stable-git: yap(5:1:4)
% devel:yap(6:0:5)
%

:- dynamic prolog_system_recogniser/2.
:- ensure_loaded( library(lists) ).

prolog_system_recogniser( Flag, sicstus(Major:Minor:Fix) ) :-
   atom_concat('SICStus ',VerPfxAtom,Flag),
   atom_concat(VerAtom,_Rem,VerPfxAtom),
   atom_concat(VerOnly,' ',VerAtom),
   atom_concat(MjrPfx,MnrFixAtom,VerOnly),
   atom_concat(MajorAtom,'.',MjrPfx),
   atom_concat(MnrPfx,FixAtom,MnrFixAtom),
   atom_concat(MinorAtom,'.',MnrPfx),
   atom_codes(MajorAtom,MajorCs),
   number_codes(Major,MajorCs),
   atom_codes(MinorAtom,MinorCs),
   number_codes(Minor,MinorCs),
   atom_codes(FixAtom,FixCs),
   number_codes(Fix,FixCs).

prolog_system_recogniser( Flag, yap(Major:Minor:Fix) ) :-
   % 'Yap-5.1.4'
   atom_concat('Yap-',VerPfxAtom,Flag),
   atom_concat(MjrPfx,MnrFixAtom,VerPfxAtom),
   atom_concat(MajorAtom,'.',MjrPfx),
   atom_concat(MnrPfx,FixAtom,MnrFixAtom),
   atom_concat(MinorAtom,'.',MnrPfx),
   atom_codes(MajorAtom,MajorCs),
   number_codes(Major,MajorCs),
   atom_codes(MinorAtom,MinorCs),
   number_codes(Minor,MinorCs),
   atom_codes(FixAtom,FixCs),
   number_codes(Fix,FixCs).

prolog_system_recogniser( Flag, yap(Major:Minor:Fix) ) :-
   % 'YAP 6.0.5 (x86_64-linux): Tue May  4 11:12:56 BST 2010'
   % atom_codes( Flag, FlagCs ),
   atom_concat( 'YAP ', Right, Flag ),
   atom_codes( Right, RightCs ),
   append( VersCs, [0' |_], RightCs ), 
   append( MjrCs, [0'.|MnrFixCs], VersCs ), % '
   append( MnrCs, [0'.|FixCs], MnrFixCs ), % '
   number_codes( Major, MjrCs ),
   number_codes( Minor, MnrCs ),
   number_codes( Fix, FixCs ).

prolog_system_recogniser( Flag, swi(Major:Minor:Fix) ) :-
   number(Flag),
   Fix   is Flag mod 100,
   Major is Flag // 10000,
   Minor is ((Flag - (Major*10000)) // 100).

:-
	( current_predicate(upsh_pl/1) ->
     write( true ), nl,
	  true
	  ;
     ( ( current_prolog_flag( version, VersionFlag ), 
         prolog_system_recogniser(VersionFlag,Engine) ) ->
         write( engine(Engine) ), nl,
	      assert( upsh_pl(Engine) )
         ;
         M='Unrecognised prolog engine in trying to assert if_pl/1.',
	      write( user_error, M ),
	      nl( user_error )
	  )
	).

upsh_pl_sat( range(Engine,FromInc,ToExc) ) :- 
	!,	
	upsh_pl( Prolog ),
	functor( Prolog, Engine, 1 ),
	arg( 1, Prolog, Running ),
	upsh_pl_version_later_than( Running, FromInc, true ),
	upsh_pl_version_later_than( ToExc, Running, false ).
upsh_pl_sat( geq(System) ) :- 
	!,
	upsh_pl( Prolog ),
	functor( System, Engine, 1 ),
	functor( Prolog, Engine, 1 ),
	arg( 1, System, CondVer ),
	arg( 1, Prolog, Running ),
	upsh_pl_version_later_than( Running, CondVer, true ).
upsh_pl_sat( Engine ) :- 
	upsh_pl( Engine ).

upsh_pl_version_later_than( A:_B, C:_D, _EqBool ) :-
	A > C,
	!.
upsh_pl_version_later_than( A:B, A:C, EqBool ) :-
	!,
	upsh_pl_version_later_than( B, C, EqBool ).
upsh_pl_version_later_than( A, B, EqBool ) :-
	\+ A = _:_,
	( A > B -> true
		;
		( (EqBool == true, A =:= B) -> 
			true
			;
			false
		)
	).

upsh_pl_call( Cond, Call ) :-
	( upsh_pl_sat(Cond)	->
		call( Call )
		;
		true
	).
upsh_pl_call( Cond, Then, Else ) :-
        ( upsh_pl_sat(Cond) -> 
                call( Then )
                ;
                call( Else )
        ).

upsh_pl_call_version( Engine, Version, Before, ThisAndAfter ) :-
	upsh_pl( Prolog ),
	( functor( Prolog, Engine, 1 ) ->
		arg( 1, Prolog, Running ),
		( Running @< Version -> 
			call( Before )
			;
			call( ThisAndAfter )
		)
		;
		true
	).
	
:- abolish( prolog_system_recogniser/2 ).

% cannot support Ciao yet since :- doesnt mean the same thing.
% we should use
% :- initialization(init).
% 
% init:-
    % write(a), 
    % nl.
% 
	  % % ciao should be first because it : is not an xfy operator
	  % % and current_op is not a built-in predicate
	  % ( VersionFlag=ciao(JdotN,Fix) ->
	    % atom_codes( JdotN, JdotNCs ),
	    % ((JdotNCs = [MjC1,0'.|MnCs],MjCs=[MjC1]);
	      % (JdotNCs = [MjC1,MjC2,0'.|MnCs],MjCs=[MjC1,MjC2])
	    % ),
	    % atom_codes( Major, MjCs ),
	    % atom_codes( Minor, MnCs ),
	    % !,
	    % bb_put( ciao, yes )
	    % % assert( (pl(ciao(Major:Minor:Fix))) )
