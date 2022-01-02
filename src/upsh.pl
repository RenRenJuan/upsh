

% Unix Prolog SHell (upsh) for SWI-Prolog
% Nicos Angelopoulos, 13th June, 1999.
% last modification, 26 Jan 2018.
% see contact details at: http://stoics.org.uk/~nicos/sware/contact.html
% 
% A wrapper for Prolog scripting. Can be either called 
% from the top of a prolog program (pl sripting), eg
% 
% #! /bin/sh
% exec  './upsh' `pwd` $0 s "$@"
% 
% for silent invocation, this should be
% #! /bin/sh
% exec  './upsh' `pwd` $0 s p "$@"
% 
% or more interestingly since there is no need to change your prolog program,
% from the unix command line, 
% 
%  %./upsh say(.pl) tara was here or 42
%
% Your Prolog program will be loaded and executed. The latter by 
% calling say/1,0 or main/1,0 predicate after loading the (script) file say.pl.
% The argument to say,main/1  will be a list of the trailing arguments 
% supplied. e.g the example above will start executing with goal,
% ? - main( [tara,was,here,or,42] ).
%
% For more information Readme.txt and documentation in doc/.

% To escape a character such as = in filename, so that upsh doesnot
% translate it, use \\ at the command line.

:- module( upsh_exec, [upsh/0] ).

% swi speciifc, but it succeeds in Yap also.
% :- set_prolog_flag( allow_dot_in_atom, true ).  % commented out, 19.3.15

:- multifile portray_message/2.
:- dynamic portray_message/2.
:- discontiguous portray_message/2.

:- multifile help/1.
:- multifile usage_example/1.

% portray_message(informational,restored(_,_,_)).
% portray_message(Any,Trap) :- 
    % write(trap(Any-Trap)),nl,fail.

:- prolog_flag( version, Vers ), 
     ( atom_concat('Yap',_,Vers) -> set_prolog_flag(informational_messages,off) ; true ).

:- ensure_loaded( upsh_cond_pl ).   

% :- ensure_loaded( library(ansi_term) ).   
:- upsh_pl_call( swi(_), use_module(library(ansi_term)), true ).

:- ensure_loaded( library(lists) ). 
        % reverse/2, append/3, select/3, is_list/1, remove_duplicates/2.
        % memberchk/2.
:- upsh_pl_call( swi(_), ensure_loaded('lib/swi_system'), ensure_loaded( library(system) ) ).
        % working_directory/2, environ/2, file_exists/1, datime/1 
        %   ?sicstus(_), yap(_).
        % get_time/1, convert_time/8, ? cd/1, swi(_)

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- 
    SwiCall =  (
                get_time( T ), 
                convert_time( T, Y, M, D, H, N, S, _W ),
                Dtime = datime( Y, M, D, H, N, S ) ),
    upsh_pl_call( swi(_), SwiCall, datime(Dtime) ),
    write( dtime(Dtime) ), nl,
    % upsh_pl_call( swi(_), Vpl='src/upsh_version.pl', Vpl='upsh_version.pl' ),
    % upsh_pl_call( swi(_), Vpl='src/upsh_version.pl', Vpl='upsh_version.pl' ),

    absolute_file_name( pack('upsh/src/upsh_version.pl'), Vpl, [access(read)] ),
    open( Vpl, read, In ), 
    read( In, upsh_version(Ver) ),
    close( In ),
    write( version(Ver) ), nl,
    upsh_pl( Pl ),
    Dtime = datime(Y,M,D,H,N,S),
    Built = built_on(Y/M/D,H:N:S),
    % UpshVer = upsh(Ver)-Pl-Time),
    % upsh_pl_call( swi(_), asserta(upsh_flag(upsh_version,UpshVer)), bb_put(upsh_version,UpshVer) ).
    asserta( upsh_exec:upsh_exec_version(upsh_exec(upsh(Ver),Pl,Built)) ).

user:file_search_path( upsh, '' ). % for absolute filenames
user:file_search_path( upsh, './' ).
:- expand_file_name( '~/bin/cline_upsh/', [Cupsh] ),
   assertz( user:file_search_path( upsh,Cupsh) ).
user:file_search_path( upsh, '/local/share/cline_upsh/' ). % system wide repository.

:- ( member(Alias,[swi,user_app_data]),
     user:file_search_path(Alias,Search),
     directory_file_path(Search,pack,PackD),
     exists_directory(PackD),
     directory_files(PackD,Files),
     member(Dir,Files),
     Dir \== '.', Dir \=='..',
     directory_file_path(PackD,Dir,AbsDir),
     directory_file_path(AbsDir,scripts,ScriptsD),
     exists_directory(ScriptsD),
     debug( upsh,'assert-zing:~w',[user:file_search_path(upsh,ScriptsD)] ),
     assertz(user:file_search_path(upsh,ScriptsD)),
    fail
   ;
   true
   ).

   

% :- upsh_pl_call( swi(_), ensure_loaded('lib/absolute_file_name_swi') ). % obsolete
:- upsh_pl_call( yap(_), ensure_loaded('lib/yap_skip_line') ).
:- upsh_pl_call( yap(_), ensure_loaded('lib/upsh_yap_absolute_file_name') ).
% :- upsh_pl_call( yap(_), ensure_loaded('lib/compile_sil_yap') ).
% :- upsh_pl_call( swi(_), ensure_loaded( library() ).
:- upsh_pl_call( sicstus(_), ensure_loaded( library(charsio) ) ).
    % read_term_from_chars/2.
:- upsh_pl_call( yap(_), ensure_loaded( library(charsio) ) ).       % read_term_from_chars/2.

% let it pick it up from .*rc ?
% :- multifile library_directory/1.
% library_directory( '/home/nicos/pl/lib' ).

:- multifile( upsh_built_call/1 ). 
:- dynamic( upsh_built_call/1 ).

upsh_built_call( false ).

upsh :-
    upsh_pl_call( swi(_), set_stream(user_error, alias(user_output)),
            (current_output(Output),set_prolog_flag(user_error,Output)) ),
    upsh_pl_call( geq(sicstus(3:12:2)), set_prolog_flag(informational,on) ),
    upsh_pl_call( sicstus(_),current_prolog_flag(argv,FrgArgs) ),
    upsh_pl_call( yap(_), unix(argv(FrgArgs)) ),
    upsh_pl_call( swi(_), 
        ( current_prolog_flag( argv, FrgArgs ), (memberchk(b,FrgArgs)-> debug(upsh); true )
                    % from swipl 6.5.2 the above is now the case
          % current_prolog_flag(argv,SwiFrgArgs),
          % append( _PreSwiArgs, [--|FrgArgs], SwiFrgArgs ) )
        ) ),
    debug( upsh,'FrgArgs:~w',[FrgArgs] ),
    upsh_combine_spaced_args( FrgArgs, AllArgs ),
    debug( upsh, 'AllArgs: ~w', [AllArgs] ),
    ( append( UpshArgs, [-|PrgArgs], AllArgs ) -> 
        true
        ;
        PrgArgs = [],
        UpshArgs = AllArgs
    ),
    % upsh_unix_args_to_terms( UpshArgs, ArgPVrPs, UniArgs ),
    upsh_unix_args_to_terms( UpshArgs, [], UpArgVs, UniArgs ),
    upsh_check_error_stream( UniArgs, UniDsArgs ),
    upsh_check_args( UniDsArgs, PrvMode, Pwd, Load, Sil, Wait, FuN, Spy, Wraps, RemUArgs ),
    upsh_verbose( all_os_arguments(FrgArgs) ),
    upsh_verbose( defragmented_os_arguments(AllArgs) ),
    upsh_verbose( all_upsh_arguments(UniArgs) ),
    upsh_check_trans( RemUArgs, UpArgVs, PrgArgs, TransArgs  ),
    upsh_verbose( main_pl_arguments(TransArgs) ),
    ( memberchk(Load,[locate,load,edit]) -> 
        Mode=Load 
        ;
        Mode = PrvMode
    ),
    upsh_main( Mode, Pwd, TransArgs, Load, Sil, Wait, FuN, Spy, Wraps ),
    !,
    upsh_halt( 0 ).

upsh :-
    write( user_error, 'Upsh internal failure while processing arguments.' ),nl(user_error),
    write( user_error, 'Either: (a) Upsh specific arguments contain unrecognised elements, or ' ),
    nl( user_error ),
    write( user_error, '        (b) arguments contained illegal syntax.' ), nl( user_error ),
    upsh_halt(4).

upsh_main( edit, Pwd, [File|_], _Load, Sil, _Wait, _FuN, _Spy, _Wraps ) :-
    upsh_load_file( File, Pwd, locate, Sil, _Old, BaseFile ),
    % upsh_pl_call( swi(_), Call=exists_file(BaseFile),
     %                          Call = file_exists(BaseFile) ),
    % ( call(Call) ->
   ( file_exists(BaseFile) ->
        Base = BaseFile
        ;
        atom_concat( BaseFile, '.pl', Base )
    ),
    ( environ('USER',nicos) -> 
        atom_concat( 'gvim ', Base, Gvim ),
        system( Gvim )
        ;
        ( environ('EDITOR',Editor) -> 
            atom_concat( Editor, ' ', EdSpc ),
            atom_concat( EdSpc, Base, Edit ),
            atom_concat( Edit, ' &', EditBck ),
            system( EditBck )
            ;
               Mess = 'Cannot locate editor to use (EDITOR variable).',
               upsh_error( Mess )
        )
    ).

upsh_main( locate, Pwd, [File|_], Load, Sil, _Wait, _FuN, _Spy, _Wraps ) :-
    !,
    upsh_load_file( File, Pwd, Load, Sil, _Old, Base ),
    write( Base ), nl.
upsh_main( load, Pwd, [File|_], Load, Sil, _Wait, _FuN, _Spy, _Wraps ) :-
    !,
    upsh_load_file( File, Pwd, Load, Sil, _Old, _Base ).

upsh_main( script, Pwd, Args, Load, Sil, Wait, FuN, Spy, Wraps ) :-
    !,
    upsh_verbose( executing_as_script ),
    Args = [File|RestArgs],
    atom_codes( File, FileCs ),
    reverse( FileCs, RevFileCs ),
    ( append( RevBaseCs, [0'/|RevDirCs], RevFileCs ) ->
        reverse( RevDirCs, DirCs ),
        atom_codes( Dir, DirCs ),
        upsh_pl_call( sicstus(_), working_directory( Old, Dir ) ),
        upsh_pl_call( yap(_), working_directory( Old, Dir ) ),
        upsh_pl_call( swi(_), chdir( Pwd ) ),  %and hope for the best.
        reverse( RevBaseCs, BaseCs ),
        atom_codes( LoadFile, BaseCs )
        ;
        LoadFile = File
    ),
    open( LoadFile, read, InStream ),
    upsh_verbose( loading(LoadFile) ),
    upsh_pl_call( swi(_), true, prolog_flag( user_input, OldInput, InStream ) ),
    skip_line( user ),
    skip_line( user ),
    upsh_pl_call( sicstus(_), load_files([user],[compilation_mode(Load)]) ),
    upsh_pl_call( swi(_), load_files(user,[compilation_mode(Load),silent(Sil)]) ),
    upsh_pl_call( yap(_), compile(user_input) ),
    upsh_pl_call( swi(_), true, (prolog_flag(user_input,_InSBe,OldInput),working_directory(Old,Pwd)) ), 
    upsh_spy_this( Spy ),
    upsh_call_main( RestArgs, LoadFile, Old, Wait, FuN, Wraps ).

upsh_main( command_line, Pwd, Args, Load, Sil, Wait, FuN, Spy, Wraps ) :-
    SW = (Spy,Wait),
    upsh_verbose( executing_from_command_line ),
    ( Args = [File|RestArgs] ->
        ( File=t(First) ->
            upsh_load_file( First, Pwd, Load, Sil,  Old, Base ),
            Lopts = (Pwd,Load,Sil),
            % change back to Old ?
            upsh_multi_scripts_main( RestArgs, Base, [], true, Old, Lopts, SW )
            ;
            ( File=i(First,FstVar) -> 
                upsh_load_file( First, Pwd, Load, Sil, Old, Base ),
                Lopts = (Pwd,Load,Sil),
                ThisGoal =.. [Base,FstVar],
                upsh_provider_skip( RestArgs, First, ThisGoal, Old, Lopts, SW )
                % upsh_multi_scripts_main( RestArgs, First, [], true, Old, Lopts, Wait )
                ;
                upsh_load_file( File, Pwd, Load, Sil, Old, Base ),
                upsh_spy_this( Spy ),
                upsh_call_main_example( RestArgs, Base, Old, Wait, FuN, Wraps )
            )
        )
        ;
        nl( user_error ),
        Mess = 'No file given at command line invocation.',
        upsh_error( Mess ),
        upsh_halt( 2 )
    ).
upsh_main( _, _Pwd, _Args, _Load, _Sil, _Wait, _FuN, _Spy ) :-
    upsh_halt( 2 ).

upsh_call_main_example( RestArgs, _Base, _Old, _Wait, _FuN, _Wraps ) :-
    upsh_select_nv( RestArgs, x(I), _Out ),
     !,
     ( current_predicate(usage_example/1) ->
          findall( Ex, usage_example(Ex), Exs ),
          ( upsh_nth(1, I, Exs, Example) ->
               write( 'running ': Example ), nl,
               system( Example )
               ;
               Mess = 'Cannot identify usage_example.',
               upsh_error( Mess, 7 )
          )
          ;
          Mess = 'Cannot find usage_example/1 in memory.',
          upsh_error( Mess, 7 )
     ).
     
upsh_call_main_example( RestArgs, Base, Old, Wait, FuN, Wraps ) :-
     upsh_call_main( RestArgs, Base, Old, Wait, FuN, Wraps ).

upsh_multi_scripts_main( [], FuN, RevFuNArgs, Left, Old, _Lopts, SW ) :-
    reverse( RevFuNArgs, FuNArgs ),
    LastGoal =.. [FuN,FuNArgs],
    SW = (Spy,Wait),
    upsh_spy_this( Spy ),
    upsh_call( (Left,LastGoal), Old, Wait ).
upsh_multi_scripts_main( [H|T], FuN, RevArgs, Left, Old, Lopts, SW ) :-
    var(H),
    !,
    upsh_multi_scripts_main( T, FuN, [H|RevArgs], Left, Old, Lopts, SW ).

upsh_multi_scripts_main( [t(NwFuNTrm)|T], FuN, RevFuNArgs, Left, Old, Lopts, SW ) :-
    !,
    reverse( RevFuNArgs, FuNArgs ),
    ThisGoal =.. [FuN,FuNArgs],
    Lopts = (Pwd,Load,Sil),
    upsh_load_file( NwFuNTrm, Pwd, Load, Sil, _NwOld, NwFuN ), 
    % unclear about this. HERE
    upsh_multi_scripts_main( T, NwFuN, [], (Left,ThisGoal), Old, Lopts, SW ).
upsh_multi_scripts_main( [i(NwFuNTrm,Out)|T], FuN, RevFuNArgs, Left, Old, Lopts, SW ) :-
    !,
    reverse( RevFuNArgs, FuNArgs ),
    ThisGoal =.. [FuN,FuNArgs],
    Lopts = (Pwd,Load,Sil),
    upsh_load_file( NwFuNTrm, Pwd, Load, Sil, _NwOld, NwFuN ), 
    % unclear about this. HERE
    ThisGoal =.. [FuN,Out],
    !,
    upsh_provider_skip( T, NwFuN, Left, Old, Lopts, SW ).
upsh_multi_scripts_main( [H|T], FuN, RevArgs, Left, Old, Lopts, SW ) :-
    upsh_multi_scripts_main( T, FuN, [H|RevArgs], Left, Old, Lopts, SW ).

upsh_provider_skip( [], _FuN, Left, Old, _Lopts, SW ) :-
    % HERE
    % ( In == [] ; (\+ var(In), (\+ In = t(_)) ; \+ = i(_,_) ) ),
    % upsh_multi_scripts_main( [], FuN, [], Left, Old, Lopts, Wait ).
    upsh_call( Left, Old, SW ).
upsh_provider_skip( [H|T], FuN, Left, Old, Lopts, SW ) :-
    var(H),
    !,
    % this should probably be picked up by the upsh args=opts translator.
    upsh_error( 'Skipping dubious variable in args of provider': FuN ), 
    upsh_provider_skip( T, FuN, Left, Old, Lopts, SW ).
upsh_provider_skip( [t(NwFuNTrm)|T], _FuN, Left, Old, Lopts, SW ) :-
    !,
    Lopts = (Pwd,Load,Sil),
    upsh_load_file( NwFuNTrm, Pwd, Load, Sil, _NwOld, NwFuN ), 
    % unclear about this. HERE
    upsh_multi_scripts_main( T, NwFuN, [], Left, Old, Lopts, SW ).
upsh_provider_skip( [i(NwFuNTrm,OutVar)|T], _FuN, Left, Old, Lopts, SW ) :-
    !,
    Lopts = (Pwd,Load,Sil),
    upsh_load_file( NwFuNTrm, Pwd, Load, Sil, _NwOld, NwFuN ), 
    This =.. [NwFuN,OutVar],
    upsh_provider_skip( T, NwFuN, (Left,This), Old, Lopts, SW ).
upsh_provider_skip( [H|T], FuN, Left, Old, Lopts, SW ) :-
     Mess = ('Skipping argument, ', H, ', for provider: ', FuN),
     upsh_error( Mess ),
    upsh_provider_skip( T, FuN, Left, Old, Lopts, SW ).

upsh_call_main( Args, Base, Old, Wait, FuN, WrapS ) :-
    ( var(FuN) ->
          length( Args, Arity ),
        ( current_predicate( user:Base/Arity ) ->
               % upsh_single_arity_arg( Arity, WrapS, Args, TheArgs ),
            Call =.. [Base|Args]
            ;
        ( current_predicate(user:Base/1) ->
               Call =.. [Base,Args]
               ;
        ( current_predicate( user:Base/0 ) ->
            Call =.. [Base]
            ;
        ( current_predicate( user:main/1 ) -> 
            Call =.. [main,Args]
            ;
        ( current_predicate( user:main/0 ) -> 
            Call =.. [main]
            ;
            write( user_error, 'Upsh cannot locate in memory suitable entry-goal. ' ), nl( user_error ),
            write( user_error, 'In your Prolog script define one of: ' ),
            write( user_error, [Base/Arity,Base/1,Base/0,main/1,main/0] ),
               nl( user_error ),
               write( user_error, 'or use n/2 to define a suitable starting predicate.' ),
               nl( user_error ), nl( user_error ),
            upsh_halt( 5 )
        )
        )
        )
        )
          )
        ;
          ( FuN = Name/Arity ->
               ( current_predicate(user:FuN) ->
                    % we can do a little more guessing here 
                    % if Arity \== length(Args)
                    upsh_single_arity_arg( Arity, WrapS, Args, TheArgs ),
                    Call =.. [Name|TheArgs]
                    ;
                write( user_error, 'Upsh cannot locate in memory suitable entry-goal given by n/2 argument.' ), nl( user_error ),
                     write( user_error, 'In your Prolog script define one of: ' ),
                     write( user_error, [FuN] ), nl( user_error ), nl( user_error ),
                     write( user_error, [Name/Arity] ), nl( user_error ), nl( user_error ),
                     upsh_halt( 5 )
               )
               ;
             ( current_predicate(user:FuN/1) -> 
                    upsh_single_arity_arg( 1, WrapS, Args, TheArgs ),
                 Call =.. [FuN,TheArgs]
                 ;
                 ( user:current_predicate(FuN/0) -> 
                     Call =.. [FuN]
                     ;
                     write( user_error, 'Upsh cannot locate in memory suitable entry-goal given by n/1 argument.' ), nl( user_error ),
                     write( user_error, 'In your Prolog script define one of: ' ),
                     write( user_error, [FuN/1,0] ), nl( user_error ), nl( user_error ),
                     upsh_halt( 5 )
                    )
            )
        )
    ),
    upsh_call( Call, Old, Wait ).

upsh_call( Call, Old, Wait ) :-
    upsh_verbose( upsh_calling(Call) ),
    ( catch(user:Call,All,upsh_catcher(All)) ->
        Exit is 0
        ;
          upsh_error( 'Entry-goal failed.' ),
        Exit is 1
    ),
    working_directory( _Pwd, Old ),
    ( (Wait == bell; Wait == both) ->
        put( 7 )
        ;
        true
    ),
    ( (Wait == input; Wait == both) ->
        % get_byte( _Byte )
        get0( _Byte )
        ;
        true
    ),
    upsh_halt( Exit ).

upsh_catcher( All ) :-
    print_message( error, All ),
    upsh_halt( 2 ).

upsh_check_args( InArgs, _Mode, _Pwd, _Load, _Sil, _Wait, _Fun, _Spy, _Wraps, _OutArgs ) :-
     % here ( current_predicate(help/1) -> 
          
    upsh_select_nv( InArgs, h, _Out ),
    nl,
    write( 'usage: upsh [h,v]~ [(c),s] [a,b,e/1,f/1,d/1,l/1,o,p,r,w]* - Program [Args]*' ), nl,
    write( 'or' ), nl,
    write( '       upsh [c] [(m),n] UArgs [[t,i]= Pn[,Vn] [PnArgs]]*' ), nl,
    nl,
    write( '        h    : help; overrides' ), nl,
    write( '        v    : version; overrides' ), nl, nl,
    write( '        c    : command line invocation- default' ), nl,
    write( '        s    : script invocation' ), nl, nl,
    write( '        i/2  : provider script Program' ), nl,
    write( '        t/1  : script Program' ), nl, nl,
    write( '        a    : last argument will be the list of all read vars as upsh_vs/1' ), nl,
    write( '        b    : be verbose' ), nl,
    write( '        e/1  : errors file (default: std_out stream)' ), nl,
     write( '        f    : donot load ~/.<System>rc'), nl,
    write( '        n/1,2: name,[arity] of goal (def.: <file>/n,1,0 or main/1,0)' ), nl,
    write( '        d/1  : working directory (def: \'.\')' ), nl,
    write( '        l/1  : load method: {(f)ast,*r*ead} or {co*m*pile,co(n)sult,loa(d),(l)ocate,(e)dit}' ), nl,
    write( '        p    : surpress loading info (when loading Program)' ), nl,
    write( '        o    : do not translate Args; e.g. very=nice=tara not to very(nice(tara))' ), nl,
    write( '        r/1* : (s)trict about \'-\', *w*rap single as list, or (n)ot wrap' ), nl,
    write( '        w/1  : at end: ring \'bell\', wait for \'input\', \'both\' or *none*' ), nl,
    write( '        x/1  : run nth usage_example/1' ), nl,
    write( '        y/2  : spy this predicate (Name,Arity)' ), nl,
    nl,
    write( 'For scripts add the following lines to the source and do chmod u+x <Program>.pl' ), nl,
    write( 'To escape = in atoms use \\\\=, for term functors use ==.' ), nl, nl,
    write( 'To pass a list use lst functor: pupsh say tara=lst=1,2,3     ->    tara([1,2,3])' ), nl, nl,
    write( '#! /bin/sh' ), nl,
    write( 'exec upsh s - $0 "$@"' ), nl, nl,
    upsh_halt( 0 ).

upsh_check_args( InArgs, _Mode, _Pwd, _Load, _Sil, _Wait, _FuN, _Spy, _Wraps, _OutArgs ) :-
    upsh_select_nv( InArgs, v, _Out ),
    upsh_exec_version( UpshVer ),
    % upsh_pl_call( swi(_), upsh_flag(upsh_version,UpshVer), bb_get(upsh_version,UpshVer) ),
    % write( user_error, 'Upsh ' ),
    write( user_error, UpshVer ),
    nl(user_error),
    upsh_halt( 0 ).

upsh_check_args( InArgs, Mode, Pwd, Load, Sil, Wait, Pred, Spy, WrapS, OutArgs ) :-
     Spy = SpyName/SpyAri,
    ( upsh_select_nv( InArgs, upsh_exec(Exec), EInArgs ) ->
        bb_put( upsh_exec, Exec )
        ;
        EInArgs = InArgs,
        upsh_pl_call( swi(_),
            ( 
                current_prolog_flag(argv, Argv),
                ( nth1(N,Argv,'-x')->
                    NxN is N + 1, nth1(NxN,Argv,StateF),
                    upsh_afn( StateF, StateAfn, [], _B ),
                    flag( upsh_exec, _, StateAfn )
                    ; true 
                )
            )
        )
    ),
    ( upsh_select_nv( EInArgs, y(SpyName,SpyAri), YInArgs ) ->
        true
        ;
        YInArgs = EInArgs,
        SpyName = null__
    ),
    ( upsh_select_nv( YInArgs, b, BInArgs ) ->
        VerbFlag = on
        ;
        VerbFlag = off,
        BInArgs = YInArgs 
    ),
    upsh_pl_call( swi(_),
            flag( upsh_verbose,_, VerbFlag ),
            bb_put( upsh_verbose, VerbFlag )
    ),
    upsh_check_mode( BInArgs, Mode, MidArgs ),
    upsh_check_load_mode( MidArgs, Load, SecArgs ),
    upsh_check_load_surpress( SecArgs, Sil, TrdArgs ),
    ( upsh_select_nv(TrdArgs,f,FrtArgs) ->
          true
          ;
          FrtArgs = TrdArgs,
          upsh_pl_call( sicstus(_), RC = '~/.sicstusrc' ),
          upsh_pl_call( swi(_), expand_file_name( '$HOME/.swiplrc',[RC|_]) ),
          upsh_pl_call( yap(_), expand_file_name( '$HOME/.yaprc',[RC|_]) ),
          % upsh_pl_call( swi(_), RC = '/home/nicos/.plrc' ),
          % upsh_pl_call( yap(_), RC = '~/.yaprc' ),
          ( file_exists(RC) -> user:ensure_loaded( RC ); true )  % added, 19.3.15
     ),
    ( upsh_select_nv(FrtArgs,d(Dir),Frt1Args) ->
        true
        ;
        Dir = '.',
        Frt1Args = FrtArgs 
    ),
    ( upsh_select_nv(Frt1Args,w(Wait),FthArgs) ->
        true % Wait = yes
        ;
        Wait = none,
        FthArgs = FrtArgs
    ),
    ( upsh_select_nv(FthArgs,n(Name),SxthArgs) ->
          Pred= Name/1
        ;
         ( upsh_select_nv(FthArgs,n(Name,Arity),SxthArgs) ->
               Pred= Name/Arity
               ;
               % let Pred be a variable
             SxthArgs = FthArgs
          )
    ),
    ( upsh_select_nv(SxthArgs,r(w),OutArgs) ->
          WrapS=true
        ;
         ( upsh_select_nv(SxthArgs,r(n),OutArgs) ->
               WrapS=false
               ;
               WrapS=true,
              OutArgs = SxthArgs
          )
    ),
    upsh_afn( Dir, Pwd, [file_type(directory),access(exist)], _ ).
    % absolute_file_name( Dir, Pwd ).

upsh_check_trans( UpshArgs, UpArgVs, PrgArgs, VarTransArgs  ) :-
    ( upsh_select_nv(UpshArgs,a,NOaUpshArgs) ->
        A = true
        ;
        A = false,
        NOaUpshArgs = UpshArgs
    ),
    ( upsh_select_nv( NOaUpshArgs, o, RemUpsh ) ->
        TransPrgArgs = PrgArgs
        ;
        RemUpsh = NOaUpshArgs,
        % upsh_unix_args_to_terms( PrgArgs, ArgsVrPs, TransPrgArgs ),
        upsh_unix_args_to_terms( PrgArgs, [], PrgArgVs, TransPrgArgs )
        % upsh_common_variables( ArgsVrPs )
    ),
    ( upsh_select_nv( RemUpsh, r(s), _ ) ->
        TransArgs = TransPrgArgs
        ;
        append( RemUpsh, TransPrgArgs, TransArgs )
    ),
    append( UpArgVs, PrgArgVs, AllVs ),
    upsh_common_variables( AllVs ),
    ( A == true ->
        % choose_variables_from_pairs( TransArgs, AllPairs, [_|UpshVs] ),
        upsh_pl_call( swi(_), 
                upsh_swi_remove_duplicates( AllVs, UnqVs ),
                remove_duplicates( AllVs, UnqVs )
            ),
        append( TransArgs, [upsh_vs(UnqVs)], VarTransArgs )
        ;
        VarTransArgs = TransArgs
        % upsh_common_variables( PrgArgVs )
    ).

% choose_variables_from_pairs( [], _Pairs, [] ).
% choose_variables_from_pairs( [H|T], Pairs, [HP|TPs] ) :-
    % upsh_id_pair_select( Pairs, H, HP, RemPairs ),
    % choose_variables_from_pairs( T, RemPairs, TPs ).

% upsh_id_pair_select( [], Key, HP, RemPairs ) :- fail.
upsh_id_pair_select( [Hk-Hp|TPs], Key, Value, RemPairs ) :-
    ( Hk == Key ->
        Value = Hp,
        RemPairs = TPs
        ;
        RemPairs = [Hk-Hp|RestPairs],
        upsh_id_pair_select( TPs, Key, Value, RestPairs )
    ).

upsh_check_mode( InArgs, Mode, OutArgs ) :-
    ( upsh_select_nv(InArgs,s,OutArgs) -> 
        ( upsh_select_nv(InArgs,c,_IrrArgs) -> 
               upsh_error( 'upsh called with incompatible options', 3 )
            ;
            Mode = script
        )
        ;
        Mode = command_line,    % default 
        (upsh_select_nv(InArgs,c,OutArgs) ->
            true
            ;
            OutArgs = InArgs
        )
    ).

upsh_check_error_stream( InArgs, OutArgs ) :-
    ( upsh_select_nv(InArgs,e(File),OutArgs) -> 
        ( File  == dev_null ->
            open_null_stream( Error )
            ;
            % file_exists, might be /dev/null is also ok 
            open( File, append, Error )
        ),
        upsh_pl_call( sicstus(_), set_prolog_flag( user_error, Error ) ),
        upsh_pl_call( swi(_), set_stream(Error,alias(user_error)) ),
        upsh_pl_call( yap(_), set_prolog_flag( user_error, Error ) )
        ;
        upsh_pl_call( sicstus(_), current_prolog_flag( user_output,Error) ),
        upsh_pl_call( swi(_), current_output(Error) ),
        upsh_pl_call( yap(_), current_prolog_flag(user_output,Error) ),
        OutArgs = InArgs
    ).

upsh_check_load_mode( InArgs, LoadMode, OutArgs ) :-
    ( upsh_select_nv(InArgs,l(Load),OutArgs) ->
            ( (Load==n;Load==consult) ->
                LoadMode = consult
                ;
                ( (Load==m;Load==compile) ->
                    LoadMode = compile
                    ; 
                    ( (Load==l;Load==locate) -> 
                        LoadMode = locate
                        ;
                        ( (Load==d;Load==load) ->
                            LoadMode = load
                            ;
                            ( (Load==e;Load==edit) -> 
                                LoadMode = edit
                                ;
                                LArg = 'upsh called with erroneous l/1 argument',
                                        upsh_error( LArg, 6 )
                            )
                        )
                    )

                )
            )
            ;
            LoadMode = compile,
            OutArgs = InArgs
    ).

upsh_check_load_surpress( InArgs, Sil, OutArgs ) :-
    ( upsh_select_nv(InArgs,p,OutArgs) ->
        Sil = true,
        upsh_pl_call( range(sicstus,(3:0:0),(3:9:0)), 
            upsh_sicstus_nc('compat/sicstus/sics38_pncms') ),
        upsh_pl_call( range(sicstus,(3:9:0),(3:12:2)), 
            upsh_sicstus_nc('compat/sicstus/sics39_pncms') ),
        upsh_pl_call( geq(sicstus(3:12:2)), 
            set_prolog_flag(informational,off) ),
        upsh_pl_call( yap(_), set_prolog_flag(informational_messages,off) )
        ;
        Sil = false,
        OutArgs = InArgs
    ).

upsh_sicstus_nc( 'compat/sicstus/sics39_pncms' ) :-
    !,
    assert( portray_message( informational, imported(_,_,_) ) ),
    assert( portray_message( informational, loading(_,_,_) ) ),
    assert( portray_message( informational,loaded(_,_,_,_,_,_) ) ),
    assert( portray_message( informational,foreign_resource(_,_,_,_) ) ).
upsh_sicstus_nc( LibFile ) :-
    open_null_stream( Null ),
    current_prolog_flag( user_error, StdErr ),
    % set_prolog_flag( user_error, Null ),
    ( ensure_loaded( library(LibFile) ) ; true ),
    set_prolog_flag( user_error, StdErr ),
    close( Null ).

upsh_unix_args_to_terms( [], Vs, Vs, [] ).
upsh_unix_args_to_terms( [H|T], Acc, AllVs, [Th|Tt] ) :-
    atom_codes( H, HCs ),
    upsh_unix_eqcs_arg_to_term( HCs, any, ThisVs, Th ),
    append( ThisVs, Acc, NxAcc ),
    upsh_unix_args_to_terms( T, NxAcc, AllVs, Tt ).

upsh_unix_eqcs_arg_to_term( ACs, Atm, Vs, Term ) :-
    ( append(LeftCs,[0'=,0'=|Right],ACs) ->
        atom_codes( Left, LeftCs ),
        Term = (Left = Sub )
        ;
        Right = ACs, Sub = Term
    ),
    upsh_unix_eqcs_arg_to_term( Right, Atm, Vs, [], Sub ). 

upsh_nest_term_flatten( List, Term ) :-
    upsh_nest_term_flatten_1( List, Term ).
upsh_nest_term_flatten_1( [F,S|T], Term ) :-
    !,
    ( is_list(S) -> 
        append( S, [F], NxH )
        ;
        S =.. [Name|Args],
        append( Args, [F], NxArgs ),
        NxH =.. [Name|NxArgs]
    ),
    upsh_nest_term_flatten_1( [NxH|T], Term ).
upsh_nest_term_flatten_1( [Term], Term ).

upsh_unix_eqcs_arg_to_term( ACs, Atm, Vs, Stack, Term ) :-
    ( upsh_break_list_on_list( ACs, [0':,0',,0'=], Brk, LeftCs, RightCs ) -> 
        ( Brk==0'= ->
            upsh_in_codes_to_term( LeftCs, Atm, Vs1, Functor ),
            ( Functor == atm ->
                NxAtm = atm, NxStack = Stack
                ;
                ( Functor == lst ->
                    NxAtm = any, NxStack = [[]|Stack]
                    ;
                    ( Functor = num ->
                              NxAtm = num, NxStack = Stack
                              ;
                         ( Functor = trm ->
                             NxAtm = trm, NxStack = Stack
                             ;
                             NxAtm = any, NxStack = [Functor|Stack]
                         )
                         )
                )
            )
            ;
            ( Brk==0', ->
                upsh_in_codes_to_term( LeftCs, Atm, Vs1, Arg ),
                ( Atm == lst -> 
                    NxStack = [Arg|Stack],
                    NxAtm = lst
                    ;
                    NxAtm = any,
                    Stack = [MostRec|TStack],
                    ( is_list(MostRec) -> 
                        append( MostRec, [Arg], NxRec )
                        ;
                        MostRec =.. [Functor|ExArgs],
                        append( ExArgs, [Arg], NxArgs ),
                        NxRec =.. [Functor|NxArgs]
                    ),
                    NxStack = [NxRec|TStack]
                )
                ;
                % Brk == 0':
                NxAtm = any,
                upsh_in_codes_to_term( LeftCs, Atm, Vs1, LArg ),
                Stack = [MostRec|TStack],
                ( is_list(MostRec) -> 
                    ( LArg == '' -> 
                        _NxRec = NxSecRec % fixme: branch singleton
                        ;
                        append( MostRec, [LArg], SecArg )
                    )
                    ;
                    MostRec =.. [Functor|ExArgs],
                    ( LArg == '' -> 
                        NxArgs = ExArgs
                        ;
                        append( ExArgs, [LArg], NxArgs )

                    ),
                    SecArg =.. [Functor|NxArgs]
                ),
                TStack = [SecRec|RStack],
                ( is_list(SecRec) -> 
                    append( SecRec, [SecArg], NxSecRec )
                    ;
                    SecRec =.. [SecF|SecExAs],
                    append( SecExAs, [SecArg], NxSecArgs ),
                    NxSecRec =.. [SecF|NxSecArgs]
                ),
                NxStack = [NxSecRec|RStack]
            )
        ),
        upsh_unix_eqcs_arg_to_term( RightCs, NxAtm, VsRec, NxStack, Term ),
        append( Vs1, VsRec, Vs )
        ;
        upsh_cleaned_escaped( ACs, ClACs ),
        upsh_in_codes_to_term( ClACs, Atm, Vs, Last ),
        upsh_nest_term_flatten( [Last|Stack], Term )
        % upsh_type_and_nest_term_flatten( Atm, [Last|Stack], Term )
    ).

upsh_load_file( File, Pwd, Load, Sil, Old, Base ) :-
    ( upsh_looks_up(File,AbsFile) ->
        PrvBase = File
        ;
        Options = [file_type(source),access(read),file_errors(fail)],
        upsh_afn( upsh(File), PrvAbsFile, Options, PrvBase ),
        % upsh_pl_call( swi(_), Call=exists_file(PrvAbsFile),
                            % Call = file_exists(PrvAbsFile) ),
        % ( call(Call) ->
      ( file_exists(PrvAbsFile) ->
            AbsFile = PrvAbsFile
            ;
            upsh_afn( File, AbsFile, Options, PrvBase )
        )
    ),
    ( Load == locate -> 
        Base = AbsFile
        ;
        ( Load == load -> Cmode = consult; Cmode = Load ),
        Base = PrvBase,
        upsh_verbose( loading(AbsFile) ),
        upsh_pl_call( swi(_), 
            (   catch(
                user:load_files([AbsFile], [compilation_mode(Cmode),silent(Sil)] ),
                All, upsh_catcher(All) ),
                working_directory( Old, Pwd )
               ) ),
        upsh_pl_call( sicstus(_),
            (   catch(
                load_files([AbsFile], [compilation_mode(Cmode)] ),
                    All, upsh_catcher(All) ),
                working_directory( Old, Pwd )
            ) ),
        upsh_pl_call( yap(_),
        (   catch( compile([AbsFile]), All, upsh_catcher(All)),
                working_directory( Old, Pwd )
            ) )
    ),
    !.
upsh_load_file( File, _Pwd, _Load, _Sil, _Old, _Base ) :-
     upsh_error( 'Error while trying to load file':File, 2 ).

upsh_innver_cont_strip( InCs, Atm, Vs, Cont, Rest ) :-
    ( upsh_break_list_on( InCs, 0':, LeftCs, Rest ) ->
        ( LeftCs = [] ->
            Cont = [], Vs = []
            ;
            upsh_in_codes_to_term( LeftCs, Atm, Vs, Cont )
        )
        ;
        Rest = [],
        ( InCs = [] -> 
            Cont = [], Vs = []
            ;
            upsh_in_codes_to_term( InCs, Atm, Vs, Cont )
        )
    ).

upsh_in_codes_to_term( [], _, [], '' ) :-
    !.
% upsh_in_codes_to_term( InCs, Atm, Vs, Term ) :-
upsh_in_codes_to_term( InCs, Atm, Vs, Term ) :-
     ( Atm = any -> true; true ), % instantiate Atm if var,
                                  % which it shouldnt be by this state anyways
     once( upsh_type_and_in_codes_to_term( Atm, InCs, Vs, Term ) ).

upsh_type_and_in_codes_to_term( atm, InCs, Vs, Term ) :-
     Vs = [],
    atom_codes( Term, InCs ).
upsh_type_and_in_codes_to_term( num, InCs, Vs, Term ) :-
     Vs = [],
     number_codes( Term, InCs ).
upsh_type_and_in_codes_to_term( _Any, InCs, Vs, Term ) :-
     catch( number_codes(Term,InCs), _Excp, fail ),
     Vs = [].
upsh_type_and_in_codes_to_term( _Any, InCs, Vs, Term ) :-
    atom_codes( PrvTerm, InCs ),
    (    append(PfxInCs,[0'/|_],InCs),
        atom_codes( PfxTerm, PfxInCs ),
        upsh_seems_like_a_file( PfxTerm, PrvTerm )
          ;
          upsh_seems_like_a_file( '.', PrvTerm)
     ),
    Term = PrvTerm, 
     Vs = [].
upsh_type_and_in_codes_to_term( _Any, InCs, Vs, Term ) :-
     upsh_protect_period( InCs, [], PInCs ),
    upsh_pl_call( sicstus(_), upsh_codes_to_term(PInCs,Vs,Term) ),
    upsh_pl_call( yap(_), upsh_codes_to_term(PInCs,Vs,Term) ),
    upsh_pl_call( swi(_), (atom_codes(InAtm,PInCs),atom_to_term(InAtm,Term,Vs)) ).

upsh_tuple_to_list( Tuple, Term ) :-
    upsh_tuple_to_list_1( Tuple, PrvList ),
    ( PrvList=[Term] -> 
        true
        ; 
        Term = PrvList
    ).
upsh_tuple_to_list_1( X, X ) :- X == [], !.
upsh_tuple_to_list_1( X, [X] ) :-
    var( X ), !.
upsh_tuple_to_list_1( (A,B), [A|T] ) :-
    !,
    upsh_tuple_to_list_1( B, T ).
upsh_tuple_to_list_1( X, [X] ).

upsh_to_list( [], [] ) :- !.
upsh_to_list( [H|T], [H|T] ) :- !.
upsh_to_list( Oth, [Oth] ) :- !.

upsh_base_name( Name, Base ) :-
    atom_codes( Name, Codes ),
    reverse( Codes, RevCodes ),
    ( upsh_break_list_on(RevCodes,0'/,LocRevCodes,_R1) ->
        true
        ;
        LocRevCodes = RevCodes
    ),
    reverse( LocRevCodes, LocCodes ),
    ( upsh_break_list_on(LocCodes,0'.,BaseCodes,_R2) ->
        true
        ;
        BaseCodes = LocCodes
    ),
    atom_codes( Base, BaseCodes ).

upsh_codes_to_term( ACs, Vs, Term ) :-
    ( append( _Pre, ".", ACs ) -> 
        Full = ACs
        % append( [0'', 
        ;
        append( ACs, ".", Full )
    ),
    open_chars_stream( Full, FStream ),
    catch( read_term(FStream,Term,[variable_names(Vs)]), _Any, upsh_codes_to_term_excp(ACs,Term) ),
    close( FStream ).

upsh_codes_to_term_excp( ACs, Atom ) :-
    atom_codes( Atom, ACs ).

% this allows scripts to read from upsh dirs.
/*
portray_message( error, Message ) :-
    Message = existence_error(Goal, 1,file,F,_),
    upsh_afn( upsh(F), AbsF, [file_type(source),access(read),file_errors(fail)], _ ),
    % absolute_file_name( upsh(F), AbsF ),
    upsh_pl_call( swi(_), Call=exists_file(AbsF), Call=file_exists(AbsF) ),
    ( call(Call) ->
        AorP = AbsF
        ;
        atom_codes(AbsF,AbsFCs),
        append( AbsFCs, ".pl", AorPcs ),
        atom_codes( AorP, AorPcs ),
        call(Call)
    ),
    Goal =.. [Functor,PrepF|Rest],
    ( PrepF = Mod:_F ->
        true
        ;
        Mod = user
    ),
    AbsGoal =.. [Functor,Mod:AbsF|Rest],
    call( AbsGoal ).
*/

portray_message( informational, abort(user) ) :-
        print_message( help, aborting_execution ),
        upsh_halt( 4 ).
portray_message( informational, prompt(_A,_B,_C,_D,_E) ) :-
    print_message( help, aborting_execution ),
    upsh_halt( 4 ).

upsh_break_list_on_list( [0'\\,Escpd|Xs], List, Brk, [Escpd|L], R ) :-
    !,
    upsh_break_list_on_list( Xs, List, Brk, L, R ).
upsh_break_list_on_list( [X|Xs], List, X, [], Xs ) :-
    memberchk( X, List ),
    !.
upsh_break_list_on_list( [X|Xs], List, Brks, [X|XLa], XRa ) :-
    upsh_break_list_on_list( Xs, List, Brks, XLa, XRa ).

upsh_break_list_on( [X|Xs], X, [], Xs ) :-
    !.
upsh_break_list_on( [X|Xs], Xa, [X|XLa], XRa ) :-
    upsh_break_list_on( Xs, Xa, XLa, XRa ).

upsh_afn( InFileName, Afn, Options, Base ) :-
    upsh_expand_file_name( InFileName, FileName, Base ),
    % write( uefn(FileName) ), nl, flush_output,
     upsh_pl_call_version( sicstus, (3:9:0),
                absolute_file_name(FileName,Afn),
                absolute_file_name(FileName,Afn,Options)
                ),
    upsh_pl_call( swi(_), (
        ( atomic(FileName) -> 
            expand_file_name(FileName,[ExpName|_]), 
            absolute_file_name(ExpName,Afn,Options)
            ; 
            absolute_file_name(FileName,Afn,Options)
        ) ) ),
    % write( absolute_file_name(FileName,Afn,Options) ), nl,
    % absolute_file_name(FileName,Afn,Options),
    % write( absolute_file_name_out(FileName,Afn,Options) ), nl,
    upsh_pl_call( yap(_), (
                upsh_yap_absolute_file_name(FileName,PrvAfn),
                upsh_yap_abs_fn_3( Options, PrvAfn, Afn )
                    ) ),
    !.

upsh_halt( HaltCode ) :-
    ( (HaltCode == 0,upsh_pl(yap(_))) ->
        open_null_stream( Null ),
        set_prolog_flag( user_error, Null ),
        halt( HaltCode )
        ;
        halt( HaltCode )
    ).

upsh_looks_up( File, AbsFile ) :-
    atomic( File ),
    Lookup = '$HOME/bin/cline_upsh/Lookup.pl',
    upsh_pl_call( swi(_), expand_file_name(Lookup,[Index|_]), Index = Lookup ),
    absolute_file_name( Index, AbsIndex ),
    % upsh_pl_call( swi(_), exists_file(AbsIndex), file_exists(AbsIndex) ),
   file_exists( AbsIndex ),
    open( AbsIndex, read, In ),
    ( upsh_has_look_up( In, File, LookUp ) -> 
        Options = [file_type(directory),access(read),file_errors(fail)],
        upsh_afn( LookUp, AbsLook, Options, _ ),
        % absolute_file_name( LookUp, AbsLook ),
        % upsh_pl_call( swi(_), Exists=exists_file(AbsLook), Exists=file_exists(AbsLook) ),
        % ( (call(Exists),
            % file_property(AbsLook,type(directory)) ) ->
        atom_concat( AbsLook, '/', SlLookUp ),
        atom_concat( SlLookUp, File, AbsFile ),
                % ;
                % AbsFile = AbsLook
        % ),
        close( In )
        ; 
        close( In ), fail
    ).

upsh_has_look_up( In, File, LookUp ) :-
    read( In, InTerm ),
    InTerm \== end_of_file,
    ( InTerm = upsh_look_up(File,LookUp) ->
        true
        ;
        upsh_has_look_up( In, File, LookUp )
    ).

upsh_verbose( Term ) :-
    upsh_pl_call( swi(_), flag(upsh_verbose,Verb,Verb), bb_get(upsh_verbose,Verb)),
    ( Verb == on ->
        writeq( Term ), nl
        ;
        true
    ).

upsh_select_nv( [H|T], El , Rem ) :-
    ( var(H) -> 
        Rem = [H|TRem], %2004/05/05.
        upsh_select_nv( T, El , TRem )
        ; 
        ( H = El -> 
            Rem = T
               % mention in doc that upsh is designed to 
               % only pick the minimum of its flags that it sees
               % so users can use same as far as they put upsh one first
            ;
            Rem = [H|TRem],
            upsh_select_nv( T, El , TRem )
        )
    ).

upsh_common_variables( [] ).
upsh_common_variables( [H|T] ) :-
    ( memberchk( H, T ) -> true ; true ),
    upsh_common_variables( T ).

upsh_cleaned_escaped( [], [] ).
upsh_cleaned_escaped( [0'\\,C|T], [C|R] ) :-
    memberchk( C, [0'=,0'\\,0':,0',] ),
    !,
    upsh_cleaned_escaped( T, R ).
upsh_cleaned_escaped( [H|T], [H|R] ) :-
    upsh_cleaned_escaped( T, R ).

upsh_expand_file_name( upsh(Fname), upsh(AtmFname), Base ) :-
    upsh_fname( Fname, AtmFname, Base ).
/* 18.01.26
upsh_expand_file_name( upsh(Fname), Full, Base ) :-
     !,
    upsh_fname( Fname, AtmFname, Base ),
    user:file_search_path( upsh, Path ),
    atom_concat( Path, AtmFname, Full ).
    */
upsh_expand_file_name( Fname, Fname, Base ) :-
    atomic( Fname ),
    upsh_base_name( Fname, Base ).
    % upsh_rm_filename_ext( Fname, Base ).

upsh_fname( Fname, Fname, Base ) :-
    atomic( Fname ),
    !,
    upsh_base_name( Fname, Base ).
    % upsh_rm_filename_ext( Fname, Base ).
upsh_fname( Left/Rem, Fname, Base ) :-
    !,
    upsh_fname( Rem, AtmRem, Base ),
    upsh_fname_left( Left, LeftAtm ),
    atom_concat( LeftAtm, '/', LeftSl ),
    atom_concat( LeftSl, AtmRem, Fname ).
    
upsh_fname_left( Left, Left ) :-
    atomic( Left ),
    !.
upsh_fname_left( Lft/Rgt, Atm ) :-
    upsh_fname_left( Lft, LftAtm ),
    upsh_fname_left( Rgt, RgtAtm ),
    atom_concat( LftAtm, '/', LftSl ),
    atom_concat( LftSl, RgtAtm, Atm ).

    % not in use ? HERE
upsh_rm_filename_ext( Fname, Base ) :-
    atom_codes( Fname, FCs ),
    ( append( BaseCs, [0'.|_], FCs ) ->
        atom_codes( Base, BaseCs )
        ;
        Base = Fname
    ).

upsh_combine_spaced_args( [], [] ).
upsh_combine_spaced_args( [H|T], [Fst|R] ) :-
    ( (upsh_arg_ends_with_no_esc_special( H ),
       T = [Nxt|Fol]  ) -> 
            atom_concat( H, Nxt, Fst )
            ;
            Fst = H,
            Fol = T
    ),
    upsh_combine_spaced_args( Fol, R ).

upsh_spy_this( Spy ) :-
    ( (Spy = null__/Ari,var(Ari)) -> 
        true
        ;
        spy( Spy )
    ).

upsh_swi_remove_duplicates( [], [] ).
upsh_swi_remove_duplicates( [H|T], [H|R] ) :-
    upsh_swi_delete_identicals( T, H, Th ),
    upsh_swi_remove_duplicates( Th, R ).

upsh_swi_delete_identicals( [], _I, [] ).
upsh_swi_delete_identicals( [H|T], I, R ) :-
    H == I,
    !,
    upsh_swi_delete_identicals( T, I, R ).
upsh_swi_delete_identicals( [H|T], I, [H|R] ) :-
    upsh_swi_delete_identicals( T, I, R ).

upsh_protect_period( [], Acc, Pct ) :-
    reverse( Acc, Pct ).
upsh_protect_period( [0'.|T], Acc, P ) :-
    !,
    upsh_atom_cut_off( Acc, [], PLft ),
    upsh_protect_period( T, [], PRgt ),
    ( PRgt = [0''|RRgt] ->
        append( PLft, [0'.|RRgt], P )
        ;
        upsh_atom_code_rgt_off( PRgt, URgt ),
        append( PLft, [0'.|URgt], P )
    ).
upsh_protect_period( [H|T], R, P ) :-
    upsh_protect_period( T, [H|R], P ).

upsh_atom_cut_off( [H|T], Acc, QCs ) :-
    ( (H>=0'a,H=<0'z) ; (H>=0'A,H=<0'Z) ; (H>=0'0,H=<0'9) ; (H==0'_)),
    !,
    upsh_atom_cut_off( T, [H|Acc], QCs ).
upsh_atom_cut_off( List, Acc, QCs ) :-
    reverse( List, Rev ),
    append( Rev, [0''|Acc], QCs ).

upsh_atom_code_rgt_off( [H|T], QCs ) :-
    ( (H>=0'a,H=<0'z) ; (H>=0'0,H=<0'9) ; (H==0'_)),
    !,
    QCs = [H|RCs],
    upsh_atom_code_rgt_off( T, RCs ).
upsh_atom_code_rgt_off( List, [0''|List] ).

upsh_yap_abs_fn_3( Options, PrvAfn, Afn ) :-
    ( memberchk(file_type(source),Options) -> 
        (atom_concat( PrvAfn, '.pl', Afn );Afn=PrvAfn)
        ;
        Afn = PrvAfn
    ),
    ( memberchk(access(read),Options) -> 
        file_exists(Afn)
        ;
        true
    ).

% this is a bit ad-hoc. try to integrate if possible
upsh_seems_like_a_file( _Pfx, File ) :-
   file_exists( File ),
    % upsh_pl_call( swi(_), exists_file(File), file_exists(File)  ),
    !.
upsh_seems_like_a_file( _Pfx, File ) :-
    upsh_pl_call( swi(_), exists_directory(File), fail ),
    !.
upsh_seems_like_a_file( _Pfx, File ) :-
    atom_concat( File, '.pl', FilePl ),
   file_exists( FilePl ),
    % upsh_pl_call( swi(_), exists_file(FilePl), file_exists(FilePl)  ),
    !.
upsh_seems_like_a_file( _Pfx, File ) :-
    atom_concat( File, '.pl', FilePl ),
    upsh_pl_call( swi(_), exists_directory(FilePl), fail ),
    !.
upsh_seems_like_a_file( Pfx, _File ) :-
    upsh_pl_call( swi(_), exists_directory(Pfx), file_exists(Pfx) ).

upsh_error( Mess, Halt ) :-
     upsh_error( Mess ),
     halt( Halt ).

upsh_error( Mess ) :-
     write( user_error, Mess ),
     nl( user_error ), nl( user_error ).

upsh_nth( Stop, Stop, [H|_T], Elem ) :-
     !,
     Elem = H.
upsh_nth( I, Stop, [_H|T], Elem ) :-
     NxI is I + 1,
     upsh_nth( NxI, Stop, T, Elem ).

/*
upsh_single_arity_arg( 1, true, Args, TheArgs ) :-
     \+ var(Args),
     \+ Args=[_|-],
     !,
     TheArgs = Args.
     */
upsh_single_arity_arg( 1, false, Args, TheArg ) :- 
     \+ var(Args),
     Args = TheArg,
     !.
upsh_single_arity_arg( 1, _Wraps, Args, [Args] ) :- !.
upsh_single_arity_arg( _NotOne, _Wraps, Args, Args ).

upsh_arg_ends_with_no_esc_special( Arg ) :-
    member( C, [':',',','='] ),
    atom_concat( Left, C, Arg ),
    \+ atom_concat( _, '\\', Left ).
