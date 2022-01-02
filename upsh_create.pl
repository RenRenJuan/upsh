
% :- debug( upsh_create ).
% 18.12.10: no longer used.... ?!

upsh_create :-
    multifile( upsh_built_call/1 ),
    dynamic( upsh_built_call/1 ),
    asserta( upsh_built_call(true) ),
    load_files( 'src/upsh' ), 
    Exec = 'bin/upsh',
    ( exists_directory(bin) -> true; make_directory(bin) ),
    qsave_program( Exec, [init_file(none_what_noever),goal(upsh_exec:upsh)] ), 
    debug( upsh_create, 'Saved on: ~p', Exec ),
    % once( absolute_file_name( path(swipl), Swipl ) ),
    % directory_file_path( Parent, _SwiplExec, Swipl ),
    % directory_file_path( Parent, upsh, UpshDst ),
    % rename_file( Exec, UpshDst ), $ bug? this does not work
    % atomic_list_concat( [mv,Exec,UpshDst], ' ', UpshMv ),
    % debug( upsh_create, 'Moved to: ~p', UpshDst ),
    % Pfx = 'bin/upsh f atm=src/lib/fix_upsh_ex_swi pl=',
    % ShArgs = ' f src/lib/fix_upsh_ex_swi pl=',
    % Psfx = ' pupsh=pupsh',
    % current_prolog_flag( executable, PlExec ),
    % atomic_list_concat( [Exec,ShArgs,PlExec,Psfx], Shell ),
    % debug( upsh_create, 'Shelling: ~w', Shell ),
    % write( shell(Shell) ), nl,
    % shell( Shell ),
    halt.

% :- initialization( upsh_create ).
