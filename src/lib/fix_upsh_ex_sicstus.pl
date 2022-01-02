:- ensure_loaded( library(lists) ).  			% append/3, memberchk/2.
:- ensure_loaded( library(system) ).  			% exec/3.

:- ensure_loaded( sys_info ). 			% /1.
:- ensure_loaded( fget_line ). 			% /2.
:- ensure_loaded( fput_line ). 			% /2.
:- ensure_loaded( copy_stream_on_stream ).	% /2.
:- ensure_loaded( flatoms ). 				% /2.

main( Args ) :-
	( memberchk(exec(SicstusExec),Args) ->
		atom_codes( SicstusExec, PrvSicstusExecCs )
		;
		exec( 'which sicstus', [std,pipe(In),std], _Pid ),
		fget_line( In, PrvSicstusExecCs )
	),
	upsh_pl_call( geq(sicstus(3:12:2)), 
			append( PrvSicstusExecCs, " --noinfo ", SicstusExecCs ), PrvSicstusExecCs = SicstusExecCs ),
	% environ( cputype, CpuType ),
	open( 'upsh_sicstus.sav', read, InStream ),
	fget_line( InStream, LineFirst ),
	fget_line( InStream, LineSecond),
	Pre3122 = (SicsSfx = " 2> /dev/null"),
	Pst3122 = (SicsSfx = " " ),
	upsh_pl_call( geq(sicstus(3:12:2)), Pst3122, Pre3122 ),
	% append( Prefix, SicsSfx, LineNew ),
	append( "exec sicstus", OrgSufix, LineSecond ),
	append( "exec ", SicstusExecCs, Exec ),
	append( Exec, OrgSufix, SicsExec ),
	append( SicsExec, " upsh_exec=$0 ", ExecPfx ),
	% append( ExecPfx, OrgSufix, LineNewPfx ),
	append( ExecPfx, SicsSfx, LineNew ),
	% add exec ( /FullPath/)sicstus
	% append( LineSecond, " 2> /dev/null", LineNew ),
	open( upsh_sicstus_dn, write, OutStream ),
	fput_line( OutStream, LineFirst ),
	fput_line( OutStream, LineNew   ),
	copy_stream_on_stream( InStream, OutStream ),
	close( InStream ),
	close( OutStream),
	sys_info( 'chmod og-xr upsh_sicstus_dn' ),
	sys_info( 'chmod ugo+x upsh_sicstus_dn' ),
	% flatoms( ['mv upsh_dn $HOME/bin/',CpuType,'/',upsh], FinMv ),
	% flatoms( ['mv upsh_sicstus_dn $HOME/bin/',upsh], FinMv ),
	FinMv = 'mv upsh_sicstus_dn $HOME/bin/upsh',
	sys_info( FinMv ),
	sys_info( 'rm upsh_sicstus.sav' ).
