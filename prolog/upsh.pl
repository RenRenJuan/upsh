:- module( upsh, [upsh_make/0,upsh_make/1,upsh_version/1,upsh_version/2] ).

:- ensure_loaded( '../src/upsh_version' ).

upsh_make_defaults( Defs ) :- 
    current_prolog_flag( home, SwiHome ),
    % 1st place to look: ../../bin/swipl relative to flag home
    file_directory_name( SwiHome, SwiLib ),
    file_directory_name( SwiLib, BaseD ),
    directory_file_path( BaseD, bin, BinD ),
    Exec = upsh,
    ( exists_directory(BinD) ->
        Defs = [exec(Exec),bin_dir(BinD)]
        ;
        write( unable_to_locate_bin_dir_for_upsh__use_option(bin_dir) ), nl,
        Defs = [exec(Exec)]
    ).

/** <module> Unix to Prolog shell.

---+ Upsh 2.5

Upsh stands for Unix to Prolog shell. It is a Prolog program which can be used to run Prolog programs from the command line or as scripts.<br>
It origianlly ran on three prolog engines without changes to the source code. 
Current versions are only tested on SWI-Prolog.

With version 2.*, Upsh has all the features I had envisaged and many which I just thought of on the way. 
It is also fairly stable. <br>
The development has now been switched to SWI and upsh is also provided as an easy to install SWI pack.<br>

It is unlikely there will be any major releases in the future. I will of course get fixes on reported bugs 
or add features that are interesting. <br>
For reporting a bug or feature requests contact me:

http://stoics.org.uk/~nicos/sware/contact.html

If you use Upsh for a while I would appreciate an email with the kind of scripts you using it with.

The pack has a single main predicate which creates an executable state that provides a convenient way of executing prolog scripts for the command line.<br> 
The state can be invoked by calling the created executable (upsh) on command line. <br>
By default the upsh binary is placed in same directory as the swipl executable. Only tested on linux. 


==
Install:
?- pack_install(upsh).

Load: 
?- use_module(library(upsh)).

Create the executable.
?- upsh_make.

test.

?- halt.

-- ask your shell to re-read its executables with something like: rehash

upsh say naku

%  /home/nicos/.rcpl compiled 0.00 sec, 8 clauses
% /home/nicos/bin/cline_upsh/say.pl compiled 0.00 sec, 5 clauses
naku

lykos;upsh/scripts% upsh v
upsh_exec(upsh(2:2:1),swi(7:7:13),built_on(2018/5/2,14:42:49))
==

The executable will look into three places for scripts. 
  * local directory
  * $HOME/bin/cline
  * PACK/scripts  for any installed PACK

Upsh will also convert os friendly arguments to Prolog terms:

==
upsh say a=b
%  /home/nicos/.rcpl compiled 0.00 sec, 8 clauses
% /home/nicos/bin/cline_upsh/say.pl compiled 0.00 sec, 5 clauses
a(b)
==

The executable takes a number of one letter flags:

==
upsh say p a=b
%  /home/nicos/.rcpl compiled 0.00 sec, 8 clauses
a(b)

upsh say f p a=b
a(b)
==

By default the executable looks into script say.pl for main/0, main/1, main/n, say/0, say/1, say/n and calls it with the appropriate number of arguments.

@author nicos angelopoulos
@version  2.3 2018/5/2
@version  2.4 2018/12/10  simplified upsh_make/1 (no more shell call to build the executable).
@version  2.5 2019/12/23  work around new SWI configs

*/

/** upsh_make.
    upsh_make( Opts ).

Create an upsh executable for running Prolog scripts from command line.

Opts
  * bin_dir(BinDir=SwiBin) 
     directory for putting the executable in, defaults to the one holding swipl
     constructed from current_prolog_flag( home, Home ).

  * exec(Exec=upsh)
     name of executable

@author Nicos Angelopoulos
@version  0.1 2017/06/03
@version  0.2 2018/12/10

*/
upsh_make :-
    upsh_make( [] ).

upsh_make( ArgS ) :-
    \+ var(ArgS),
    ( is_list(ArgS) -> Args = ArgS; Args = [ArgS] ),
    upsh_make_defaults( Defs ),
    append( Args, Defs, Opts ),
    multifile( upsh_built_call/1 ),
    dynamic( upsh_built_call/1 ),
    asserta( upsh_built_call(true) ),
    load_files( pack('upsh/src/upsh') ), 
    memberchk( exec(Exec), Opts ),
    memberchk( bin_dir(BinD), Opts ),
    directory_file_path( BinD, Exec, AbsExec ),
    qsave_program( AbsExec, [init_file(none_what_noever),goal(upsh_exec:upsh)] ),
    abolish( upsh_built_call/1 ).

/* pre 18.12.10:
upsh_make( ArgS ) :-
    \+ var(ArgS),
    ( is_list(ArgS) -> Args = ArgS; Args = [ArgS] ),
    upsh_make_defaults( Defs ),
    append( Args, Defs, Opts ),
    % delete_local_exec,
    once( absolute_file_name( pack(upsh), UpshD ) ),
    % working_directory( Old, UpshD ),
    % Shell ='swipl -f upsh_create.pl -g upsh_create',
    debug( upsh, 'Shelling: ~w', Shell ),
    shell( Shell ),
    memberchk( exec(Exec), Opts ),
    memberchk( bin_dir(BinD), Opts ),
    directory_file_path( BinD, Exec, PathTo ),
    % rename_file( 'bin/upsh', PathTo ),
    atom_concat( 'mv bin/upsh ', PathTo, MvTo ),
    shell( MvTo ),
    debug( upsh, 'Moved to: ~p', PathTo ),
    working_directory( _, Old ),
    debug( upsh, 'Done', true ).
    */

delete_local_exec :-
    Loc = 'bin/upsh',
    exists_file( Loc ),
    !,
    delete_file( Loc ).
delete_local_exec.
