Overview.
--------

This is upsh (Unix-Prolog SHell, or something like that-
btw, I pronounce it, oupssss). The unix scripts in this
directory will built an executable state (upsh*). This can
be used for running Prolog programs from the unix command line.

There are two main ways in which the state can be used:
(a) command_line invocation, by issuing something like,
   % upsh say hello world
(b) script invocation, by 
   % say_pl bye

In both cases, execution will start from <Script>/1,0 or main/1,0
one of which should be defined in source Program (this case `say.pl').
If more that one of these predicates are defined, the first one in
the order given above will be executed.

See doc/upsh-manual.ps for more information.

Prologs.
-------

Upsh was written  on SICStus prolog.
Currently is known to work with anything later than :
	SICStus (3.8.5)
	SWI (4.0.2)
	Yap (4.3.18)

Installation.
------------
For the Prolog of you choice do 
  % ./make_Prolog

If this works, it will create an upsh* executable in ~/bin/
Obviously if you want upsh-es for more than one Prolog engine,
move this executable to (say) ~/bin/upsh_Prolog1
and repeat the above for Prolog2.

Things that you can change are :
(a) the dir in which upsh*  will go. Edit make_<Prolog> 
(b) the dir where command_line invocations of upsh* will search
    for sources. Edit the file_search_path( upsh, '~/bin/upsh_cline' )
    line of file src/upsh.pl

After installation do 
   % rehash 
(or the equivelant thing for your shell)
to make current shell, aware of the new executable.

If you want to use the automatic finding of command_line sources,
create the directory ~/bin/upsh_cline or the directory of (b) above.
Any Prolog program left there will be accessible to upsh for command_line
invocation (see examples). In addition you can create file Lookup.pl
in the same directory. See scripts/Lookup.pl for an example file.

Upsh options.
------------

The proper syntax for invoking upsh* expects a number of
atoms that are taken to be upsh* options followed by a -
seperator, followed by the Program to be executed and its 
Arguments. Thus,
   upsh {h,v}~ {<c>,s} {<m>,n} [b,e/1,f/1,d,p,o,r,w,a]- Program [Arguments]*

Curly brackets ({}) mean either-or.  Less, and greater mean default value.
[Argumentss]* means a number of arguments (possibly none).
Things starting with a capital letter are place-holders, lower case 
starters are literals.
Order is not important (apart of the placement of `-', that is).

Normally any progam arguments (Arg) of the form nice=tara, will
be translated to nice(tara) (similarly is=no=good goes to is(no(good))).
These will be seen as terms in the call to the script. If 
you use '-' and option 'o' then the is=no=good is passed as picked 
by current_prolog_flag( argv, AllArgs ), (i.e. as one atom).
Every other Arg is passed as atom (including numbers, so make sure
you convert them, before using them in your program).

Sloppy syntax, is also supported. This allows for the omission of `-'.
It is the default behaviour (change with 'r' option) and it means
upsh* will take anything that looks like an upsh option
from the command line and treats anything that it didnt use 
as if it appearred after a hyphen, '-'. (In this case your 
Args will get translated for sure because that's what happens to
upsh arguments).

The following options are thought to be recognised.

h : prints a help message (on error stream) and exits.
v : prints the version id (on error stream) and exits.
b : be verbose
c : command line invocation (default).
s : script invocation.
e : errors file (default: File=std_out). This the only way to redirect
      the error stream on some Prologs, since upsh muffles the std_err
	   to avoid the logo message of these Prologs. You can use /dev/null
	   ok. Also, dev_null is recognised.
f : name of functor for top goal (default: <filename>/1{.pl} then, main/1)
d : working directory (default: Dir=\'.\').
m : compile Program (default).
n : consult Program. Not very useful as is because upsh will throw
      you out of the Prolog system, in the event of something gone
		wrong, anyway. The original use was to be able to debug when
		execution failed/aborted unexpectedly. If you know what you
		are doing comment out the portray_message( informational, _ )
		lines in upsh_Prolog.pl rebuild and you can then do some 
		debugging from the Prolog prompt.
p : surpress loading info (of Program). 
      All those compiling and compiled messages.
o : donot translate, Arg. thus `very=nice=tara' behind `-' will not be
      translated to term very(nice(tara)).
r : be strict about the \`-\' . Options to the left Program and it Args to
      the right. Anything that is not an upsh option and appears to the 
		left goes to the bin. Similarly Args that could be seen as 
      upsh options and appear to the right of '-' will be passed to the
		Program.
w : wait for input at end of execution.
a : last argument holds all read variable lists (upsh_vs/1).

For scripts add the following, as the first two lines in the prolog file,

/bin/sh
exec upsh s - $0 "$@"

Then, do :
   % chomod u+x <PlFile>

If you have problems with working in the wrong directory,
try the following as a second line, instead :

exec upsh s d=`pwd` - $0 "$@

Examples.
--------

Upsh, comes with two examples, say and tall which can be
found in directory scripts/ . 'say' simply spews out its
input arguments one at the time. tall, touches all its
arguments (where `touch'; is the unix command).
These come in two flavours, File.pl  for running  as an upsh*
command_line argument, or used from within the interpreter,
while File_pl is an executable script.

Some examples with these two programs, follow :

% upsh say hello world
% upsh p - say hello tara
% upsh n - say bye
% upsh e=errors - say hello all
% more errors 
% rm errors
% ./say_pl hello 
% cp tall_pl ~/bin/tall                (or similar dir in your $PATH)
% rehash                               (or similar, for your shell)
% tall tara sha tom                    (these 3 files will be `touched')
% rm ~/bin/tall
% rm tara sha tom
% upsh d='..' - upsh/tall.pl panos petros dhmhtra giannis
                                       (these 4 files will be `touched' on ..)
% rm ../panos ../petros ../dhmhtra ../giannis
% upsh upsh/tall.pl d='..' panos petros dhmhtra giannis
                                       (illustrates sloppy syntax) 
% rm ../panos ../petros ../dhmhtra ../giannis
% upsh say kalh=sebasth
% upsh o - say tavladoros=andrea       (illustrates translation) 
% mkdir ~/bin/upsh_cline               (if you havent done so yet)
% cp say.pl ~/bin/upsh_cline           
% cd ~
% upsh say bye-t-bye                   (~/bin/upsh_cline/say.pl is used)
% rm ~/bin/upsh_cline/say.pl           (if you so wish)

Distribuition.
-------------

This software is not distributated, per se. It simply exists.
Whether you use it or not, is your problem.

Interesting suggestions, can be send to n.angelopoulos@nki.nl

Nicos, April 2004.

Last update: April 2012.
