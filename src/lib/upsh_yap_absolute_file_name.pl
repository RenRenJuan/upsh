upsh_yap_absolute_file_name( File, File ) :-
	atomic( File ),
	file_exists( File ),
	!.
	
upsh_yap_absolute_file_name( FileTerm, AbsFile ) :-
	FileTerm =.. [Alias,File],
	atom_codes( File, FileCs ),
	file_search_path( Alias, Path ),
	atom_codes( Path, PathCs ),
	append( PathCs, [0'/|FileCs], SearchCs ),
	atom_codes( Search, SearchCs ),
	( exists(Search) ->
		AbsFile = Search
		;
		atom_codes( Search, SearchCs ),
		append( SearchCs, ".pl", AbsFileCs ),
		atom_codes( AbsFile, AbsFileCs ),
		exists( AbsFile )
	),
	!.

upsh_yap_absolute_file_name( File, AtomicFile ) :-
	( atomic(File) -> 
		% AtomicFile = File 
		absolute_file_name( File, AtomicFile )
		;
		File =.. TermListFile,
		last( TermListFile, NxFile ),
		upsh_yap_absolute_file_name( NxFile, AtomicFile )
	).
