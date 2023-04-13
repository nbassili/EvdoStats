:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

% :- find_book('12867416',2019-2020).  %v3
% :- find_book('94700120',2022-2023).  %v4
%  25585  - Γκιούρδας %v4

:- dynamic course/4, cached_course/4.
:- if(exists_source(courses)).
:- [courses].
:- endif.
:- if(exists_source(cached_courses)).
:- [cached_courses].
:- endif.



find_book(B,AcadYear,Options) :-
	not(is_list(B)), !,
	find_book([B],AcadYear,Options).
find_book(Bs,AcadYear,Options) :-
	(member(silent,Options) -> Silent = yes; Silent = no),
	(Silent == yes -> true; (write('Book(s): '), write(Bs), nl, nl)),
	find_book_list(Bs,AcadYear,List,Options),
	calc_total_results(List,NoOfUniversities,NoOfDepartments,NoOfModules),
	(Silent == yes -> true; write_total_results(user,NoOfUniversities,NoOfDepartments,NoOfModules)),
	AcadYear =.. ['-',Year1,Year2],
	atomic_list_concat([results,Year1,Year2|Bs],'-',FileName),
	atom_concat(FileName,'.txt',File),
	write_results(File,Bs,List,NoOfUniversities,NoOfDepartments,NoOfModules), !.

find_book(Bs,AcadYear) :-
	find_book(Bs,AcadYear,[]).

find_book_list(Bs,AcadYear,List,Options) :-
	(member(cache,Options) -> Cache = yes; Cache = no),
	(member(silent,Options) -> Silent = yes; Silent = no),
	AcadYear=Year1-Year2,
	atomic_list_concat(['Πρόγραμμα Σπουδών (',Year1,' - ',Year2,')'],CourseName),
	dtd(html, DTD),
	setof(University-Department-N/Modules,CourseURL^CourseFileName^CourseFilePath^In^HTML^Body^Result^Ind^Module^B^Book^(
		(Cache == yes ->
			(cached_course(University,Department,CourseFileName,CourseName),
			 string_concat("cache/",CourseFileName,CourseFilePath),
			 open(CourseFilePath,read,In,[encoding(utf8)]));
			(course(University,Department,CourseURL,CourseName),
			 http_open(CourseURL,In,[]))
	        ),
		load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
		close(In),
		xpath(HTML,//body,Body),
		member(B,Bs),
		atomic_list_concat(['[',B,']'],Book),
		bagof(ModuleName,Ind^Result^Module^(xpath(Body, //ol(index(Ind))/li/ul/li(contains(text,Book)), Result),
				     		    xpath(Body,//h2(index(Ind),normalize_space),Module),
						    get_module_name(Module,ModuleName)	),
			Modules),
		length(Modules,N),
		(Silent == yes -> true; writelist(user,[University-Department-N/Modules]))
	),List).

% :- book_stats_year('94700120',2022-2023,Stats).  %v4


book_stats_year(B,AcadYear,NoOfUniversities-NoOfDepartments-NoOfModules,Options) :-
	not(is_list(B)), !,
	book_stats_year([B],AcadYear,NoOfUniversities-NoOfDepartments-NoOfModules,Options).
book_stats_year(Bs,AcadYear,NoOfUniversities-NoOfDepartments-NoOfModules,Options) :-
	find_book_list(Bs,AcadYear,List,[silent|Options]),
	calc_total_results(List,NoOfUniversities,NoOfDepartments,NoOfModules).

book_stats_year(Bs,AcadYear,NoOfUniversities-NoOfDepartments-NoOfModules) :-
	book_stats_year(Bs,AcadYear,NoOfUniversities-NoOfDepartments-NoOfModules,[]).


book_stats(_Bs,Year2,Year2,[],_Options) :- !.
book_stats(Bs,Year1,Year2,[Year1-NextYear/StatsYear|Stats],Options) :-
	Year1 < Year2,
	NextYear is Year1 + 1,
	book_stats_year(Bs,Year1-NextYear,StatsYear,Options), !,
	book_stats(Bs,NextYear,Year2,Stats,Options).
book_stats(Bs,Year1,Year2,[Year1-NextYear/0-0-0|Stats],Options) :-
	Year1 < Year2,
	NextYear is Year1 + 1,
	book_stats(Bs,NextYear,Year2,Stats,Options).

book_stats(Bs,Year1,Year2,Stats) :-
	book_stats(Bs,Year1,Year2,Stats,[]).

collect_two_years(Bs, AcYear1, AcYear2, List1, List2, Options) :-
	find_book_list(Bs, AcYear1, List1, [silent|Options]),
	find_book_list(Bs, AcYear2, List2, [silent|Options]).


compare_years_module_list(Bs, Year1, Year2, Deleted, Added, Options) :-
	collect_two_years(Bs, Year1, Year2, List1, List2, Options),
	find_differences_module(List1,List2,Deleted),
	find_differences_module(List2,List1,Added).


find_differences_module(List1,List2,AllDiffs) :-
	find_differences_dept(List1,List2,DeptDiffs),
	findall(U-D-NDiff/ModDiffs,
			(member(U-D-_N1/M1,List1),
			 member(U-D-_N2/M2,List2),
			 findall(Mod,(member(Mod,M1), not(member(Mod,M2))),ModDiffs),
			 length(ModDiffs,NDiff),
			 NDiff>0),
			RestDiffs),
	append(DeptDiffs,RestDiffs,AllDiffs).


compare_years_dept_list(Bs, Year1, Year2, Deleted, Added, Options) :-
	collect_two_years(Bs, Year1, Year2, List1, List2, Options),
	find_differences_dept(List1,List2,Deleted),
	find_differences_dept(List2,List1,Added).


find_differences_dept(List1,List2,Diffs) :-
	findall(U-D-N/M,(member(U-D-N/M,List1), not(member(U-D-_/_,List2))),Diffs).


compare_years_univ_list(Bs, Year1, Year2, Deleted, Added, Options) :-
	collect_two_years(Bs, Year1, Year2, List1, List2, Options),
	find_differences_univ(List1,List2,Deleted),
	find_differences_univ(List2,List1,Added).


find_differences_univ(List1,List2,Diffs) :-
	findall(U-D-N/M,(member(U-D-N/M,List1), not(member(U-_-_/_,List2))),Diffs).

compare_years(B, Year1, Year2, Options) :-
	not(is_list(B)), !,
	compare_years([B], Year1, Year2, Options).
compare_years(Bs, Year1, Year2, Options) :-
	(member(silent,Options) -> Silent = yes; Silent = no),
	collect_two_years(Bs, Year1, Year2, List1, List2, Options),
	find_differences_univ(List1,List2,UDeleted),
	find_differences_univ(List2,List1,UAdded),
	find_differences_dept(List1,List2,DeptDeleted),
	find_differences_dept(List2,List1,DeptAdded),
	find_differences_module(List1,List2,ModDeleted),
	find_differences_module(List2,List1,ModAdded),
	calc_total_results(UDeleted,NoOfUniversitiesD,_,_),
	calc_total_results(UAdded,NoOfUniversitiesA,_,_),
	calc_total_results(DeptDeleted,_,NoOfDepartmentsD,_),
	calc_total_results(DeptAdded,_,NoOfDepartmentsA,_),
	calc_total_results(ModDeleted,_,_,NoOfModulesD),
	calc_total_results(ModAdded,_,_,NoOfModulesA),
	(Silent == yes ->
		true;
		write_comparison_results(user,Bs,UDeleted,DeptDeleted,ModDeleted,NoOfUniversitiesD,NoOfDepartmentsD,NoOfModulesD,UAdded,DeptAdded,ModAdded,NoOfUniversitiesA,NoOfDepartmentsA,NoOfModulesA)),
	Year1 =.. ['-',Year1a,Year1b],
	Year2 =.. ['-',Year2a,Year2b],
	atomic_list_concat(['comp-results',Year1a,Year1b,Year2a,Year2b|Bs],'-',FileName),
	atom_concat(FileName,'.txt',File),
	write_comparison_results(File,Bs, UDeleted,DeptDeleted,ModDeleted,NoOfUniversitiesD,NoOfDepartmentsD,NoOfModulesD,UAdded,DeptAdded,ModAdded,NoOfUniversitiesA,NoOfDepartmentsA,NoOfModulesA).

compare_years(B, Year1, Year2) :-
	compare_years(B, Year1, Year2, []).

% :- compare_books([['94700120'],['102070469','13909']],2022-2023,[cache]).
% 
compare_books(Bs, AcadYear, Options) :-
	compare_books_aux(Bs, AcadYear, Options, ListOfLists, ListOfUDM),
	AcadYear =.. ['-',Year1,Year2],
	flatten(Bs,Books),
	atomic_list_concat(['mult-results',Year1,Year2|Books],'-',FileName),
	atom_concat(FileName,'.txt',File),
	write_mult_results(File,Bs, ListOfLists,ListOfUDM), !.

compare_books_aux([], _AcadYear, _Options, [], []).
compare_books_aux([B|RestB], AcadYear, Options, [List1|RestLoL], [NoOfUniversities-NoOfDepartments-NoOfModules|RestUDM]) :-
	(member(silent,Options) -> Silent = yes; Silent = no),
	(Silent == yes -> true; (nl, write("Book(s) : "), write(B), nl)),
	find_book_list(B, AcadYear, List1, Options),
	calc_total_results(List1,NoOfUniversities,NoOfDepartments,NoOfModules),
	(Silent == yes -> true; write_total_results(user,NoOfUniversities,NoOfDepartments,NoOfModules)),
	compare_books_aux(RestB, AcadYear, Options, RestLoL, RestUDM).

compare_books_stats(Bs, AcadYear, ListOfUDM, Options) :-
	compare_books_aux(Bs, AcadYear, [silent|Options], _ListOfLists, ListOfUDM).


write_mult_results(File,Bs, ListOfLists,ListOfUDM) :-
	open(File,write,Res,[encoding(utf8)]),
	write_mult_results_aux(Res,Bs, ListOfLists,ListOfUDM),
	close(Res).

write_mult_results_aux(_File,[],[],[]).
write_mult_results_aux(File,[B|RestB],[List|RListOfLists],[U-D-M|RListOfUDM]) :-
	nl(File), write(File,"Book(s) : "), write(File,B), nl(File),
	writelist(File,List),
	write_total_results(File,U,D,M),
	write_mult_results_aux(File,RestB,RListOfLists,RListOfUDM).

get_module_name(ModuleIn,ModuleOut) :-
	sub_string(ModuleIn,B,_L,A,":"),
	B1 is B+2,
	A1 is A - 1,
	A1 >= 0,
	sub_string(ModuleIn,B1,A1,0,ModuleOut).

write_shortlist([H]) :-
	write(H),
	write('.'), !.
write_shortlist([H|T]) :-
	write(H),
	write(', '),
	write_shortlist(T).

writelist(_,[]).
writelist(F,[U-D-N/Modules|T]) :-
	write(F,'ΤΜΗΜΑ '), write(F,D), write(F,', '),
	write(F,U),  write(F,', '),
	write(F,"Μαθήματα ("), write(F,N), write(F,'): '), write_shortlist(F,Modules), nl(F),
	writelist(F,T).

write_shortlist(F,[H]) :-
	write(F,H),
	write(F,'.'), !.
write_shortlist(F,[H|T]) :-
	write(F,H),
	write(F,', '),
	write_shortlist(F,T).


cache_one_course(CourseURL,LocalCourseFile) :-
%	'https://service.eudoxus.gr/public/departments/courses/2381/2010'
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	http_open(CourseURL,In,[]),
	open(LocalCourseFilePath,write,Out,[encoding(iso_latin_1)]),
	copy_stream_data(In, Out),
	close(In),
	close(Out), !.

inc_cache_one_course(CourseURL,LocalCourseFile,Cached) :-
	sub_string(CourseURL,54,_L,0,LastPart),
	split_string(LastPart,"/","",List),
	atomics_to_string(List,'_',StringLastPart),
	string_concat(StringLastPart,".html",LocalCourseFile),
	string_concat("cache/",LocalCourseFile,LocalCourseFilePath),
	(exists_file(LocalCourseFilePath)
	  ->
		Cached=false ;
		( http_open(CourseURL,In,[]),
		  open(LocalCourseFilePath,write,Out,[encoding(iso_latin_1)]),
		  copy_stream_data(In, Out),
		  close(In),
		  close(Out),
		  Cached=true
		 )
	),
	!.

cache_courses :-
	(exists_directory(cache) ->
		true;
		make_directory(cache)),
	tell('cached_courses.pl'),
	cache_courses_aux,
	told.

cache_courses_aux :-
	course(University,Department,CourseURL,CourseName),
	write(user,"Caching: "),
	write(user,University-Department-CourseName),
	nl(user),
	cache_one_course(CourseURL,LocalCourseFile),
	write_term(cached_course(University,Department,LocalCourseFile,CourseName),[quoted(true)]),
	write('.'), nl,
	fail; true.

inc_cache_courses :-
	tell('cached_courses.pl'),
	inc_cache_courses_aux,
	told.

inc_cache_courses_aux :-
	course(University,Department,CourseURL,CourseName),
	inc_cache_one_course(CourseURL,LocalCourseFile,Cached),
	(Cached == true ->
		(write(user,"Cached: "),
		 write(user,University-Department-CourseName),
		 nl(user));
		true),
	write_term(cached_course(University,Department,LocalCourseFile,CourseName),[quoted(true)]),
	write('.'), nl,
	fail; true.

strings_to_atoms([],[]).
strings_to_atoms([S|RestS],[A|RestA]) :-
	atom_string(A,S),
	strings_to_atoms(RestS,RestA).

write_results(File,Bs,List,NoOfUniversities,NoOfDepartments,NoOfModules) :-
	open(File,write,Res,[encoding(utf8)]),
	write(Res,'Book(s): '), write(Res,Bs), nl(Res), nl(Res), 
	writelist(Res,List),
	write_total_results(Res,NoOfUniversities,NoOfDepartments,NoOfModules),
	close(Res).

calc_total_results([],0,0,0) :- !.
calc_total_results(List,NoOfUniversities,NoOfDepartments,NoOfModules) :-
	setof(University,Department^Modules^member(University-Department-Modules,List),Universities),
	length(Universities,NoOfUniversities),
	length(List,NoOfDepartments),
	findall(N,member(University-Department-N/Modules,List),ModuleList),
	sumlist(ModuleList,NoOfModules).

write_total_results(S,NoOfUniversities,NoOfDepartments,NoOfModules) :-
	nl(S),
	(nonvar(NoOfUniversities) -> (write(S,"Συνολικός Αριθμός Πανεπιστημίων: "), write(S,NoOfUniversities), nl(S))),
	(nonvar(NoOfDepartments) -> (write(S,"Συνολικός Αριθμός Τμημάτων: "), write(S,NoOfDepartments), nl(S))),
	(nonvar(NoOfModules) -> (write(S,"Συνολικός Αριθμός Μαθημάτων: "), write(S,NoOfModules), nl(S))).

write_comparison_results(File,Bs,UDeleted,DeptDeleted,ModDeleted,NoOfUniversitiesD,NoOfDepartmentsD,NoOfModulesD,UAdded,DeptAdded,ModAdded,NoOfUniversitiesA,NoOfDepartmentsA,NoOfModulesA) :-
	(File == user -> S = user; open(File,write,S,[encoding(utf8)])),
	write(S,'Book(s): '), write(S,Bs), nl(S), nl(S), 
	nl(S), write(S,'Αφαιρέθηκαν:'), nl(S), nl(S),
	write(S,'Πανεπιστήμια:'), nl(S),
	writelist(S,UDeleted), nl(S),
	write(S,'Τμήματα:'), nl(S),
	writelist(S,DeptDeleted), nl(S),
	write(S,'Μαθήματα:'), nl(S),
	writelist(S,ModDeleted),
	write_total_results(S,NoOfUniversitiesD,NoOfDepartmentsD,NoOfModulesD),
	nl(S), write(S,'Προστέθηκαν:'),  nl(S), nl(S),
	write(S,'Πανεπιστήμια:'), nl(S),
	writelist(S,UAdded), nl(S),
	write(S,'Τμήματα:'), nl(S),
	writelist(S,DeptAdded), nl(S),
	write(S,'Μαθήματα:'), nl(S),
	writelist(S,ModAdded),
	write_total_results(S,NoOfUniversitiesA,NoOfDepartmentsA,NoOfModulesA), nl(S),
	(S == user -> true; close(S)).

/*
extract courses from web page directly
*/


init_cache :-
	( exists_file('courses.pl') ->
		copy_file('courses.pl','courses-old.pl');
		true),
	( exists_file('cached_courses.pl') ->
		copy_file('cached_courses.pl','cached_courses-old.pl');
		true),
	extract_course_list(List),
	transform_univ_data(List,A),
	flatten(A,Courses),
	convert_to_courses(Courses),
	abolish(course/4),
	consult('courses.pl'),
	cache_courses,
	abolish(cached_course/4),
	consult('cached_courses.pl').

update_cache :-
	( exists_file('courses.pl') ->
		copy_file('courses.pl','courses-old.pl');
		true),
	( exists_file('cached_courses.pl') ->
		copy_file('cached_courses.pl','cached_courses-old.pl');
		true),
	extract_course_list(List),
	transform_univ_data(List,A),
	flatten(A,Courses),
	convert_to_courses(Courses),
	abolish(course/4),
	consult('courses.pl'),
	inc_cache_courses,
	abolish(cached_course/4),
	consult('cached_courses.pl').

extract_course_list(List) :-
	dtd(html, DTD),
	http_open('https://service.eudoxus.gr/public/departments',In,[]),
	load_structure(In, HTML, [ dtd(DTD), dialect(sgml), shorttag(false), max_errors(-1),syntax_errors(quiet),encoding('utf-8') ]),
	close(In),
	findall(GlobInd-ElName-Content,
		(   xpath(HTML,//body/div/div/'*'(index(GlobInd)),X),
		    GlobInd >= 5,
		    extract_content(X,ElName,Content)
		),
		List).

extract_content(element(h2,[],SubTree),h2,UnivName) :- !,
	xpath(element(h2,[],SubTree),/self(normalize_space),UnivName).
extract_content(element(p,[],SubTree),p,DeptName) :- !,
	xpath(element(p,[],SubTree),/self(normalize_space),DeptName).
extract_content(element(ul,[],SubTree),ul,DeptCourses) :- !,
	extract_courses(element(ul,[],SubTree),DeptCourses).


transform_univ_data(List,[UList|ARest]) :-
	append([_N-h2-U|UData],[N1-h2-U1|L3],List), !,
	transform_univ_data_aux(U,UData,UList),
	transform_univ_data([N1-h2-U1|L3],ARest).
transform_univ_data(List,[UList]) :-
	append([_N-h2-U|UData],[],List), !,
	transform_univ_data_aux(U,UData,UList).
transform_univ_data([],[]).

transform_univ_data_aux(_U,[],[]) :- !.
transform_univ_data_aux(U,[_N-p-DeptName,_M-ul-DeptCourses|RestUData],[U-DeptName-DeptCourses|RestUList]) :-
	transform_univ_data_aux(U,RestUData,RestUList).

convert_to_courses(List) :-
	tell('courses.pl'),
	convert_to_courses_aux(List),
	told.

convert_to_courses_aux([]).
convert_to_courses_aux([Univ-DeptName-DeptCourses|Rest]) :-
	convert_to_courses_aux2(Univ,DeptName,DeptCourses),
	convert_to_courses_aux(Rest).

convert_to_courses_aux2(_Univ,_DeptName,[]).
convert_to_courses_aux2(Univ,DeptName,[CourseURL-CourseTitle|Rest]) :-
	Fact =.. [course,Univ,DeptName,CourseURL,CourseTitle],
	write_term(Fact,[quoted(true)]), write('.'), nl,
	convert_to_courses_aux2(Univ,DeptName,Rest).

extract_courses(Dept,DeptCourses) :-
	findall(CourseURL-CourseTitle,
		(   xpath(Dept,li,Course),
		    xpath(Course,a(@href),CourseURL1),
		    atom_concat('https://service.eudoxus.gr',CourseURL1,CourseURL),
		    xpath(Course,a(text),CourseTitle)
		),
		DeptCourses).
