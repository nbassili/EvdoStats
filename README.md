EvdoStats: EVDOXUS book statistics in Prolog

A Prolog application for exporting statistics about book from the Evdoxus site
==============================================================================

- (1) When you run the Program for the first time, you should run


		?- init_cache.
		

This initializes the cache, downloading all course pages.

- (2) Each year, you should run 


		?- update_cache.
		

This incrementally caches the course pages of the new academic year.

- (3) When you want to see details and statistics for a book for a specific academic year:


		book_details_year(+BooksEvdoxusIDs,+AcademicYear,+Options).
		

Provides statistics for a single or multiple books, i.e. at which Universities,
Departments, Modules this book is distributed through Evdoxus, on-screen
(optional) and stored at a file with name 'results-<<AcademicYear>>-<<BooksEvdoxusIDs>>.txt'.

You provide either one Book ID in Evdoxus (no list), or a list of Books IDs.
For a list of Books, statistics are aggregated for all books.
AcademicYear is in the form e.g. 2022-2023.
Options is a list (possibly empty) containing the following:
   - cache: read course pages from cache instead from the Web (recommended)
   - silent: do not print results on-screen (only at file)
   - nocode: do not print the module code after the module name

e.g. 

		?- book_details_year('94700120',2022-2023,[cache,silent]).

or   

		?- book_details_year(['94700120'],2022-2023,[cache,silent]).

(The filename that results are stored is 'results-2022-2023-94700120.txt').

		book_details_year(BooksEvdoxusIDs,AcademicYear).

This is equivalent to book_details_year(BooksEvdoxusIDs,AcademicYear,[]).

- (4) When you want to find out only the statistics for a book for a specific academic year:


		book_stats_year(+BooksEvdoxusIDs,+AcademicYear,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books
aggregatively for an academic year, in the form: NoOfUniversities - NoOfDepartments - NoOfModules

The rest of the arguments have exactly the same meaning as with book_details_year/3.

e.g. 

		?- book_stats_year('94700120',2022-2023,Stats,[cache]).
		Stats = 21-58-83

- (5) When you want to find out the statistics for a book for several academic years:


		book_stats(+BooksEvdoxusIDs,+Year1,+Year2,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books
aggregatively for all the academic years between Year1 and Year2, in a list of
the form:

		[AcademicYear1/NoOfUniversities1-NoOfDepartments1-NoOfModules1, ..., AcademicYearN/NoOfUniversitiesN-NoOfDepartmentsN-NoOfModulesN]

Year1 and Year2 are integers (e.g. 2020, 2023, ...). 
The rest of the arguments have exactly the same meaning as with book_details_year/3.

e.g.

		?- book_stats(['94700120','12867416'],2019,2023,Stats,[cache]).
		Stats = [2019-2020/(20-42-59), 2020-2021/(21-49-73), 2021-2022/(20-54-83), 2022-2023/(21-58-83)].

- (6) When you want to compare the statistics for a book for two academic years,
i.e. which Universities/Departments/Modules have been added in the second
academic year compared to the first and which Universities/Departments/Modules
in the first academic year have been deleted from the second:


		compare_years(+BooksEvdoxusIDs,+AcademicYear1, +AcademicYear2, +Options).

Provides comparative details and statistics for a single or multiple books
(aggregatively) between two (not necessarily consecutive) academic years, i.e.
which Universities/Departments/Modules have been added in the second academic
year compared to the first and which Universities/Departments/Modules in the
first academic year have been deleted from the second, on-screen (optional) and
stored at a file with name 'comp-results-<<AcademicYear1>>-<<AcademicYear2>>-<<BooksEvdoxusIDs>>.txt'.

The arguments have exactly the same meaning as with book_details_year/3.

e.g. 

		?- compare_years('94700120',2021-2022,2022-2023,[cache]).

(The filename that the results are stored is 'comp-results-2021-2022-2022-2023-94700120.txt')

- (7) When you want to retrieve (in lists) the statistics for a book for two
academic years, i.e. which Universities (including details about Departments/
Modules) have been added in the second academic year compared to the first and
which Universities (including details about Departments/Modules) in the first
academic year have been deleted from the second:


		compare_years_univ_list(+BooksEvdoxusIDs, +AcademicYear1, +AcademicYear2, -Deleted, -Added, +Options).

Provides comparative statistics for multiple books (aggregatively) between two
(not necessarily consecutive) academic years, about which Universities
(including details about Departments/Modules) have been added in the second
academic year compared to the first and which Universities (including details
about Departments/Modules) in the first academic year have been deleted from the
second, returning two lists Added and Deleted.

The arguments have exactly the same meaning as with book_details_year/3.

e.g. 

		?- compare_years_univ_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
		L1 = [],
		L2 = ['ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["7293"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"]].


This means that no University has been deleted and one has been added (with the specific Department and Module).
Notice that each Module is represented by the complex term ModuleCode-ModuleName.

- (8) When you want to retrieve (in lists) the statistics for a book for two
academic years, i.e. which Departments (including details about University/
Modules) have been added in the second academic year compared to the first and
which Departments (including details about University/Modules) in the first
academic year have been deleted from the second:


		compare_years_dept_list(+BooksEvdoxusIDs, +AcademicYear1, +AcademicYear2, -Deleted, -Added, +Options).

Provides comparative statistics for multiple books (aggregatively) between two
(not necessarily consecutive) academic years, about which Departments
(including details about University/Modules) have been added in the second
academic year compared to the first and which Departments (including details
about University/Modules) in the first academic year have been deleted from the
second, returning two lists Added and Deleted.

The arguments have exactly the same meaning as with book_details_year/3.

e.g. 

		?- compare_years_dept_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
		L1 = [],
		L2 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΜΑΘΗΜΑΤΙΚΩΝ'-1/["12661"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["7293"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΙΓΑΙΟΥ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΙΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/["8553"-"Τεχνητή Νοημοσύνη"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΕΛΟΠΟΝΝΗΣΟΥ'-'ΦΥΣΙΚΟΘΕΡΑΠΕΙΑΣ'-1/["ΦΥΣΕΠ9"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΝΕΩΝ ΤΕΧΝΟΛΟΓΙΩΝ"]].

This means that no Department has been deleted and 4 have been added (from the
specific Universities and with the specific Modules).

- (9) When you want to retrieve (in lists) the statistics for a book for two
academic years, i.e. which Modules (including details about University/
Department) have been added in the second academic year compared to the first and
which Modules (including details about University/Department) in the first
academic year have been deleted from the second:


		compare_years_module_list(+BooksEvdoxusIDs, +AcademicYear1, +AcademicYear2, -Deleted, -Added, +Options).

Provides comparative statistics for multiple books (aggregatively) between two
(not necessarily consecutive) academic years, about which Modules
(including details about University/Department) have been added in the second
academic year compared to the first and which Modules (including details
about University/Department) in the first academic year have been deleted from the
second, returning two lists Added and Deleted.

The arguments have exactly the same meaning as with book_details_year/3.

e.g. 

		?- compare_years_module_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
		L1 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΟΙΚΗΣΗΣ ΕΠΙΧΕΙΡΗΣΕΩΝ ΚΑΙ ΟΡΓΑΝΙΣΜΩΝ'-1/["44I414"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ Τ.Ε. (ΠΡΩΗΝ ΤΕΙ ΣΤΕΡΕΑΣ ΕΛΛΑΔΑΣ- ΜΕΤΑΒΑΤΙΚΗ ΛΕΙΤΟΥΡΓΙΑ)'-1/["ΕΕ4141Τ"-"Τεχνητή Νοημοσύνη"], 
			  'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΠΛΗΡΟΦΟΡΙΚΗΣ ΚΑΙ ΤΗΛΕΠΙΚΟΙΝΩΝΙΩΝ'-1/["15100481"-"ΗΛΕΚΤΡΟΜΑΓΝΗΤΙΣΜΟΣ-ΟΠΤΙΚΗ-ΣΥΓΧΡΟΝΗ ΦΥΣΙΚΗ"], 
			  'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΗΛΕΚΤΡΟΝΙΚΩΝ ΜΗΧΑΝΙΚΩΝ'-2/["ΤΛΝ7102"-"Ευφυή Συστήματα Ελέγχου (Θ)", "ΤΛΝ712"-"Ευφυή Συστήματα Ελέγχου (Θ & Ε)"], 
			  'ΟΙΚΟΝΟΜΙΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΠΛΗΡΟΦΟΡΙΚΗΣ'-1/["92-3531"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΙΓΑΙΟΥ'-'ΜΗΧΑΝΙΚΩΝ ΠΛΗΡΟΦΟΡΙΑΚΩΝ ΚΑΙ ΕΠΙΚΟΙΝΩΝΙΑΚΩΝ ΣΥΣΤΗΜΑΤΩΝ'-1/[... - ...], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΔΥΤΙΚΗΣ ΜΑΚΕΔΟΝΙΑΣ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΪΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/[...], 
			  ... - ... - ... / ..., ... - ...|...],
		L2 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΜΑΘΗΜΑΤΙΚΩΝ'-1/["12661"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
		      'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["7293"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΙΓΑΙΟΥ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΙΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/["8553"-"Τεχνητή Νοημοσύνη"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΕΛΟΠΟΝΝΗΣΟΥ'-'ΦΥΣΙΚΟΘΕΡΑΠΕΙΑΣ'-1/["ΦΥΣΕΠ9"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΝΕΩΝ ΤΕΧΝΟΛΟΓΙΩΝ"], 
			  'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΟΙΚΗΣΗΣ ΕΠΙΧΕΙΡΗΣΕΩΝ ΚΑΙ ΟΡΓΑΝΙΣΜΩΝ'-1/["44I409"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΔΙΟΙΚΗΣΗ"], 
			  'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ Τ.Ε. (ΠΡΩΗΝ ΤΕΙ ΣΤΕΡΕΑΣ ΕΛΛΑΔΑΣ- ΜΕΤΑΒΑΤΙΚΗ ΛΕΙΤΟΥΡΓΙΑ)'-1/[... - ...], 
			  'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/[...], 
			  ... - ... - ... / ..., ... - ...|...].

This means that 9 Modules have been deleted and 9 have been added (from the specific Universities and Departments).

- (10) When you want to compare details and statistics for multiple books for a specific academic year:


		compare_books(+ListofListsOfBookIDs,+AcademicYear,+Options).
	
ListofListsOfBookIDs is a list of lists. Each inner list is a list of Book IDs 
whose details and statistics are aggregated (as in e.g. book_details_year/3).
The rest of the arguments are exactly as in book_details_year/3.
The purpose of this predicate is to compare the "performance" of various books (or book groups) 
against each other, much the same way as in running multiple times the book_details_year/3 predicate.
The results are shown on-screen, unless options silent is used, and stored at a file with name 
'mult-results-<<AcademicYear>>-<<ListofListsOfBookIDs>>.txt'.

e.g. 

		?- compare_books([['94700120'],['102070469','13909']],2022-2023,[cache]).

(The filename that the results are stored is 'mult-results-2022-2023-94700120-102070469-13909.txt')

- (11) When you want to compare statistics for multiple books for a specific academic year:


		compare_books_stats(+ListofListsOfBookIDs,+AcademicYear,-Stats,+Options).
	 
The meaning of all arguments is exactly as in compare_books/3, except for Stats,
which is the variable where the result (a list) of the aggregative statistics of all book groups 
is returned. Each member of the Stats list has the format NumberOfUniversities-NumberOfDepartments-NumberOfModules.

e.g.

		?- compare_books_stats([['94700120'],['102070469','13909']],2022-2023,S,[cache]).
		S = [21-58-83, 19-56-82].

- (12) When you want to find statistics and details about which books are unique at Universities / Departments and Modules, compared to each other.

		compare_books_diff(+ListofListsOfBookIDs,+AcademicYear,+Options).

The meaning of all arguments is exactly as in compare_books/3. The ListofListsOfBookIDs should include only 2 book groups.
The purpose of this predicate is to benchmark the "performance" of 2 books (or book groups) 
against each other, i.e. to find out which Universities / Departments / Modules use exclusively only the one or the other book.
The results are shown on-screen, unless options silent is used, and stored at a file with name 
'mult-diff-results-<<AcademicYear>>-<<ListofListsOfBookIDs>>.txt'.

e.g.

		?- compare_books_diff([['94700120'],['102070469','13909']],2022-2023,[cache]).

(The filename that the results are stored is 'mult-diff-results-2022-2023-94700120-102070469-13909.txt')

- (13) When you want to retrieve (in lists) the comparison statistics for 2 books, 
i.e. which Universities (including details about Departments/Modules) use exclusively only the one or the other book.

		compare_books_univ_list(+ListofListsOfBookIDs,+AcademicYear, -U1, -U2, +Options).

Provides comparative statistics for 2 book groups, about which Universities
(including details about Departments/Modules) use exclusively only the one or the other book, 
returning two lists U1 and U2.

The meaning of all arguments is exactly as in compare_books/3. The ListofListsOfBookIDs should include only 2 book groups.

e.g. 

		?- compare_books_univ_list([['94700120'],['102070469','13909']],2022-2023,U1,U2,[cache]).
		U1 = ['ΓΕΩΠΟΝΙΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΑΞΙΟΠΟΙΗΣΗΣ ΦΥΣΙΚΩΝ ΠΟΡΩΝ ΚΑΙ ΓΕΩΡΓΙΚΗΣ ΜΗΧΑΝΙΚΗΣ'-1/["259"-"ΤΕΧΝΙΚΕΣ ΒΕΛΤΙΣΤΟΠΟΙΗΣΗΣ ΚΑΙ ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ ΣΤΗ ΓΕΩΡΓΙΑ"], 
		      'ΔΗΜΟΚΡΙΤΕΙΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΡΑΚΗΣ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["5Η9"-"ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ"]],
		U2 = [].

This means that the first book is exclusively used (compared to the second one) in 2 Universities, 
whereas the second book group is not exclusively used in any Univesity (compared to the first book).

- (14) When you want to retrieve (in lists) the comparison statistics for 2 books, 
i.e. which Departments (including details about Universities/Modules) use exclusively only the one or the other book.

		compare_books_dept_list(+ListofListsOfBookIDs,+AcademicYear, -D1, -D2, +Options).

Provides comparative statistics for 2 book groups, about which Departments
(including details about Universities/Modules) use exclusively only the one or the other book, 
returning two lists D1 and D2.

The meaning of all arguments is exactly as in compare_books/3. The ListofListsOfBookIDs should include only 2 book groups.

e.g. 

		?- compare_books_dept_list([['94700120'],['102070469','13909']],2022-2023,D1,D2,[cache]).
		D1 = ['ΓΕΩΠΟΝΙΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΑΞΙΟΠΟΙΗΣΗΣ ΦΥΣΙΚΩΝ ΠΟΡΩΝ ΚΑΙ ΓΕΩΡΓΙΚΗΣ ΜΗΧΑΝΙΚΗΣ'-1/["259"-"ΤΕΧΝΙΚΕΣ ΒΕΛΤΙΣΤΟΠΟΙΗΣΗΣ ΚΑΙ ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ ΣΤΗ ΓΕΩΡΓΙΑ"], 
		      'ΔΗΜΟΚΡΙΤΕΙΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΡΑΚΗΣ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["5Η9"-"ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΜΗΧΑΝΙΚΩΝ ΠΑΡΑΓΩΓΗΣ ΚΑΙ ΔΙΟΙΚΗΣΗΣ'-2/["1624-190409"-"ΔΙΑΧΕΙΡΙΣΗ ΑΞΙΟΠΙΣΤΙΑΣ ΣΤΟ ΔΙΑΔΙΚΤΥΟ ΤΩΝ ΠΡΑΓΜΑΤΩΝ", "1624-190816"-"ΣΥΣΤΗΜΑΤΑ ΔΙΑΧΕΙΡΙΣΗΣ ΓΝΩΣΗΣ"], 
			  'ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΜΗΧΑΝΙΚΩΝ ΠΛΗΡΟΦΟΡΙΚΗΣ ΚΑΙ ΗΛΕΚΤΡΟΝΙΚΩΝ ΣΥΣΤΗΜΑΤΩΝ'-2/["1625-1601"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ", "1625-1945"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["7293"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"], 
			  'ΙΟΝΙΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΨΗΦΙΑΚΩΝ ΜΕΣΩΝ ΚΑΙ ΕΠΙΚΟΙΝΩΝΙΑΣ'-1/[... - ...], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΔΥΤΙΚΗΣ ΑΤΤΙΚΗΣ'-'ΜΗΧΑΝΙΚΩΝ ΒΙΟΜΗΧΑΝΙΚΗΣ ΣΧΕΔΙΑΣΗΣ ΚΑΙ ΠΑΡΑΓΩΓΗΣ'-2/[...|...], 
			  ... - ... - ... / ..., ... - ...|...],
		D2 = ['ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΔΙΟΙΚΗΣΗΣ ΕΦΟΔΙΑΣΤΙΚΗΣ ΑΛΥΣΙΔΑΣ'-1/["HE2"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΕΦΟΔΙΑΣΤΙΚΗΣ ΑΛΥΣΙΔΑΣ"], 
		      'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΑΧΕΙΡΙΣΗΣ ΛΙΜΕΝΩΝ ΚΑΙ ΝΑΥΤΙΛΙΑΣ'-1/["45153"-"ΚΑΙΝΟΤΟΜΙΕΣ ΣΤΗ ΝΑΥΤΙΛΙΑ ΚΑΙ ΤΟΥΣ ΛΙΜΕΝΕΣ"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΕΦΑΡΜΟΣΜΕΝΩΝ ΜΑΘΗΜΑΤΙΚΩΝ ΚΑΙ ΦΥΣΙΚΩΝ ΕΠΙΣΤΗΜΩΝ'-1/["9549"-"Τεχνητή Νοημοσύνη"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["3287"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΜΗΧΑΝΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ'-1/["0813.9.013.0"-"Μάθηση Μηχανών - Τεχνητή Νοημοσύνη"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΕΣΣΑΛΙΑΣ'-'ΠΡΟΓΡΑΜΜΑ ΣΠΟΥΔΩΝ ΜΗΧΑΝΙΚΩΝ ΠΛΗΡΟΦΟΡΙΚΗΣ Τ.Ε. (ΛΑΜΙΑ)'-2/[... - ...|...], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΑΤΡΩΝ'-'ΕΠΙΣΤΗΜΗΣ ΚΑΙ ΤΕΧΝΟΛΟΓΙΑΣ ΤΡΟΦΙΜΩΝ'-1/[...], 
			  ... - ... - ... / ...].


This means that the first book is exclusively used (compared to the second one) in 10 Universities, 
whereas the second book group is exclusively used (compared to the first one) in 8 Universities.

- (15) When you want to retrieve (in lists) the comparison statistics for 2 books, 
i.e. which Modules (including details about Universities/Departments) use exclusively only the one or the other book.

		compare_books_module_list(+ListofListsOfBookIDs,+AcademicYear, -M1, -M2, +Options).

Provides comparative statistics for 2 book groups, about which Modules
(including details about Universities/Departments) use exclusively only the one or the other book, 
returning two lists M1 and M2.

The meaning of all arguments is exactly as in compare_books/3. The ListofListsOfBookIDs should include only 2 book groups.

e.g. 

		?- compare_books_module_list([['94700120'],['102070469','13909']],2022-2023,M1,M2,[cache]).
		M1 = ['ΓΕΩΠΟΝΙΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΑΞΙΟΠΟΙΗΣΗΣ ΦΥΣΙΚΩΝ ΠΟΡΩΝ ΚΑΙ ΓΕΩΡΓΙΚΗΣ ΜΗΧΑΝΙΚΗΣ'-1/["259"-"ΤΕΧΝΙΚΕΣ ΒΕΛΤΙΣΤΟΠΟΙΗΣΗΣ ΚΑΙ ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ ΣΤΗ ΓΕΩΡΓΙΑ"], 
		      'ΔΗΜΟΚΡΙΤΕΙΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΡΑΚΗΣ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["5Η9"-"ΥΠΟΛΟΓΙΣΤΙΚΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΜΗΧΑΝΙΚΩΝ ΠΑΡΑΓΩΓΗΣ ΚΑΙ ΔΙΟΙΚΗΣΗΣ'-2/["1624-190409"-"ΔΙΑΧΕΙΡΙΣΗ ΑΞΙΟΠΙΣΤΙΑΣ ΣΤΟ ΔΙΑΔΙΚΤΥΟ ΤΩΝ ΠΡΑΓΜΑΤΩΝ", "1624-190816"-"ΣΥΣΤΗΜΑΤΑ ΔΙΑΧΕΙΡΙΣΗΣ ΓΝΩΣΗΣ"], 
			  'ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΜΗΧΑΝΙΚΩΝ ΠΛΗΡΟΦΟΡΙΚΗΣ ΚΑΙ ΗΛΕΚΤΡΟΝΙΚΩΝ ΣΥΣΤΗΜΑΤΩΝ'-2/["1625-1601"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ", "1625-1945"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["7293"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"], 
			  'ΙΟΝΙΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΨΗΦΙΑΚΩΝ ΜΕΣΩΝ ΚΑΙ ΕΠΙΚΟΙΝΩΝΙΑΣ'-1/[... - ...], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΔΥΤΙΚΗΣ ΑΤΤΙΚΗΣ'-'ΜΗΧΑΝΙΚΩΝ ΒΙΟΜΗΧΑΝΙΚΗΣ ΣΧΕΔΙΑΣΗΣ ΚΑΙ ΠΑΡΑΓΩΓΗΣ'-2/[...|...], 
			  ... - ... - ... / ..., ... - ...|...],
		M2 = ['ΔΙΕΘΝΕΣ ΠΑΝΕΠΙΣΤΗΜΙΟ ΤΗΣ ΕΛΛΑΔΟΣ'-'ΔΙΟΙΚΗΣΗΣ ΕΦΟΔΙΑΣΤΙΚΗΣ ΑΛΥΣΙΔΑΣ'-1/["HE2"-"ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΕΦΟΔΙΑΣΤΙΚΗΣ ΑΛΥΣΙΔΑΣ"], 
		      'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΑΧΕΙΡΙΣΗΣ ΛΙΜΕΝΩΝ ΚΑΙ ΝΑΥΤΙΛΙΑΣ'-1/["45153"-"ΚΑΙΝΟΤΟΜΙΕΣ ΣΤΗ ΝΑΥΤΙΛΙΑ ΚΑΙ ΤΟΥΣ ΛΙΜΕΝΕΣ"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΕΦΑΡΜΟΣΜΕΝΩΝ ΜΑΘΗΜΑΤΙΚΩΝ ΚΑΙ ΦΥΣΙΚΩΝ ΕΠΙΣΤΗΜΩΝ'-1/["9549"-"Τεχνητή Νοημοσύνη"], 
			  'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["3287"-"ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
			  'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΜΗΧΑΝΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ'-1/["0813.9.013.0"-"Μάθηση Μηχανών - Τεχνητή Νοημοσύνη"], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΕΣΣΑΛΙΑΣ'-'ΠΡΟΓΡΑΜΜΑ ΣΠΟΥΔΩΝ ΜΗΧΑΝΙΚΩΝ ΠΛΗΡΟΦΟΡΙΚΗΣ Τ.Ε. (ΛΑΜΙΑ)'-2/[... - ...|...], 
			  'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΑΤΡΩΝ'-'ΕΠΙΣΤΗΜΗΣ ΚΑΙ ΤΕΧΝΟΛΟΓΙΑΣ ΤΡΟΦΙΜΩΝ'-1/[...], 
			  ... - ... - ... / ..., ... - ...|...].


This means that the first book is exclusively used (compared to the second one) in 20 Modules, 
whereas the second book group is exclusively used (compared to the first one) in 19 Modules.
