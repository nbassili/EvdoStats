               EvdoStats: EVDOXUS book statistics in Prolog
A Prolog application for exporting statistics about book from the Evdoxus site
==============================================================================

(1) When you run the Program for the first time, you should run

	init.

This initializes the cache, downloading all course pages.

(2) Each year, you should run 

	new_year.

This incrementally caches the course pages of the new academic year.

(3) When you want to see details and statistics for a book for a specific academic year:

	find_book(+BooksEvdoxusIDs,+AcademicYear,+Options).

Provides statistics for a single or multiple books, i.e. at which Universities,
Departments, Modules this book is distributed through Evdoxus, on-screen
(optional) and stored at a file with name 'results-<<AcademicYear>>-<<BooksEvdoxusIDs>>.txt'.

You provide either one Book ID in Evdoxus (no list), or a list of Books IDs.
For a list of Books, statistics are aggregated for all books.
AcademicYear is in the form e.g. 2022-2023.
Options is a list (possibly empty) containing the following:
   - cache: read course pages from cache instead from the Web (recommended)
   - silent: do not print results on-screen (only at file)

e.g. 

	?- find_book('94700120',2022-2023,[cache,silent]).

or   

	?- find_book(['94700120'],2022-2023,[cache,silent]).

(The filename that results are stored is 'results-2022-2023-94700120.txt').

	find_book(BooksEvdoxusIDs,AcademicYear).

This is equivalent to find_book(BooksEvdoxusIDs,AcademicYear,[]).

(4) When you want to find out only the statistics for a book for a specific academic year:

	book_stats_year(+BooksEvdoxusIDs,+AcademicYear,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books
aggregatively for an academic year, in the form: NoOfUniversities - NoOfDepartments - NoOfModules

The rest of the arguments have exactly the same meaning as with find_book/3.

e.g. 

	?- book_stats_year('94700120',2022-2023,Stats,[cache]).
	Stats = 21-58-87.

(5) When you want to find out the statistics for a book for several academic years:

	book_stats(+BooksEvdoxusIDs,+Year1,+Year2,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books
aggregatively for all the academic years between Year1 and Year2, in a list of
the form:

[AcademicYear1/NoOfUniversities1-NoOfDepartments1-NoOfModules1, ..., AcademicYearN/NoOfUniversitiesN-NoOfDepartmentsN-NoOfModulesN]

Year1 and Year2 are integers (e.g. 2020, 2023, ...). 
The rest of the arguments have exactly the same meaning as with find_book/3.

e.g.

	?- book_stats(['94700120','12867416'],2019,2023,Stats,[cache]).
	Stats = [2019-2020/(20-42-64), 2020-2021/(21-49-77), 2021-2022/(20-54-87), 2022-2023/(21-58-87)].

(6) When you want to compare the statistics for a book for two academic years,
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

The arguments have exactly the same meaning as with find_book/3.

e.g. 

	?- compare_years('94700120',2021-2022,2022-2023,[cache]).

(The filename that the results are stored is 'comp-results-2021-2022-2022-2023-94700120.txt')

(7) When you want to retrieve (in lists) the statistics for a book for two
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

The arguments have exactly the same meaning as with find_book/3.

e.g. 

	?- compare_years_univ_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
	L1 = [],
	L2 = ['ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/
	["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ "]].

This means that no University has been deleted and one has been added (with the
specific Department and Module).

(8) When you want to retrieve (in lists) the statistics for a book for two
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

The arguments have exactly the same meaning as with find_book/3.

e.g. 

	?- compare_years_dept_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
	L1 = [],
	L2 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΜΑΘΗΜΑΤΙΚΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
	'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ "],
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΙΓΑΙΟΥ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΙΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/["Τεχνητή Νοημοσύνη"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΕΛΟΠΟΝΝΗΣΟΥ'-'ΦΥΣΙΚΟΘΕΡΑΠΕΙΑΣ'-1/["ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΝΕΩΝ ΤΕΧΝΟΛΟΓΙΩΝ"]].

This means that no Department has been deleted and 4 have been added (from the
specific Universities and with the specific Modules).

(9) When you want to retrieve (in lists) the statistics for a book for two
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

The arguments have exactly the same meaning as with find_book/3.

e.g. 

	?- compare_years_module_list(['94700120'],2021-2022,2022-2023,L1,L2,[cache]).
	L1 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΟΙΚΗΣΗΣ ΕΠΙΧΕΙΡΗΣΕΩΝ ΚΑΙ ΟΡΓΑΝΙΣΜΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
	'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΠΛΗΡΟΦΟΡΙΚΗΣ ΚΑΙ ΤΗΛΕΠΙΚΟΙΝΩΝΙΩΝ'-1/["ΗΛΕΚΤΡΟΜΑΓΝΗΤΙΣΜΟΣ-ΟΠΤΙΚΗ-ΣΥΓΧΡΟΝΗ ΦΥΣΙΚΗ"], 
	'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΗΛΕΚΤΡΟΝΙΚΩΝ ΜΗΧΑΝΙΚΩΝ'-2/["Ευφυή Συστήματα Ελέγχου (Θ)", "Ευφυή Συστήματα Ελέγχου (Θ & Ε)"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΔΥΤΙΚΗΣ ΜΑΚΕΔΟΝΙΑΣ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΪΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/["ΕΙΣΑΓΩΓΗ ΣΤΗΝ ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ(ΔΗΛΩΝΕΤΑΙ ΜΟΝΟ ΑΠΟ ΦΟΙΤΗΤΕΣ ΤΟΥ ΤΕΙ- ΒΣ)"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΘΕΣΣΑΛΙΑΣ'-'ΨΗΦΙΑΚΩΝ ΣΥΣΤΗΜΑΤΩΝ'-1/["Τεχνητή Νοημοσύνη και Έμπειρα Συστήματα"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΜΑΚΕΔΟΝΙΑΣ'-'ΕΦΑΡΜΟΣΜΕΝΗΣ ΠΛΗΡΟΦΟΡΙΚΗΣ'-1/["ΗΘΙΚΗ ΚΑΙ ΔΙΑΚΥΒΕΡΝΗΣΗ ΤΕΧΝΗΤΗΣ ΝΟΗΜΟΣΥΝΗΣ"], 
	'ΠΟΛΥΤΕΧΝΕΙΟ ΚΡΗΤΗΣ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/[...], ... - ... - ... / ...],
	L2 = ['ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΜΑΘΗΜΑΤΙΚΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ"], 
	'ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ'-'ΜΗΧΑΝΙΚΩΝ ΜΕΤΑΛΛΕΙΩΝ ΜΕΤΑΛΛΟΥΡΓΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΑΛΓΟΡΙΘΜΟΙ ΜΗΧΑΝΙΚΗΣ ΕΚΜΑΘΗΣΗΣ"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΙΓΑΙΟΥ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΙΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/["Τεχνητή Νοημοσύνη"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΠΕΛΟΠΟΝΝΗΣΟΥ'-'ΦΥΣΙΚΟΘΕΡΑΠΕΙΑΣ'-1/["ΕΥΦΥΗ ΣΥΣΤΗΜΑΤΑ ΝΕΩΝ ΤΕΧΝΟΛΟΓΙΩΝ"], 
	'ΕΘΝΙΚΟ & ΚΑΠΟΔΙΣΤΡΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ ΑΘΗΝΩΝ'-'ΔΙΟΙΚΗΣΗΣ ΕΠΙΧΕΙΡΗΣΕΩΝ ΚΑΙ ΟΡΓΑΝΙΣΜΩΝ'-1/["ΤΕΧΝΗΤΗ ΝΟΗΜΟΣΥΝΗ ΚΑΙ ΔΙΟΙΚΗΣΗ"], 
	'ΕΛΛΗΝΙΚΟ ΜΕΣΟΓΕΙΑΚΟ ΠΑΝΕΠΙΣΤΗΜΙΟ'-'ΗΛΕΚΤΡΟΛΟΓΩΝ ΜΗΧΑΝΙΚΩΝ ΚΑΙ ΜΗΧΑΝΙΚΩΝ ΥΠΟΛΟΓΙΣΤΩΝ'-1/["Μηχανική Μάθηση και Εξόρυξη Γνώσης"], 
	'ΠΑΝΕΠΙΣΤΗΜΙΟ ΔΥΤΙΚΗΣ ΜΑΚΕΔΟΝΙΑΣ'-'ΜΗΧΑΝΙΚΩΝ ΣΧΕΔΙΑΣΗΣ ΠΡΟΪΟΝΤΩΝ ΚΑΙ ΣΥΣΤΗΜΑΤΩΝ'-1/[...], ... - ... - ... / ..., ... - ...].

This means that 9 Modules have been deleted and 9 have been added (from the specific Universities and Departments).

(10) When you want to compare details and statistics for multiple books for a specific academic year:

	compare_books(+ListofListsOfBookIDs,+AcademicYear,+Options).
	
ListofListsOfBookIDs is a list of lists. Each inner list is a list of Book IDs 
whose details and statistics are aggregated (as in e.g. find_book/3).
The rest of the arguments are exactly as in find_book/3.
The purpose of this predicate is to compare the "performance" of various books (or book groups) 
against each other, much the same way as in running multiple times the find_book/3 predicate.
The results are shown on-screen, unless options silent is used, and stored at a file with name 
'mult-results-<<AcademicYear>>-<<ListofListsOfBookIDs>>.txt'.

e.g. 

	?- compare_books([['94700120'],['102070469','13909']],2022-2023,[cache]).

(The filename that the results are stored is 'mult-results-2022-2023-94700120-102070469-13909.txt')

(11) When you want to compare statistics for multiple books for a specific academic year:

	 compare_books_stats(+ListofListsOfBookIDs,+AcademicYear,-Stats,+Options).
	 
The meaning of all arguments is exactly as in compare_books/3, except for Stats,
which is the variable where the result (a list) of the aggregative statistics of all book groups 
is returned. Each member of the Stats list has the format NumberOfUniversities-NumberOfDepartments-NumberOfModules.

e.g.

	?- compare_books_stats([['94700120'],['102070469','13909']],2022-2023,S,[cache]).
	S = [21-58-87, 19-61-89].

