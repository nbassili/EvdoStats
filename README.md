# EvdoStats
A Prolog application for extracting statistics about books from the Evdoxus site

- When you run the Program for the first time, you should run
?- init.

This initializes the cache, downloading all course pages.

- Each year, you should run 
?- new_year.

This incrementally caches the course pages of the new academic year.

- When you want to see details and statistics for a book for a specific academic year:

find_book(+BooksEvdoxusIDs,+AcademicYear,+Options).

Provides statistics for a single or multiple books, i.e. at which Universities, Departments, Modules this book is distributed through Evdoxus,
on-screen (optional) and stored at the file 'results.txt'.

You provide either one Book ID in Evdoxus (no list), or a list of Books IDs.
For a list of Books, statistics are aggregated for all books.
AcademicYear is in the form e.g. 2022-2023.
Options is a list (possibly empty) containing the following:
   - cache: read course pages from cache instead from the Web (recommended)
   - silent: do not print results on-screen (only at file)

e.g. ?- find_book('94700120',2022-2023,[cache,silent]).
or   ?- find_book(['94700120'],2022-2023,[cache,silent]).

?- find_book(BooksEvdoxusIDs,AcademicYear).

This is equivalent to find_book(BooksEvdoxusIDs,AcademicYear,[]).

- When you want to find out only the statistics for a book for a specific academic year:

book_stats_year(+BooksEvdoxusIDs,+AcademicYear,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books aggregatively for an academic year, in the form:

NoOfUniversities - NoOfDepartments - NoOfModules

The rest of the arguments have exactly the same meaning as with find_book/3.

e.g. 
?- book_stats_year('94700120',2022-2023,Stats,[cache]).
Stats = 21-58-87.

- When you want to find out the statistics for a book for several academic years:

book_stats_year(+BooksEvdoxusIDs,+Year1,+Year2,-Stats,+Options).

Returns at Stats argument the statistics about a book or a list of books aggregatively for all the academic years between Year1 and Year2, 
in a list of the form:

[AcademicYear1/NoOfUniversities1-NoOfDepartments1-NoOfModules1, ..., AcademicYearN/NoOfUniversitiesN-NoOfDepartmentsN-NoOfModulesN]

Year1 and Year2 are integers (e.g. 2020, 2023, ...). 
The rest of the arguments have exactly the same meaning as with find_book/3.

e.g.
?- book_stats(['94700120','12867416'],2019,2023,Stats,[cache]).
Stats = [2019-2020/(20-42-64), 2020-2021/(21-49-77), 2021-2022/(20-54-87), 2022-2023/(21-58-87)].

- When you want to compare the the statistics for a book for two academic years, i.e. which Universities/Departments/Modules have been added in the second academic year compared to the first and which Universities/Departments/Modules in the first academic year have been deleted from the second:

compare_years(+BooksEvdoxusIDs,+AcademicYear1, +AcademicYear2, +Options)

Provides comparative details and statistics for a single or multiple books (aggregatively) between two (not necessarily consecutive) academic years, i.e. which Universities/Departments/Modules have been added in the second academic year compared to the first and which Universities/Departments/Modules in the first academic year have been deleted from the second, on-screen (optional) and stored at the file 'comp-results.txt'.

The arguments have exactly the same meaning as with find_book/3.

e.g. 
?- compare_years('94700120',2021-2022,2022-2023,[cache]).
