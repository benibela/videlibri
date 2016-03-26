VideLibri
=============
VideLibri is an app to access the web catalogs/OPACs of libraries.
It has all the usual features of their OPACs, e.g. viewing your account, searching books or ordering other items. It also renews all due books automatically and can do other things only a locally running app can do, like keeping a history of all ever lend books. It probably was the first library app ever made.

So far VideLibri has been tested with 200 libraries successfully.
It is platform-independent and currently [provides binaries](http://www.videlibri.de) for (Desktop) Windows, Linux and Android. At the moment its GUI is entirely in German as no support for any non-German-speaking library has been requested, but a translation can be made if wished for.

Backend
-------------

VideLibri was designed to be self-learning, so that it can learn the structure of any website automatically as far as feasible. This should ensure that VideLibri can be used with every possible library system, all existing libraries or yet-to-be-founded libraries.

Towards this goal VideLibri implements several different query languages that are supposed to simplify the interaction with arbitrary webpages as much as possible:

- A pattern-matching "template" that selects arbitrary data from a single HTML page and can be automatically generated from an annotated sample of that page. (annotations are required, since fully autonomous learning would require a vast amount of test accounts and different search terms, and most users cannot get access to hundreds of library accounts)
- A catalog of related pages to apply these patterns to multiple webpages. Its syntax is similar to XSLT and likewise it is almost Turing complete (i.e. it has the necessary control structures, but requires XPath to do calculations ). 
- A dialect of XPath/XQuery/JSONiq that is Turing-complete and thus can calculate arbitrary, unexpected things, e.g. emulating JavaScript-only pages.
- CSS 3 Selectors for trivial selection task

The source of these interpreters has been moved to a separate repository ( internettools ) for clarity.

A spin-off command line tool ( see repository xidel ) has been developed to let you use these languages for tasks unrelated to libraries. 