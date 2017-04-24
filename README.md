VideLibri
=============
VideLibri likely was the world's first app to access the web catalogs/OPACs of libraries as it has been developed continuously since 2006.
It has all the usual features of their OPACs, e.g. viewing your account, searching books or ordering other items. It also renews all due books automatically and can do other things only a locally running app can do, like keeping a history of all ever lend books. 

So far VideLibri has been tested with 200 libraries successfully.
It is platform-independent and currently [provides binaries](http://www.videlibri.de) for (Desktop) Windows, Linux and Android. At the moment its GUI is entirely in German as no support for any non-German-speaking library has been requested, but a translation can be made if wished for.

![on Windows](http://sourceforge.net/dbimage.php?id=280463) ![on Android](http://sourceforge.net/p/videlibri/screenshot/android.png)

Backend
-------------

VideLibri was designed to be self-learning, so that it can learn the structure of any website automatically as far as feasible. This should ensure that VideLibri can be used with every possible library system, all existing libraries or yet-to-be-founded libraries.

Towards this goal VideLibri implements several different query languages that are supposed to simplify the interaction with arbitrary webpages as much as possible:

- A pattern-matching "template" that selects arbitrary data from a single HTML page and can be automatically generated from an annotated sample of that page. (annotations are required, since fully autonomous learning would require a vast amount of test accounts and different search terms, and most users cannot get access to hundreds of library accounts)
- A catalog of related pages to apply these patterns to multiple webpages. Its syntax is similar to XSLT and likewise it is almost Turing complete (i.e. it has the necessary control structures, but requires XPath to do calculations ). 
- A dialect of XPath/XQuery/JSONiq that is Turing-complete and thus can calculate arbitrary, unexpected things, e.g. emulating JavaScript-only pages.
- CSS 3 Selectors for trivial selection task

It cannot be emphasized enough that these are not programming languages for developers, rather they are query languages simple enough that any end user can use them. Thus you can also enter XQuery statements directly in the GUI of VideLibri to run queries over the sequence of lend `$books` to answer important questions like "How many books of author X have I lend?" or "Which book have I lend the most often, of all the books that have a title whose length is divisible by 7?"

The source of these interpreters has been moved to a separate repository ( internettools ) for clarity.

A spin-off command line tool ( see repository xidel ) has been developed to let you use these languages for tasks unrelated to libraries.  

Contributing
-------------
The biggest contribution are reports about changes on the account page of the catalog (e.g. screenshots or downloads of the webpage) or directly library accounts for testing purposes. It has happened many times that VideLibri had worked perfectly with a library for years, but then they changed something in their OPAC and VideLibri did not work anymore. A user wrote "Last week it stopped working" anonymously, the library refused to reply to mails, I could not see a change hidden behind the login-wall, so the issue could never be fixed. Even if the fix was trivial given information about the change.

You do not need to contribute to add a new library, just like you do not need to contribute to Firefox to visit a webpage. See the backend section above, or [this (German) tutorial](http://www.videlibri.de/help/neuebibliothek.html). 

License
-------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.