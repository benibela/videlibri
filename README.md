VideLibri :books:
=============
VideLibri likely was the world's first app to access the web catalogs/OPACs of libraries as it has been developed continuously since 2006.
It has all the usual features of their OPACs, e.g. viewing your account, searching books or ordering other items. It also renews all due books automatically and can do other things only a locally running app can do without privacy concerns, like keeping a history of all ever lend books to track your own reading habits.  With account viewing and renewing for the first public libraries completed in 2006 it is world's first universal library app.

So far VideLibri has been tested with 200 libraries successfully.
It is platform-independent and currently [provides binaries](http://www.videlibri.de) for (Desktop) Windows, Linux and Android. At the moment its GUI is entirely in German as no support for any non-German-speaking library has been requested, but a translation can be made if asked for.

![on Windows](http://sourceforge.net/dbimage.php?id=280463) ![on Android](http://sourceforge.net/p/videlibri/screenshot/android.png)

Backend
-------------

Out-of-the-box VideLibri supports the following library catalog systems, OPACs, and open standard APIs:

* aDIS/BMS
* Aleph  (with 4 separate implementations for 4 different libraries as they were incompatible to each other)
* Bibliotheca 
* Bibliotheca+/OPEN 
* Digibib 
* Libero 5 
* Netbiblio 
* PAIA 
* PICA 
    * PICA standard 
    * combined with Bibdia 
    * combined with LBS     
* Primo 
* SISIS-SunRise (including Touchpoint)
* SRU 
* Websphere (WAS) 
* Zones 1.8

When you connect to an previously untested, unknown library in VideLibri, the app will ask for the system and then for the relevant parameters (usually the server URL and, if the system allows multiple OPACs on a single server, the database id ).

However, a finite set of supported systems is not enough to access all libraries as there are libraries using other kinds of OPACs. In fact, they are not even enough to support a single library eternally, since libraries tend to replace their catalog with a new catalog system randomly and unannounced, and then you might not be able to renew your lendings anymore. Therefore it became necessary to develop a webscraping framework that can learn the structure of any OPAC and any webpage semi-automatically and should be simple enough that an end-user can understand it and use it to connect to their own library no matter which catalog system they use. This will ensure that VideLibri can be used with every possible library system, all existing libraries and all yet-to-be-founded libraries.

Towards this goal VideLibri implements several different query languages and DSLs that should simplify the interaction with an arbitrary webpage as much as possible:

- A pattern-matching "template" that selects arbitrary data from a single HTML page and can be automatically generated from an annotated sample of that page. (annotations are required, since fully autonomous learning would require a vast amount of test accounts and different search terms, and most users cannot get access to hundreds of library accounts)
- A catalog of related pages to apply these patterns to multiple webpages. Its syntax is similar to XSLT and likewise it is almost Turing complete (i.e. it has the necessary control structures, but requires XPath to do calculations ). 
- A dialect of XPath/XQuery/JSONiq that is Turing-complete and thus can calculate arbitrary, unexpected things, e.g. emulating JavaScript-only pages or rendering a 3D scene by ray tracing. (see its [test scores on the official XQuery Test Suite of the W3C](http://www.benibela.de/documentation/internettools/xqts.html), e.g. test set app-Demos includes the raytracer)
- CSS 3 Selectors for trivial selection tasks

It cannot be emphasized enough that these are not programming languages for developers, rather they are query languages simple enough that any end user can use them. Thus you can also enter XQuery statements directly in the GUI of VideLibri to run queries over the sequence of lend `$books` to answer important questions like "How many books of author X have I lend?" or "Which book have I lend the most often, of all the books that have a title whose length is divisible by 7?" (see [this (German) tutorial](http://www.videlibri.de/help/xquerysearch.html))

The source of these interpreters has been moved to a separate repository ( internettools ) for clarity.

A spin-off command line tool ( see repository xidel ) has been developed to let you use these languages for tasks unrelated to libraries.  

Contributing
-------------
The biggest contribution are reports about changes on the account page of the catalog (e.g. screenshots or downloads of the webpage) or directly library accounts for testing purposes. It has happened many times that VideLibri had worked perfectly with a library for years, but then they changed something in their OPAC and VideLibri did not work anymore. A user wrote "Last week it stopped working" anonymously, the library refused to reply to mails, I could not see a change hidden behind the login-wall, so the issue could never be fixed. Even if the fix was trivial given information about the change.

You do not need to contribute to VideLibri in order to use it with an untested library, just like you do not need to contribute to Firefox in order to visit a webpage. See the backend section above, or [this (German) tutorial](http://www.videlibri.de/help/neuebibliothek.html). 

The source is structured in individual layers that are and must remain strictly separated from each other. Everything related to individual library systems is stored with the self-learned pattern matching in  data/libraries/templates/. These templates have no access to any part of the GUI. Similarly the Pascal and Java source code does not contain anything related to library systems (except for the glue code passing data between GUI and templates) and the GUI is a dumb view displaying whatever data the templates send. 


VideLibri depends on several libraries that are stored in their own repositories  (see internettools, flre, treelistview, diagram, rcmdline) and that all need to be in the compiler search path, if you want to compile it. To compile the desktop version, you open bookWatch.lpi in Lazarus and click the compile button. To compile the android version, you open videlibriandroid.lpi in Lazarus and click the compile button. Afterwards call `./gradlew assembleDebug`. 

License
-------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.