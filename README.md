VideLibri :books: [![Build Status](https://travis-ci.org/benibela/videlibri.svg?branch=master)](https://travis-ci.org/benibela/videlibri)
=============
VideLibri is a library app with all the usual features of a library OPACs, e.g. viewing your account, searching books or ordering other items. It also renews all due books automatically and can do other things only a locally running app can do without privacy concerns, like keeping a history of all ever lend books to track your own reading habits.  Supporting account viewing and renewing in multiple public library systems in 2006 makes VideLibri the world's first universal library app.

So far VideLibri has been tested with 200 libraries successfully.
It is platform-independent and currently [provides binaries](http://www.videlibri.de) for (Desktop) Windows, Linux and Android. At the moment its GUI is entirely in German as no support for any non-German-speaking library has been requested, but a translation can be made if asked for.

![on Windows](http://sourceforge.net/dbimage.php?id=280463) ![on Android](https://a.fsdn.com/con/app/proj/videlibri/screenshots/s1.png)

Backend
-------------

Out-of-the-box VideLibri supports the following library catalog systems, OPACs, and open standard APIs:

* aDIS/BMS
* Aleph  (mostly deprecated, since barely maintained since 2007)
* Bibliotheca 
* Bibliotheca+/OPEN 
* Digibib 
* Koha
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
* VuFind (only user view)
* Websphere ("WAS") 
* Zones 1.8

When you connect to an previously untested, unknown library in VideLibri, the app will ask for the system and then for the relevant parameters (usually the server URL and, if the system allows multiple OPACs on a single server, the database id ).

However, a finite set of supported systems is not enough to access all libraries as there are libraries using other kinds of OPACs. In fact, they are not even enough to support a single library eternally, since libraries tend to replace their catalog with a new catalog system randomly and unannounced, and then you might not be able to renew your lendings anymore. Therefore in 2007 it became necessary to turn VideLibri into a webscraping framework that can learn the structure of any OPAC and any webpage semi-automatically and should be simple enough that an end-user can understand it and use it to connect to their own library no matter which catalog system they use. This will ensure that VideLibri can be used with every possible library system, all existing libraries and all yet-to-be-founded libraries.

Towards this goal VideLibri implements several different query languages and DSLs that should simplify the interaction with an arbitrary webpage as much as possible:

- A pattern-matching "template" that selects arbitrary data from a single HTML page and can be automatically generated from an annotated sample of that page. (annotations are required, since fully autonomous learning would require a vast amount of test accounts and different search terms, and most users cannot get access to hundreds of library accounts)
- A catalog of related pages to apply these patterns to multiple webpages. Its syntax is similar to XSLT and likewise it is almost Turing complete (i.e. it has the necessary control structures, but requires XPath to do calculations ). 
- A dialect of XPath/XQuery/JSONiq that is Turing-complete and thus can calculate arbitrary, unexpected things, e.g. emulating JavaScript-only pages or rendering a 3D scene by ray tracing. (see its [test scores on the official XQuery Test Suite of the W3C](http://www.benibela.de/documentation/internettools/xqts.html), e.g. test set app-Demos includes the raytracer)
- CSS 3 Selectors for trivial selection tasks

It cannot be emphasized enough that these are not programming languages for developers, rather they are query languages simple enough that any end user can use them. Thus you can also enter XQuery statements directly in the GUI of VideLibri to run queries over the sequence of lend `$books` to answer important questions like "How many books of author X have I lend?" or "Which book have I lend the most often, of all the books that have a title whose length is divisible by 7?" (see [this (German) tutorial](http://www.videlibri.de/help/xquerysearch.html))

The source of these interpreters has been moved to a separate repository ( internettools ) to improve modularization and because some people want to scrape webpages that are not part of an OPAC.

A spin-off command line tool ( see repository xidel ) has been developed to let you use these languages for tasks unrelated to libraries.  

Contributing
-------------

Due to VideLibri being its own interpreter contributing a connection to new libraries is very simple and can be done without any preparation. Most open source projects require one to install a compiler, an IDE, a build system and various other dependencies before you can contribute to them. VideLibri does not need any of this, it is completely self-contained. You can install VideLibri and directly edit the source of the templates (in the directory data/libraries/templates), because the source is there and there is nothing but the source. VideLibri just loads the templates from the text files and evaluates them itself without involving any other software. Although making changes to the GUI or internals like the HTML parser or the interpreter still requires a recompilation.

Another big contributions are reports about changes on the account page of the catalog (e.g. screenshots or downloads of the webpage) or library accounts for testing purposes. It has happened frequently that VideLibri worked perfectly with a library for years, but then they changed something on their webpage and VideLibri did not work anymore. An anonymous bug report "It worked last week and now it does not" is of no use, when I do not have access to their OPAC and the library refuses to reply to mails. 

You do not need to contribute to VideLibri in order to use it with an untested library, just like you do not need to contribute to Firefox in order to visit a webpage. See the backend section above, or [this (German) tutorial](http://www.videlibri.de/help/neuebibliothek.html). 

The source is structured in individual layers that are and must remain strictly separated from each other. Everything related to individual library systems is stored with the self-learned pattern matching in  data/libraries/templates/. These templates have no access to any part of the GUI. Similarly the Pascal and Java source code does not contain anything related to library systems (except for the glue code passing data between GUI and templates) and the GUI is a dumb view displaying whatever data the templates send. 


VideLibri depends on several libraries that are stored in their own repositories  (see internettools, flre, treelistview, diagram, rcmdline) and that all need to be in the compiler search path, if you want to compile it. To compile the desktop version, you open bookWatch.lpi in Lazarus and click the compile button. To compile the android version, you open videlibriandroid.lpi in Lazarus and click the compile button. Afterwards call `./gradlew assembleDebug`. 

License
-------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.