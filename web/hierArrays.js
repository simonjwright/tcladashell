//-----------------------------------------------------------
//
// File Name: hierArrays.js
//
// Purpose:   This file defines the arrays necessary for
//            implementing DHTML Lab Hierarchical Menus.
//
//-----------------------------------------------------------

// Find base URL
//--------------
re  = /\/[^/]*$/
loc = new String (document.location);
baseURL = loc.replace(re, "/");

arMenu1 = new Array(
   150,
   "","",
   "","",
   "","",
   "","",
   "Home",            baseURL + "index.htm",       0,
   "Introduction",    baseURL + "tash.htm",        0,
   "Download",        baseURL + "download.htm",    0,
   "License",         baseURL + "license.htm",     0,
   "Release Notes",   baseURL + "release.htm",     0,
   "Documentation",   baseURL + "docs.htm",        0,
   "Install",         "",                          1,
   "Test programs",   baseURL + "tests.htm",       0,
   "Demo programs",   baseURL + "demos.htm",       0,
   "Mailing List",    baseURL + "maillist.htm",    0
)

arMenu1_7 = new Array(
   "Unix",            baseURL + "unix.htm",        0,
   "Windows",         baseURL + "windows.htm",     0
)

arMenu2 = new Array(
   150,
   "","",
   "","",
   "","",
   "","",
   "Scriptics",       "http://dev.scriptics.com/",           0,
   "Intro to Tcl/Tk", "http://dev.scriptics.com/scripting/", 0,
   "Tcl Consortium",  "http://www.tclconsortium.com/",       0,
   "Tcl/Tk Books",    baseURL + "tclbooks.htm",              0
)

arMenu3 = new Array(
   150,
   "","",
   "","",
   "","",
   "","",
   "Ada Power",   "http://www.adapower.com/",                               0,
   "Ada FAQ",     "http://www.adapower.com/lab/adafaq/",                    0,
   "Tutorial",    "http://www.adahome.com/Tutorials/Lovelace/lovelace.htm", 0,
   "ACM SIGAda",  "http://www.acm.org/sigada/",                             0,
   "Ada Home",    "http://www.adahome.com/",                                0,
   "Ada Books",   baseURL + "adabooks.htm",                                 0
)
