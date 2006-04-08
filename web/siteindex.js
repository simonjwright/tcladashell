//------------------------------------------------------------
//
// File Name:	siteindex.js
//
// Purpose:	Writes standard TASH site index HTML.
//
// Inputs:      var basehref = 'web/' for index.htm file,
//                           = undefined for all other files
//
//------------------------------------------------------------

var output = "";

if (typeof basehref == "undefined") {
  var homebasehref='../';
  var basehref='';
}

output += "<div align=left class='nav'>";
output += "<table border='0' cellspacing='0' cellpadding='0'>";
output += "  <tr>";
output += "    <td class='navhead'>TASH</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+homebasehref+"index.htm'>Home</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"tash.htm'>Introduction</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <A href='"+basehref+"download.htm'>Download</A></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"license.htm'>Software&nbsp;License</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <A href='"+basehref+"release.htm'>Release&nbsp;Notes</A></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"docs.htm'>Documentation</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"unix.htm'>Unix</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"windows.htm'>Windows</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"demos.htm'>Demo&nbsp;Programs</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"tests.htm'>Test&nbsp;Programs</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"maillist.htm'>Mailing&nbsp;List</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td class='navhead'> Tcl/Tk</td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='http://www.tcl.tk/'>Users&nbsp;Home</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='http://www.tcl.tk/scripting/'>Getting&nbsp;Started</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='http://www.activestate.com/tcl/'>Binary&nbsp;downloads</td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='http://tcl.sourceforge.net/'>Developers&nbsp;Home</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"tclbooks.htm'>Books</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td class='navhead'>Ada</td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='http://www.adapower.com'>Ada&nbsp;Power</a></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <A href='http://www.acm.org/sigada/'>ACM&nbsp;SIGAda</A></td>";
output += "    </tr>";
output += "  <tr>";
output += "    <td> <a href='"+basehref+"adabooks.htm'>Books</a></td>";
output += "    </tr>";
output += "</table>";
output += "</div>";

document.write (output);
