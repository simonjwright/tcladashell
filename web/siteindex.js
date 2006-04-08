//------------------------------------------------------------
//
// File Name:	siteindex.js
//
// Purpose:	Writes standard TASH site index HTML.
//
//------------------------------------------------------------

var output = "";

output += "<div id='myChaser'>\n";
output += "  <div class='navhead'>Site Index</div>\n";
output += "  <div class='nav'>\n";
output += "    <A HREF='javascript:void(0)'\n";
output += "       onMouseOver = 'popUp(\"elMenu1\",event)'\n";
output += "        onMouseOut = 'popDown(\"elMenu1\")'\n";
output += "           onClick = 'return false'>\n";
output += "      TASH </A>\n";
output += "    </div>\n";
output += "  <div class='nav'>\n";
output += "    <A HREF='javascript:void(0)'\n";
output += "       onMouseOver = 'popUp(\"elMenu2\",event)'\n";
output += "        onMouseOut = 'popDown(\"elMenu2\")'\n";
output += "           onClick = 'return false'>\n";
output += "      Tcl/Tk </A>\n";
output += "    </div>\n";
output += "  <div class='nav'>\n";
output += "    <A HREF='javascript:void(0)'\n";
output += "       onMouseOver = 'popUp(\"elMenu3\",event)'\n";
output += "        onMouseOut = 'popDown(\"elMenu3\")'\n";
output += "           onClick = 'return false'>\n";
output += "      Ada </A>\n";
output += "    </div>\n";
output += "</div>\n";

document.write (output);
