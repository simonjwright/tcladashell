//------------------------------------------------------------
//
// File Name:	footer.js
//
// Purpose:	Writes standard TASH footer HTML.
//
//------------------------------------------------------------

var output  = "";

// Disguise email address
//-----------------------
var first   = 'ma';
var second  = 'il';
var third   = 'to:';
var address = '&#116;&#119;&#101;&#115;&#116;&#108;&#101;&#121;';
var domain  = '&#97;&#100;&#97;&#116;&#99;&#108;&#46;&#99;&#111;&#109;';

output += "      <br>\n";
output += "      </div>\n";
output += "      </td>\n";
output += "    <!-- Body text right margin -->\n";
output += "    <td width='20'> &nbsp;</td>\n";
output += "    </tr>\n";
output += "  <tr class='sig'>\n";
output += "    <th> &nbsp; Author  </th>\n";
output += "    <th> &nbsp;         </th>\n";
output += "    <th> E-Mail Address </th>\n";
output += "    <th> Last Revised   </th>\n";
output += "    <th> &nbsp;         </th>\n";
output += "    </tr>\n";
output += "  <tr class='sig'>\n";
output += "    <td> &nbsp; Terry J. Westley </td>\n";
output += "    <td> &nbsp;                  </td>\n";
output += '    <td> <a href="';
output += first+second+third;
output += address;
output += '&#64;';
output += domain;
output += '">&nbsp;';
output += address;
output += '&#64;';
output += domain;
output += '<\/a></td>\n';
output += "    <td> " + document.lastModified + " </td>\n";
output += "    <td> &nbsp; </td>\n";
output += "    </tr>\n";

document.write (output);
