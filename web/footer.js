//------------------------------------------------------------
//
// File Name:	footer.js
//
// Purpose:	Writes standard TASH footer HTML.
//
//------------------------------------------------------------

var output = "";

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
output += "    <td> <a href='mailto:twestley@adatcl.com'>\n";
output += "         twestley@adatcl.com</a> </td>\n";
output += "    <td> " + document.lastModified + " </td>\n";
output += "    <td> &nbsp; </td>\n";
output += "    </tr>\n";

document.write (output);
