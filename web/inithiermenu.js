//------------------------------------------------------------
//
// File Name:	inithiermenu.js
//
// Purpose:	Initialization for running DHTML Lab
//		Hierarchical Menus.
//
//------------------------------------------------------------

// set browser-detection variables
//--------------------------------
   NS4 = (document.layers);
   IE4 = (document.all);
  ver4 = (NS4 || IE4);
   IE5 = (IE4 && navigator.appVersion.indexOf("5.")!=-1);
 isMac = (navigator.appVersion.indexOf("Mac") != -1);
isMenu = (NS4 || (IE4 && !isMac) || (IE5 && isMac));

// default functionality for dHTML-challenged browsers
//----------------------------------------------------
function popUp() {
    return
};

function popDown(){
    return
};

if (!ver4) {
    event = null;
};

// set parameter variables
//------------------------
if (isMenu) {

    menuVersion = 3;

    menuWidth = 100;        // menu width (pixels)
    childOverlap = 6;       // child menu overlaps its parent
    childOffset = 3;        // child menu offset from parent
    perCentOver = null;     // child menu % overlap its parent
                            // (overrides childOverlap if not null)
    secondsVisible = 0.1;   // How many seconds should the top level menu
                            // in a tree remain visible upon a users
                            // mouseout of any menu in the tree?

    fntCol = "#006699";     // font color
    fntSiz = "10";          // font size
    fntBold = true;         // Boolean for bold
    fntItal = false;        // ...or italic text
    fntFam = "verdana,arial,geneva,helvetica,sans-serif";  // font family

    backCol = "#FFFFCC";    // background color
    overCol = "#006699";    // mouseover background color 
    overFnt = "#FFFFCC";    // font color on mouseover (IE only)

    borWid = 2;             // border width (pixels)
    borCol = "#006699"      // border color
    borSty = "solid";       // border style (IE only)
    itemPad = 3;            // 

    imgSrc = "images/tri.gif"; // image that indicates there is a child item
    imgSiz = 10;            // default size of image

    separator = 1;          // 
    separatorCol = "#006699"; // (IE only)
    
    isFrames = false;       // using frameset?
    navFrLoc =  "left";     // where is the nav frame?
    mainFrName = "main";    // name of the main frame?

    clickStart = false;     // show menus on mouseover
    clickKill = false;      // hide menus on mouseout

    keepHilite = true;      // maintain item highlight on menu traversal

    NSfontOver = true;      // change NS font color onmouseover
    showVisited = "green";  // shade visited items for NS
};

// Define menu arrays and load menu engine
//----------------------------------------
if (isMenu) {
    document.write("<SCRIPT LANGUAGE='JavaScript1.2' SRC='hierArrays.js' TYPE='text/javascript'><\/SCRIPT>");

    document.write("<SCRIPT LANGUAGE='JavaScript1.2' SRC='hierMenus.js' TYPE='text/javascript'><\/SCRIPT>");
};
