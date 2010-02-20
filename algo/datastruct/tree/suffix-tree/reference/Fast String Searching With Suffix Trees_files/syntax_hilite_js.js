function getBrowserType() {
	var detect = navigator.userAgent.toLowerCase();
	var browser;
	var doCheckIt = function (bString) {
		place = detect.indexOf(bString) + 1;
		return place;
	};
	if (doCheckIt('konqueror')) { browser = "konqueror"; }
	else if (doCheckIt('safari')) { browser = "safari"; }
	else if (doCheckIt('omniweb')) { browser = "omniweb"; }
	else if (doCheckIt('opera')) { browser = "opera"; }
	else if (doCheckIt('webtv')) { browser = "webtv"; }
	else if (doCheckIt('icab')) { browser = "icab"; }
	else if (doCheckIt('msie')) { browser = "msie"; }
	else if (doCheckIt('firefox')) { browser = "firefox"; }
	else if (!doCheckIt('compatible')) { browser = "nn"; }
	return browser;
}

function strTrim(str) {
	var i,j;
	i = 0;
	j = str.length-1;
	str = str.split("");
	while(i < str.length) {
		if(str[i]==" ") {
			str[i] = "";
		} else {
			break;
		}
		i++;
	}
	while(j > 0) {
		if(str[j]== " ") {
			str[j]="";
		} else {
			break;
		}
		j--;
	}
	return str.join("");
}

function igEncodeHTML(igHTML) {
	var regExLT = /</g;
	var regExGT = />/g;
	igHTML = igHTML.replace(regExLT, "&lt;");
	igHTML = igHTML.replace(regExGT, "&gt;");
	return igHTML;
}

function doCleanUp(sTxt) {
	sTxt = sTxt.replace(/(\r\n|\r|\n)/g, "\n");
	var arrTxt = sTxt.split("\n");
	for(i=0; i<arrTxt.length; i++) {
		if(arrTxt[i].substr((arrTxt[i].length-1), 1)==" ") {
			arrTxt[i] = arrTxt[i].substr(0, (arrTxt[i].length-1));
		}
		if(arrTxt[i].substr((arrTxt[i].length-1), 1)=="	") {
			arrTxt[i] = arrTxt[i].substr(0, (arrTxt[i].length-1));
		}
	}
	sTxt = arrTxt.join("\n");
	var regExNL1a = /([\n]{2,})/g;			//to find two consecutive 'newlines'
	var regExNL1b = /([ ]{1,})\n/g;			//to find more than 1 whitespace before 'newline'
	var regExNL1c = /([	|\t]{1,})\n/g;		//to find more than 1 tab before 'newline'
	var regExNL1d = /\n([ ]{1,})\n/g;		//to find a line with only spaces
	var regExNL1e = /\n([	|\t]{1,})\n/g;	//to find a line with only tabs
	var regExNL1g = / {4}/g;				//to find 4 space chars
	sTxt = sTxt.replace(regExNL1g, "	");
	sTxt = sTxt.replace(regExNL1d, "\n").replace(regExNL1e, "\n");
	sTxt = sTxt.replace(regExNL1b, "\n").replace(regExNL1c, "\n");
	sTxt = sTxt.replace(regExNL1a, "\n");
	if(sTxt.substr(0, 1)=="\n") {
		sTxt = sTxt.substr(1, sTxt.length);
	}
	if(sTxt.substr((sTxt.length-1), 1)=="\n") {
		sTxt = sTxt.substr(0, (sTxt.length-1));
	}
	return sTxt;
}

function getTagCode(sID) {
	var myBrowser = strTrim(navigator.appName.substring(0, 9));
	myBrowser = myBrowser.toLowerCase();
	if(document.getElementById) {
		oDoc = document.getElementById(sID);
	} else if(document.all) {
		oDoc = document.all[sID];
	}
	var getTxt = "";
	if(typeof(oDoc.innerText) != 'undefined') {
		getTxt = strTrim(oDoc.innerText);
	} else {
		getTxt = strTrim(oDoc.innerHTML);	//textContent doesn't keep \n with <LI>, so use innerHTML
		var regExLi = /<\/li>/gi;		//RegEx to find </li>
		var regExHTML = /<\S[^>]*>/g;	//RegEx to find HTML Tags
		var regExAnd = /&amp;/g;		//to find ampersand as HTML entity
		var regExSpace = /&nbsp;/g;		//to find whitespace as HTML entity
		var regExLT = /&lt;/g;			//to find < as HTML entity
		var regExGT = /&gt;/g;			//to find > as HTML entity
		getTxt = getTxt.replace(regExLi, "\n");		//replace </li> with \n
		getTxt = getTxt.replace(regExHTML, "");		//strip out all HTML Tags
		getTxt = getTxt.replace(regExAnd, "&");		//replace &amp; with &
		getTxt = getTxt.replace(regExSpace, " ");	//replace &nbsp; with simple whitespace
		getTxt = getTxt.replace(regExLT, "<");		//replace &lt; with <
		getTxt = getTxt.replace(regExGT, ">");		//replace &gt; with >
	}
	return getTxt;
}

function showCodeTxt(sId) {
	var cdTxt = igEncodeHTML(getTagCode(sId));
	cdTxt = doCleanUp(cdTxt);
	var cdTxtPrefix = "<html><head><title>iG:Syntax Hiliter &raquo; Plain-Text View</title><style>body { margin:0px; padding:0px; white-space:nowrap; }</style></head><body><pre>\n";
	var cdTxtSuffix = "\n</pre><br /></body></html>";
	cdWin = window.open("about:blank", "cdWin", "toolbar=0,scrollbars=1,location=0,statusbar=0,menubar=0,resizable=1,width=700,height=400,left=35,top=85");
	cdWin.document.open();
	cdWin.document.write(cdTxtPrefix+cdTxt+cdTxtSuffix);
	cdWin.document.close();
}

function getCodeTxt(sId) {
	var cdTxt = igEncodeHTML(getTagCode(sId));
	cdTxt = doCleanUp(cdTxt);
	return cdTxt;
}

function hidePlainTxt(bID) {
	var oCodeBox = document.getElementById(bID);
	if(arrCode[bID]=="") {
		alert("The HTML View for this Code Box is not available");
	} else {
		var lnkID = "l"+bID;
		lnkID = lnkID.toLowerCase();
		var oLnk = document.getElementById(lnkID);
		var sInnerHTML = "<a href=\"#\" onclick=\"javascript:showPlainTxt('"+bID+"'); return false;\">PLAIN TEXT</a>";
		oLnk.innerHTML = sInnerHTML;
		oCodeBox.innerHTML = "";
		oCodeBox.innerHTML = arrCode[bID];
		arrCode[bID] = "";
	}
}

function showPlainTxt(bID) {
	var sHtmlCode, sPlainCode, sInnerHTML, oLnk, intHeightDiff, intWidthDiff;
	var browserName = getBrowserType();
	if(browserName=="msie") {
		intHeightDiff = 20;
		intWidthDiff = 5;
	} else if(browserName=="opera") {
		intHeightDiff = 20;
		intWidthDiff = 12;
	} else if(browserName=="firefox") {
		intHeightDiff = 20;
		intWidthDiff = 12;
	}
	var oCodeBox = document.getElementById(bID);
	//get InnerHTML
	sHtmlCode = oCodeBox.innerHTML;
	arrCode[bID] = sHtmlCode;
	var lnkID = "l"+bID;
	lnkID = lnkID.toLowerCase();
	oLnk = document.getElementById(lnkID);
	sInnerHTML = "<a href=\"#\" onclick=\"javascript:hidePlainTxt('"+bID+"'); return false;\">HILITED HTML</a>";
	oLnk.innerHTML = sInnerHTML;
	sPlainCode = getCodeTxt(bID);
	var cbHeight = oCodeBox.parentNode.clientHeight;
	var cbWidth = oCodeBox.parentNode.clientWidth;
	var ptHeight = cbHeight-intHeightDiff;
	var ptWidth = cbWidth-intWidthDiff;
	sPlainCodeHTML = "<textarea style=\"width:"+ptWidth+"px; height:"+ptHeight+"px;\" wrap=\"off\">"+sPlainCode+"</textarea>";
	oCodeBox.innerHTML = "";
	oCodeBox.innerHTML = sPlainCodeHTML;
}
