// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
// Put your codes here


//Use Fox Splitter addon to mimic C-x 1/2/3 stuff
ext.add("other-window", function() {
    function focusSubBrowserById(aId) {
        SplitBrowser.getSubBrowserById(aId).browser.contentWindow.focus();
    }

    var browsers = SplitBrowser.browsers;
    if (SplitBrowser.activeBrowser === gBrowser) {
        focusSubBrowserById(browsers[arg == null ? 0 : browsers.length - 1].id);
        return;
    }
    var id = SplitBrowser.activeSubBrowser.id;
    for (var i = 0; i < browsers.length; i++) {
        if (browsers[i].id == id) {
            break;
        }
    }
    var nextIndex = arg == null ? i + 1 : i - 1;
    if (nextIndex >= browsers.length || nextIndex < 0) {
        gBrowser.contentWindow.focus();
    } else {
        focusSubBrowserById(browsers[nextIndex].id);
    }
}, 'Select another window (Fox Splitter addon)');

ext.add("delete-frame", function() {
    SplitBrowser.activeBrowserCloseWindow();
}, 'Close current active "window" (SplitBrowser addon)');

ext.add("delete-window", function() {
    if (typeof SplitBrowser == "undefined") {
        BrowserCloseTabOrWindow();
    } else {
        var b = SplitBrowser.activeBrowser;
        if (b.mTabs.length > 1) {
            b.removeTab(b.mCurrentTab);
        } else if (b === gBrowser) {
            gBrowser.removeTab(gBrowser.mCurrentTab);
        }
    }
}, 'Close tab (or \'window\', if Spli tBrowser addon installed)');

ext.add("delete-other-windows", function() {
    var url = SplitBrowser.activeBrowser != gBrowser ? SplitBrowser.activeSubBrowser.src : null;
    var browsers = SplitBrowser.browsers;
    for (var i = 0; i < browsers.length; ++i) {
        browsers[i].close();
    }
    if (url) {
        window.loadURI(url);
    }
}, 'delete-other-window (Fox Splitter addon)');


ext.add("split-window-vertically" , function() {
    if ( (typeof(SplitBrowser)) != "undefined" ) {
        SplitBrowser.addSubBrowser(window.content.location.href, SplitBrowser.activeSubBrowser, SplitBrowser.POSITION_BOTTOM);
    } else {
        splitpannel.toggle(window._content.document.location, true, "bottom");
    }
}, 'split-window-vertically (Fox Splitter addon)');

ext.add("split-window-horizontally", function() {
    if ( (typeof(SplitBrowser)) != "undefined" ) {
        SplitBrowser.addSubBrowser(window.content.location.href, SplitBrowser.activeSubBrowser, SplitBrowser.POSITION_RIGHT);
    } else {
        splitpannel.toggle(window._content.document.location, true, "right");
    }
}, 'split-window-horizontally (Fox Splitter addon)');
    
ext.add("view-in-split-panel", function () {
    splitpannel.toggle(window._content.document.location, true, 'right');
}, 'Open Split Panel and load current URL in it .');

ext.add("cnblogs-ing-in-split-panel", function () {
    splitpannel.toggle('http://space.cnblogs.com/mi/', true, 'right');
}, 'Open Split Panel and load http://space.cnblogs.com/mi/ in it .');

ext.add("google-reader-i-in-split-panel", function() {
    splitpannel.toggle('https://www.google.com/reader/i/#stream/user%2F02753487812100788291%2Fstate%2Fcom.google%2Fread', true, 'right');
}, 'Open Split Panel and load Google Reader in it .');

ext.add("google-translate-in-split-panel", function () {    
    splitpannel.toggle("http://translate.google.com/m?hl=zh-CN&sl=auto&tl=en&ie=UTF-8", true, 'right');
}, 'Open Split Panel and load Google Translate (any->en) in it .');
    
ext.add("google-translate-cn-in-split-panel", function () {
    splitpannel.toggle("http://translate.google.com/m?hl=zh-CN&sl=auto&tl=zh-CN&ie=UTF-8", true, 'right');
}, 'Open Split Panel and load Google Translate (any->zh-CN) in it .');

ext.add("read-it-later-list-in-split-panel", function() {    
    splitpannel.toggle("http://readitlaterlist.com/unread", true, 'right');
}, 'Show Read It Later list Split Panel');


ext.add("view-in-sidebar", function() {
    toggleSidebar('', false);

    var sidebarcmd = document.getElementById('viewURISidebar');
    //print(sidebarcmd.getAttribute("sidebarurl"))
    //print(sidebarcmd.getAttribute("checked"))
    sidebarcmd.removeAttribute("checked");
    sidebarcmd.setAttribute("sidebarurl",   content.location.href);
    sidebarcmd.setAttribute("sidebartitle", content.document.title);

    toggleSidebar('viewURISidebar', true);
}, "Load current URL in sidebar");

//toggle sidebar
ext.add("toggle-sidebar", function () {
    toggleSidebar("");
}, 'toggle sidebar');
 
ext.add("read-it-later-sidebar", function () {
    toggleSidebar("RIL_sidebarlist");
}, 'Toggle ReadItLater sidebar. (readitlater extension)');
 
ext.add("save-to-read-sidebar", function () {    
    toggleSidebar("viewSidebar_save2read");
}, 'Toggle Save-To-Read sidebar. (save2read extension)');

ext.add("pano-sidebar", function() {
    toggleSidebar('viewPanoramaSidebar');
}, 'Toggle Pano side bar (extension Pano).');
    
ext.add("scrapbook-sidebar", function () {
    toggleSidebar("viewScrapBookSidebar");
}, 'Toggle Scrapbook sidebar (extension Scrapbook or Scrapbook Plus)');


//make some ScrapBook (Plus)'s command could be manipulated with keyboard     
ext.add("scrapbook-highlight", function(ev, arg) {
    //if ARG given, switch to correspding highligher and use it
    sbPageEditor.highlight(arg);
}, "Highlight selection with scrapbook's highlighter");

ext.add("scrapbook-undo", function() {
    sbPageEditor.undo();
}, "Scrapbook Editor's undo.");

ext.add("scrapbook-save", function() {
    sbPageEditor.saveOrCapture();
}, "Capture current page to Scrapbook, or save modification.");


//is.gd service
ext.add("is.gd", function () {
    let endpoint = "http://is.gd/api.php?longurl=" + encodeURIComponent(window._content.document.location);
    let result = util.httpGet(endpoint, true);
    if (result.status == 200) {
        command.setClipboardText(result.responseText);
        display.echoStatusBar("Short URL copied into clipboard: " + result.responseText, 3000);
    }
    else
        display.echoStatusBar("is.gd service failed: " + result.statusText, 3000);
}, "Shorten current page's URL with http://is.gd service");

// goo.gl
ext.add("goo.gl", function () {
    let endpoint = "https://www.googleapis.com/urlshortener/v1/url";
    let params = { "longUrl": window._content.document.location.href };
    let result = util.httpPostJSON(endpoint, params, function (xhr) {
        if (xhr.status == 200) {        
            var ret = JSON.parse(xhr.responseText);
            command.setClipboardText(ret.id);        
            display.echoStatusBar("Short URL copied into clipboard: " + ret.id, 3000);
        } else {
            display.echoStatusBar("goo.gl service failed: " + xhr.statusText, 3000);
        }
    });
}, "Shorten URL with http://goo.gl service");
 
// goo.gl only accepts content-type as 'application/json',
// but keysnail's httpGet/httpPost doesn't support it
util.httpPostJSON= function (url, params, callback) {
            let xhr = new XMLHttpRequest();
 
            switch (typeof params)
            {
            case "string":
                // nothing
                break;
            case "object":
                params = JSON.stringify(params);
                break;
            default:
                params = "";
                break;
            }
 
            let async = typeof callback === "function";
 
            if (async)
            {
                xhr.onreadystatechange = function () {
                    if (xhr.readyState === 4)
                        callback(xhr);
                };
            }
 
            xhr.open("POST", url, async);
 
            xhr.setRequestHeader("Content-type", "application/json");
            xhr.setRequestHeader("Content-length", params.length);
            xhr.setRequestHeader("Connection", "close");
 
            xhr.send(params);
 
            return xhr;
}

ext.add("bookmark-on-delicious", function(ev, arg) {
    var f='http://www.delicious.com/save?url='+encodeURIComponent(content.location.href)+
        '&title='+encodeURIComponent(content.title)+
        '&notes='+encodeURIComponent(''+(content.getSelection?
                                         content.getSelection():
                                         content.getSelection?
                                               content.getSelection():
                                               content.selection.createRange().text))+
        '&v=6&';
    var a=function(){
        if(!window.open(f+'noui=1&jump=doclose','deliciousuiv6',
                        'location=yes,links=no,scrollbars=no,toolbar=no,width=550,height=550'))
            location.href=f+'jump=yes'
    };
    if(/Firefox/.test(navigator.userAgent)) {
        setTimeout(a,0)}
    else{
        a();
    }
}, "bookmark current page on delicious.");

//Yet Another Twitter Client KeySnail 
plugins.options["twitter_client.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "SPC"   : "prompt-next-page",
    "b"     : "prompt-previous-page",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "q"     : "prompt-cancel",
    // twitter client specific actions
    "t"     : "tweet",
    "r"     : "reply",
    "R"     : "retweet",
    "D"     : "delete-tweet",
    "f"     : "add-to-favorite",
    "v"     : "display-entire-message",
    "V"     : "view-in-twitter",
    "c"     : "copy-tweet",
    "s"     : "show-target-status",
    "@"     : "show-mentions",
    "/"     : "search-word",
    "o"     : "open-url"
};

plugins.options["twitter_client.update_interval"] = 10000;


//jump to previous page or next page
ext.add("previous-page", function () {
    var document = window._content.document;
    var links = document.links;
    for(i = 0; i < links.length; i++) {
        if (   (links[i].text == '上一页')   || (links[i].text == '<上一页')
               || (links[i].text == '上一篇') || (links[i].text == '< 前一页')
               || (links[i].text == 'Previous') || (links[i].text == 'Prev') 
               || (links[i].text == '<')        || (links[i].text == '<<')) 
            document.location = links[i].href;
    }
}, "Previous page");

ext.add("next-page", function () {
    var document = window._content.document;
    var links = document.links;
    for(i = 0; i < links.length; i++) {
        if (   (links[i].text == '下一页')  || (links[i].text == '下一页>')
               || (links[i].text == '下一篇') || (links[i].text == '后一页 >')
               || (links[i].text == 'Next')    || (links[i].text == 'next') 
               || (links[i].text == '>')       || (links[i].text == '>>')) 
            document.location = links[i].href;
    }
}, "Next page");


// paste and go
ext.add("paste-and-go", function() {
    var url = command.getClipboardText();
    if (url.indexOf("://") != -1)
    {
	    window._content.location = url;
    }
    else
    {
	    //url = util.format("http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8", encodeURIComponent(url));
	    BrowserSearch.loadSearch(url, false);
    }
}, "Paste the URL or keyword from clipboard and Go");

ext.add("paste-to-tab-and-go", function() {
    var url = command.getClipboardText();
    if (url.indexOf("://") != -1)
	    gBrowser.loadOneTab(url, null, null, null, false);
    else
    {
	    //url = util.format("http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8", encodeURIComponent(url));
	    BrowserSearch.loadSearch(url, true);
    }
}, "Paste the URL or keyword from clipboard to a new tab and Go");

// selection
ext.add("search-selection", function() {
    if(!getBrowserSelection()) return;
    BrowserSearch.loadSearch(getBrowserSelection(), true);
}, "Use the default search engine to search the phrase currently selected");

ext.add("go-to-selected-url", function() {
    if(!getBrowserSelection()) return;
    gBrowser.loadOneTab(getBrowserSelection(), null, null, null, true);
}, "Open selected text as an URL and go to it.");


ext.add("next-occur", function() {
    var word = getBrowserSelection();
    if (word) {
        gFindBar._findField.value = word;
        gFindBar._highlightDoc(true, word);
    }
    gFindBar.onFindAgainCommand(false);
}, "highlight next occurrence of current selected word");

ext.add("previous-occur", function() {
    var word = getBrowserSelection();
    if (word) {
        gFindBar._findField.value = word;
        gFindBar._highlightDoc(true, word);
    }
    gFindBar.onFindAgainCommand(true);
}, "highlight previouse occurence of current selected word");

ext.add("increase-digit-in-url", function() {
    var pattern = /(.*?)([0]*)([0-9]+)([^0-9]*)$/;
    var url = content.location.href;
    var digit = url.match(pattern);
    if (digit[1] && digit[3]) {
        let len = digit[3].length;
        let next = +digit[3] + (arg ? arg : 1);
        content.location.href = digit[1] + (digit[2] || "").slice(next.toString().length - len) + next + (digit[4] || "");
    }
}, 'Increment last digit in the URL');

ext.add("decrease-digit-in-url", function() {
    var pattern = /(.*?)([0]*)([0-9]+)([^0-9]*)$/;
    var url = content.location.href;
    var digit = url.match(pattern);
    if (digit[1] && digit[3]) {
        let len = digit[3].length;
        let next = +digit[3] - (arg ? arg : 1);
        content.location.href = digit[1] + (digit[2] || "").slice(next.toString().length - len) + next + (digit[4] || "");
    }
}, 'Decrement last digit in the URL');

ext.add("count-region", function(ev, arg) {
    var aInput = ev.originalTarget;
    var value = aInput.value;
 
    var selcount = aInput.selectionEnd - aInput.selectionStart;
 
    display.echoStatusBar("Total:" + value.length + " Selected:" + selcount, 3000);
}, "Count chars.");

function inputChars(ev, chars) {
    var aInput = ev.originalTarget;
    var value = aInput.value;
    var originalSelStart = aInput.selectionStart;
    
    aInput.value = value.slice(0, aInput.selectionStart) + chars + value.slice(aInput.selectionEnd, value.length)
    
    aInput.selectionStart = originalSelStart + 1;
    aInput.selectionEnd = aInput.selectionStart;
}

//{{{ inline translate:
// based on code stolen from Mar Mod extension
function google_translate (whatToTranslate, lang, callback) {
     if (whatToTranslate.length<=0 || whatToTranslate.length>=1000) {
         alert('Text is too long');
         return;
     }
    
    var httpRequest = null;
    
    //e.g http://translate.google.com/m?hl=zh-CN&sl=auto&tl=en&ie=UTF-8&prev=_m&q=dictionary
    // var fullUrl = "http://translate.google.com/m?hl=" + lang + "&sl=auto&tl=" + lang + "&ie=UTF-8" +
    //    "&q=" + whatToTranslate;
    var fullUrl = "http://translate.google.hu/translate_t?text=" + whatToTranslate +
        "&hl=" + lang + "&langpair=auto|" + lang + "&tbb=1" ;

    function removeHTMLTags(mitkell) {  //clean up a string from html tags
 	    var strInputCode = mitkell;
 	    var strTagStrippedText = strInputCode.replace(/<\/?[^>]+(>|$)/g, "");
 	    return strTagStrippedText;
 	}

    function infoReceived() {  // if there is response from Google then write out translation
        var output = httpRequest.responseText;
        // alert(output)
        if (output.length) {
            // Build the output string from Google Page
            output = output.replace(/&quot;/gi,'"');
            output = output.replace(/&lt;/gi,'<');
            output = output.replace(/&gt;/gi,'>');
            output = output.replace(/&amp;/gi,'&');
            output = output.replace(/&#39;/gi,"'");
            var fieldArray = output.split('</head>');
            if (fieldArray[1].search('class="short_text"')!=-1) {
                var tempResz = fieldArray[1].split('<span id=result_box class="short_text">');
            }
            else if (fieldArray[1].search('class="medium_text"')!=-1) {
                var tempResz = fieldArray[1].split('<span id=result_box class="medium_text">');
            }
            else {
                var tempResz = fieldArray[1].split('<span id=result_box class="long_text">');
            }
            //alert(tempResz[1]);
            var kimenet = tempResz[1].split('</span></div>');
            if (callback) {
                callback(kimenet[0]);
            } else {
                display.echoStatusBar(kimenet[0], 5000);               
            }
        }
    }
    
    httpRequest = new XMLHttpRequest();
    httpRequest.open("GET", fullUrl, true);
    httpRequest.onload = infoReceived;
    httpRequest.send(null);
}

function inline_translate_selection(lang) {
    
    var callback = function (result) {
             var range = content.getSelection().getRangeAt(0);
             range.deleteContents();
             range.insertNode(document.createTextNode(removeHTMLTags(result[0])));
    };

    google_translate(content.getSelection(), lang, callback);
}

ext.add("google-translate-selection-inline", function() {
    inline_translate_selection("en");
}, "Translate the selection and replace it with result.");

ext.add("google-translate-selection-inline-to-cn", function() {
    inline_translate_selection("zh-CN");
}, "Translate the selection and replace it with result.");

ext.add("google-translate-selection", function() {
    var selection = content.getSelection();
    var callback = function(result) {
        //alert(result);
        // <span title="Emacs粉丝应该很喜欢这个插件" onmouseover="this.style.backgroundColor='#ebeff9'" onmouseout="this.style.backgroundColor='#fff'">Emacs fans should like this plug-in</span>
        var ret = result.split(">");
        result = ret[1].split("<");
        display.echoStatusBar(selection + ": " + result[0], 5000)
    }
    google_translate(selection, "en", callback);
}, "Translate the selection to English and show result in status bar.");

ext.add("google-translate-selection-to-cn", function() {
    var selection = content.getSelection();
    var callback = function(result) {
        //alert(result);
        // <span title="Emacs粉丝应该很喜欢这个插件" onmouseover="this.style.backgroundColor='#ebeff9'" onmouseout="this.style.backgroundColor='#fff'">Emacs fans should like this plug-in</span>
        var ret = result.split(">");
        result = ret[1].split("<");
        display.echoStatusBar(selection + ": " + result[0], 5000)
    }
    google_translate(selection, "zh-CN", callback);
}, "Translate the selection to Chinese and show result in status bar.");

ext.add("wiktionary-lookup-selection", function() {
    if (!getBrowserSelection()) {
        display.echoStatusBar("Wiktionary: You must selection something.", 2000);
        return;
    }
    gd12.select.lookupSelected();
}, 'Show translation for selection (extension: Wiktionary & Google Translate).');

ext.add("open-extension-dialog", function(ev, arg) {
    var wm = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator);

    OpenAddonsMgr = function(type, typeUrl) {
        var extensionManager = wm.getMostRecentWindow("Extension:Manager");
        if (extensionManager) {
            extensionManager.focus();
            extensionManager.showView(type);
        } else {
            var addonManager = wm.getMostRecentWindow("Addons:Manager");
            if (addonManager) {
                addonManager.focus();
                addonManager.gViewController.loadView(typeUrl);
            } else {
                //var contents = toolbar_buttons.getUrlContents("chrome://mozapps/content/extensions/extensions.xul");
                window.openDialog(
                    "chrome://mozapps/content/extensions/extensions.xul",
                    "",
                    "chrome,menubar,extra-chrome,toolbar,dialog=no,resizable,width=1024,height=768,centerscreen",
                    //contents.match("Addons:Manager") ? {"view" :typeUrl} : type);
                    {"view" :typeUrl});
            }
        }
    };
    OpenAddonsMgr('extensions', 'addons://list/extension');
}, "Open the Addons Manager.");

ext.add("gwt", function() {
    var newurl = "http://gxc.google.com/gwt/x?u=" + window.content.location.href;
    content.location.href = newurl;
}, "Use GWT to view current url.");

    
ext.add("evernote-clearly", function() {
   readable_by_evernote__menu__call();
}, "Read current page in Evernote Clearly (call again to quit)."); 

ext.add("tabundle-group", function(ev, arg) {
    Tabundle.createGroupListHtml = function() {
		//....https://gist.github.com/1851778
    };
    Tabundle.createIndexHtml();
    //var path = Tabundle.createListHtml()
    var path = Tabundle.createGroupListHtml();
    if (path) {
        gBrowser.selectedTab = gBrowser.addTab('file://' + path)
    }
}, "Use tabundle extension to capture info of all tabs of current group."); 
    
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

hook.setHook('KeySnailInitialized', function () {
    hook.removeHook("KeySnailInitialized", arguments.callee);
    var displayHelpKey = [];
    for (let[k, act] in Iterator(actionKeys.selector)) {
        if (act === "prompt-display-keymap-help") {
            displayHelpKey.push(k);
        }
    }
    $("keysnail-prompt-selector-help-title").setAttribute("value", util.getLocaleString("promptSelectorKeymapHelpTitle", [displayHelpKey.join(", ")]));
});

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }
    command.closeFindBar();
    var marked = command.marked(aEvent);
    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});

hook.setHook('Unload', function () {
    util.getBrowserWindows().some(function (win) {
        if (win === window) {
            return false;
        }
        const ks = win.KeySnail;
        share.pluginUpdater = ks.getPluginUpdater(share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});
hook.addToHook('Unload', function () {
    util.getBrowserWindows().some(function (win) {
        if (win === window) {
            return false;
        }
        const ks = win.KeySnail;
        share.pluginUpdater = ks.getPluginUpdater(share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});
hook.addToHook('Unload', function () {
    util.getBrowserWindows().some(function (win) {
        if (win === window) {
            return false;
        }
        const ks = win.KeySnail;
        share.pluginUpdater = ks.getPluginUpdater(share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});
hook.addToHook('Unload', function () {
    util.getBrowserWindows().some(function (win) {
        if (win === window) {
            return false;
        }
        const ks = win.KeySnail;
        share.pluginUpdater = ks.getPluginUpdater(share.pluginUpdater.pluginsWithUpdate);
        ks.setUpPluginUpdaterDelegator();
        return true;
    });
});


// ============================= Key bindings ============================== //

key.setGlobalKey('C-M-r', function (ev) {
    userscript.reload();
}, 'Reload the initialization file', true);

key.setGlobalKey(['C-c', 'C-r'], function (ev) {
    userscript.reload();
}, 'Reload the initialization file', true);

key.setGlobalKey('M-x', function (ev, arg) {
    ext.select(arg, ev);
}, 'List exts and execute selected one', true);

key.setGlobalKey('M-:', function (ev) {
    command.interpreter();
}, 'Command interpreter', true);

key.setGlobalKey(['<f1>', 'b'], function (ev) {
    key.listKeyBindings();
}, 'List all keybindings');

key.setGlobalKey(['<f1>', 'F'], function (ev) {
    openHelpLink("firefox-help");
}, 'Display Firefox help');

key.setGlobalKey('C-m', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RETURN, true);
}, 'Generate the return key code');

key.setGlobalKey(['C-x', 'l'], function (ev) {
    command.focusToById("urlbar");
}, 'Focus to the location bar', true);

key.setGlobalKey(['C-x', 'g'], function (ev) {
    command.focusToById("searchbar");
}, 'Focus to the search bar', true);

key.setGlobalKey(['C-x', 't'], function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, 'Focus to the first textarea', true);

key.setGlobalKey(['C-x', 's'], function (ev) {
    command.focusElement(command.elementsRetrieverButton, 0);
}, 'Focus to the first button', true);

key.setGlobalKey(['C-x', '0'], function (ev, arg) {
    ext.exec("delete-window", arg, ev);
}, 'Close current active "window" (Fox Splitter addon)');

key.setGlobalKey(['C-x', '1'], function (ev, arg) {
    ext.exec("delete-other-windows", arg, ev);
}, 'close-other-window (Fox Splitter addon)');

key.setGlobalKey(['C-x', '2'], function (ev, arg) {
    ext.exec("split-window-vertically", arg, ev);
}, 'split-window-vertically (Fox Splitter addon)');

key.setGlobalKey(['C-x', '3'], function (ev, arg) {
    ext.exec("split-window-horizontally", arg, ev);
}, 'split-window-horizontally (Fox Splitter addon)');

key.setGlobalKey([['C-x', 'k'], ['C-c', 'x']], function (ev) {
    ext.exec("delete-window", arg, ev);
}, 'Close tab (or \'window\', if Fox Splitter addon installed)');

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    ext.exec("other-window", arg, ev);
}, 'other-window (Fox Splitter addon)', true);

key.setGlobalKey([['C-x', 'K'], ['C-x', '5', '0']], function (ev) {
    closeWindow(true);
}, 'Close the window');

key.setGlobalKey([['C-x', 'n'], ['C-x', '5', '2']], function (ev) {
    OpenBrowserWindow();
}, 'Open new window');

key.setGlobalKey(['C-x', 'C-c'], function (ev) {
    goQuitApplication();
}, 'Exit Firefox', true);

key.setGlobalKey(['C-x', 'C-o'], function (ev, arg) {
    command.focusOtherFrame(arg);
}, 'Select next frame');

key.setGlobalKey(['C-x', 'C-1'], function (ev) {
    window.loadURI(ev.target.ownerDocument.location.href);
}, 'Show current frame only', true);

key.setGlobalKey(['C-x', 'C-f'], function (ev) {
    BrowserOpenFileWindow();
}, 'Open the local file', true);

key.setGlobalKey(['C-x', 'C-s'], function (ev) {
    saveDocument(window.content.document);
}, 'Save current page to the file', true);

key.setGlobalKey('M-w', function (ev) {
    command.copyRegion(ev);
}, 'Copy selected text', true);

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs like incremental search forward', true);

key.setGlobalKey('C-r', function (ev) {
    command.iSearchBackwardKs(ev);
}, 'Emacs like incremental search backward', true);

key.setGlobalKey(['C-c', 'u'], function (ev) {
    undoCloseTab();
}, 'Undo closed tab');

key.setGlobalKey(['C-c', 'C-c', 'C-v'], function (ev) {
    toJavaScriptConsole();
}, 'Display JavaScript console', true);

key.setGlobalKey(['C-c', 'C-c', 'C-c'], function (ev) {
    command.clearConsole();
}, 'Clear Javascript console', true);

key.setGlobalKey([['C-c', 'e'], ['C-c', 'C-i']], function (ev, arg) {
    ext.exec("edit_text", arg, ev);
}, 'edit by external editor', true);

key.setGlobalKey(['C-c', 'f'], function (ev, arg) {
    ext.exec("hok-start-foreground-mode", arg, ev);
}, 'Start Hit a Hint foreground mode', true);

key.setGlobalKey(['C-c', 'F'], function (ev, arg) {
    ext.exec("hok-start-background-mode", arg, ev);
}, 'Start Hit a Hint background mode', true);

key.setGlobalKey(['C-c', ';'], function (ev, arg) {
    ext.exec("hok-start-extended-mode", arg, ev);
}, 'Start Hit a Hint extended mode', true);

key.setGlobalKey(['C-c', 'C-e'], function (ev, arg) {
    ext.exec("hok-start-continuous-mode", arg, ev);
}, 'Start Hit a Hint continuous mode', true);

key.setGlobalKey(['C-c', 'g', 'u'], function (ev) {
    var uri = getBrowser().currentURI;
    if (uri.path == "/") {
        return;
    }
    var pathList = uri.path.split("/");
    if (!pathList.pop()) {
        pathList.pop();
    }
    loadURI(uri.prePath + pathList.join("/") + "/");
}, 'Go upper directory');

key.setGlobalKey(['C-c', 'g', 'U'], function (ev) {
    var uri = window._content.location.href;
    if (uri == null) {
        return;
    }
    var root = uri.match(/^[a-z]+:\/\/[^/]+\//);
    if (root) {
        loadURI(root, null, null);
    }
}, 'Go to the root directory', true);

key.setGlobalKey(['C-c', 't', 't'], function (ev, arg) {
    ext.exec("twitter-client-tweet", arg);
}, 'Tweet', true);

key.setGlobalKey(['C-c', 't', 'T'], function (ev, arg) {
    ext.exec("twitter-client-tweet-this-page", arg);
}, 'Tweet with the title and URL of this page', true);

key.setGlobalKey(['C-c', 't', 'r'], function (ev, arg) {
    ext.exec("twitter-client-display-timeline", arg);
}, 'Display your timeline', true);
    
key.setGlobalKey(['C-c', 'p'], function (ev, arg) {
    ext.exec("paste--and-go", arg, ev);
}, 'Paste an URL or a search term and Go');
    
    
key.setGlobalKey(['C-c', 'P'], function (ev, arg) {
    ext.exec("paste-to-tab-and-go", arg, ev);
}, 'Paste to new tab and Go');

key.setGlobalKey('C-M-l', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'Select next tab');

key.setGlobalKey('C-M-h', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'Select previous tab');

key.setGlobalKey(['C-o', 'o'], function (ev, arg) {
    shell.input("Open ", arg);
}, 'Open an URL', true);

key.setGlobalKey(['C-o', 't'], function (ev, arg) {
    shell.input("tabopen ", arg);
}, 'Open an URL in new tab', true);

key.setGlobalKey(['C-o', 'f'], function (ev, arg) {
    BrowserOpenFileWindow();
}, 'Open an existing file', true);

key.setGlobalKey(['C-o', 'b'], function (ev, arg) {
    ext.exec("bmany-list-all-bookmarks", arg, ev);
}, 'bmany - List all bookmarks', true);

key.setGlobalKey(['C-o', 'k'], function (ev, arg) {
    ext.exec("bmany-list-all-bookmarks-with-keyword", arg, ev);
}, 'bmany - List bookmarks with keyword', true);

key.setViewKey([['C-n'], ['C-c', 'j']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, 'Scroll line down');

key.setViewKey([['C-c', 'k'], ['C-p']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, 'Scroll line up');

key.setViewKey([['C-c', 'g', 'g'], ['M-<']], function (ev) {
    goDoCommand("cmd_scrollTop");
}, 'Scroll to the top of the page', true);

key.setViewKey(['C-c', 'g', 't'], function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'Select next tab');

key.setViewKey(['C-c', 'g', 'T'], function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'Select previous tab');

key.setViewKey(['C-c', 'g', 'i'], function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, 'Focus to the first textarea', true);

key.setViewKey([['C-c', 'G'], ['M->']], function (ev) {
    goDoCommand("cmd_scrollBottom");
}, 'Scroll to the bottom of the page', true);

key.setViewKey(['C-c', 'r'], function (ev) {
    BrowserReload();
}, 'Reload the page', true);

key.setViewKey(['C-c', 'h'], function (ev) {
    BrowserBack();
}, 'Back');

key.setViewKey(['C-c', 'l'], function (ev) {
    BrowserForward();
}, 'Forward');

key.setViewKey(['C-c', 'i'], function (ev, arg) {
    //stolen from keysnail's vi-style configuration
    children = document.getElementById("nav-bar").children;
    for (i = 0; i < children.length; i++) {
        children[i].style.backgroundColor = "pink";
    }
    util.setBoolPref("accessibility.browsewithcaret", !util.getBoolPref("accessibility.browsewithcaret"));
}, 'Enter caret mode');

key.setViewKey(['C-c', ':'], function (ev, arg) {
    shell.input(null, arg);
}, 'List and execute commands', true);

key.setViewKey(['C-c', 'd'], function (ev) {
    BrowserCloseTabOrWindow();
}, 'Close tab / window');

key.setViewKey(['C-c', 'q'], function (ev, arg) {
    ext.exec("search-selection", arg, ev);
}, 'Search the selection with current default engine');

key.setViewKey(['C-o', 'B'], function (ev, arg) {
    ext.exec("bmany-list-bookmarklets", arg, ev);
}, 'bmany - List all bookmarklets');

key.setViewKey('C-f', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RIGHT, true);
}, 'Scroll right');

key.setViewKey(['C-c', 'z', 'i'], function (ev) {
    FullZoom.enlarge();
}, 'Enlarge text size');

key.setViewKey(['C-c', 'z', 'o'], function (ev) {
    FullZoom.reduce();
}, 'Reduce text size');

key.setViewKey('M-v', function (ev) {
    goDoCommand("cmd_scrollPageUp");
}, 'Scroll page up');

key.setViewKey('C-v', function (ev) {
    goDoCommand("cmd_scrollPageDown");
}, 'Scroll page down');

key.setViewKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all', true);

key.setViewKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, 'Focus to the next button');

key.setViewKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, 'Focus to the previous button');

key.setViewKey(['[', '['], function (ev, arg) {
    ext.exec("previous-page", arg, ev);
}, 'Previous page');

key.setViewKey([']', ']'], function (ev, arg) {
    ext.exec("next-page", arg, ev);
}, 'Next page');

key.setViewKey('M-<down>', function (ev, arg) {
    var backForwardMenu = document.getElementById("backForwardMenu");
    backForwardMenu.openPopupAtScreen(document.width / 2, document.height / 2, true);
}, 'Show page history menu');

key.setEditKey(['C-x', 'h'], function (ev) {
    command.selectAll(ev);
}, 'Select whole text', true);

key.setEditKey([['C-x', 'u'], ['C-_']], function (ev) {
    display.echoStatusBar("Undo!", 2000);
    goDoCommand("cmd_undo");
}, 'Undo');

key.setEditKey(['C-x', 'r', 'd'], function (ev, arg) {
    command.replaceRectangle(ev.originalTarget, "", false, !arg);
}, 'Delete text in the region-rectangle', true);

key.setEditKey(['C-x', 'r', 't'], function (ev) {
    prompt.read("String rectangle: ", function (aStr, aInput) {command.replaceRectangle(aInput, aStr);}, ev.originalTarget);
}, 'Replace text in the region-rectangle with user inputted string', true);

key.setEditKey(['C-x', 'r', 'o'], function (ev) {
    command.openRectangle(ev.originalTarget);
}, 'Blank out the region-rectangle, shifting text right', true);

key.setEditKey(['C-x', 'r', 'k'], function (ev, arg) {
    command.kill.buffer = command.killRectangle(ev.originalTarget, !arg);
}, 'Delete the region-rectangle and save it as the last killed one', true);

key.setEditKey(['C-x', 'r', 'y'], function (ev) {
    command.yankRectangle(ev.originalTarget, command.kill.buffer);
}, 'Yank the last killed rectangle with upper left corner at point', true);

key.setEditKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'Set the mark', true);

key.setEditKey('C-o', function (ev) {
    command.openLine(ev);
}, 'Open line');

key.setEditKey('C-\\', function (ev) {
    display.echoStatusBar("Redo!", 2000);
    goDoCommand("cmd_redo");
}, 'Redo');

key.setEditKey('C-a', function (ev) {
    command.beginLine(ev);
}, 'Beginning of the line');

key.setEditKey('C-e', function (ev) {
    command.endLine(ev);
}, 'End of the line');

key.setEditKey('C-f', function (ev) {
    command.nextChar(ev);
}, 'Forward char');

key.setEditKey('C-b', function (ev) {
    command.previousChar(ev);
}, 'Backward char');

key.setEditKey('M-f', function (ev) {
    command.forwardWord(ev);
}, 'Next word');

key.setEditKey('M-b', function (ev) {
    command.backwardWord(ev);
}, 'Previous word');

key.setEditKey('C-n', function (ev) {
    command.nextLine(ev);
}, 'Next line');

key.setEditKey('C-p', function (ev) {
    command.previousLine(ev);
}, 'Previous line');

key.setEditKey('C-v', function (ev) {
    command.pageDown(ev);
}, 'Page down');

key.setEditKey('M-v', function (ev) {
    command.pageUp(ev);
}, 'Page up');

key.setEditKey('M-<', function (ev) {
    command.moveTop(ev);
}, 'Beginning of the text area');

key.setEditKey('M->', function (ev) {
    command.moveBottom(ev);
}, 'End of the text area');

key.setEditKey('C-d', function (ev) {
    goDoCommand("cmd_deleteCharForward");
}, 'Delete forward char');

key.setEditKey('C-h', function (ev) {
    goDoCommand("cmd_deleteCharBackward");
}, 'Delete backward char');

key.setEditKey('M-d', function (ev) {
    command.deleteForwardWord(ev);
}, 'Delete forward word');

key.setEditKey([['C-<backspace>'], ['M-<delete>']], function (ev) {
    command.deleteBackwardWord(ev);
}, 'Delete backward word');

key.setEditKey('M-u', function (ev, arg) {
    command.wordCommand(ev, arg, command.upcaseForwardWord, command.upcaseBackwardWord);
}, 'Convert following word to upper case');

key.setEditKey('M-l', function (ev, arg) {
    command.wordCommand(ev, arg, command.downcaseForwardWord, command.downcaseBackwardWord);
}, 'Convert following word to lower case');

key.setEditKey('M-c', function (ev, arg) {
    command.wordCommand(ev, arg, command.capitalizeForwardWord, command.capitalizeBackwardWord);
}, 'Capitalize the following word');

key.setEditKey('C-k', function (ev) {
    command.killLine(ev);
}, 'Kill the rest of the line');

key.setEditKey('C-y', command.yank, 'Paste (Yank)');

key.setEditKey('M-y', command.yankPop, 'Paste pop (Yank pop)', true);

key.setEditKey('C-M-y', function (ev) {
    if (!command.kill.ring.length) {
        return;
    }
    let (ct = command.getClipboardText()) (!command.kill.ring.length || ct != command.kill.ring[0]) &&
        command.pushKillRing(ct);
    prompt.selector({message: "Paste:", collection: command.kill.ring, callback: function (i) {if (i >= 0) {key.insertText(command.kill.ring[i]);}}});
}, 'Show kill-ring and select text to paste', true);

key.setEditKey('C-w', function (ev) {
    goDoCommand("cmd_copy");
    goDoCommand("cmd_delete");
    command.resetMark(ev);
}, 'Cut current region', true);

key.setEditKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, 'Focus to the next text area');

key.setEditKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, false, true);
}, 'Focus to the previous text area');

key.setCaretKey([['C-c', 'i'], ['ESC']], function (ev, arg) {
    children = document.getElementById("nav-bar").children;
    for (i = 0; i < children.length; i++) {
        children[i].style.backgroundColor = "transparent";
    }
    util.setBoolPref("accessibility.browsewithcaret", false);
}, 'Exit caret mode');

key.setCaretKey(['C-c', ';'], function (ev) {
    ext.exec("hok-start-extended-mode", ev);
}, 'Start Hit a Hint extended mode');

key.setCaretKey([['C-a'], ['^']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectBeginLine") : goDoCommand("cmd_beginLine");
}, 'Move caret to the beginning of the line');

key.setCaretKey('C-e', function (ev) {
    ext.exec("hok-start-continuous-mode", ev);
}, 'Start Hit a Hint continuous mode');

key.setCaretKey([['$'], ['M->'], ['G']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectEndLine") : goDoCommand("cmd_endLine");
}, 'Move caret to the end of the line');

key.setCaretKey([['C-n'], ['j']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLineNext") : goDoCommand("cmd_scrollLineDown");
}, 'Move caret to the next line');

key.setCaretKey([['C-p'], ['k']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLinePrevious") : goDoCommand("cmd_scrollLineUp");
}, 'Move caret to the previous line');

key.setCaretKey([['C-f'], ['l']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharNext") : goDoCommand("cmd_scrollRight");
}, 'Move caret to the right');

key.setCaretKey([['C-b'], ['h'], ['C-h']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharPrevious") : goDoCommand("cmd_scrollLeft");
}, 'Move caret to the left');

key.setCaretKey([['M-f'], ['w']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordNext") : goDoCommand("cmd_wordNext");
}, 'Move caret to the right by word');

key.setCaretKey([['M-b'], ['W']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordPrevious") : goDoCommand("cmd_wordPrevious");
}, 'Move caret to the left by word');

key.setCaretKey([['C-v'], ['SPC']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPageNext") : goDoCommand("cmd_movePageDown");
}, 'Move caret down by page');

key.setCaretKey([['M-v'], ['b']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPagePrevious") : goDoCommand("cmd_movePageUp");
}, 'Move caret up by page');

key.setCaretKey([['M-<'], ['g']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectTop") : goDoCommand("cmd_scrollTop");
}, 'Move caret to the top of the page');

key.setCaretKey('J', function (ev) {
    util.getSelectionController().scrollLine(true);
}, 'Scroll line down');

key.setCaretKey('K', function (ev) {
    util.getSelectionController().scrollLine(false);
}, 'Scroll line up');

key.setCaretKey(',', function (ev) {
    util.getSelectionController().scrollHorizontal(true);
    goDoCommand("cmd_scrollLeft");
}, 'Scroll left');

key.setCaretKey('.', function (ev) {
    goDoCommand("cmd_scrollRight");
    util.getSelectionController().scrollHorizontal(false);
}, 'Scroll right');

key.setCaretKey('z', function (ev) {
    command.recenter(ev);
}, 'Scroll to the cursor position');

key.setCaretKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'Set the mark', true);

key.setCaretKey(':', function (ev, arg) {
    shell.input(null, arg);
}, 'List and execute commands', true);

key.setCaretKey('R', function (ev) {
    BrowserReload();
}, 'Reload the page', true);

key.setCaretKey('B', function (ev) {
    BrowserBack();
}, 'Back');

key.setCaretKey('F', function (ev) {
    ext.exec("hok-start-background-mode", ev);
}, 'Start Hit a Hint background mode');

key.setCaretKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all', true);

key.setCaretKey('f', function (ev) {
    ext.exec("hok-start-foreground-mode", ev);
}, 'Start Hit a Hint foreground mode');

key.setCaretKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, 'Focus to the next button');

key.setCaretKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, 'Focus to the previous button');


key.setViewKey(['C-c', '*'], function (ev, arg) {
    ext.exec("next-occur", arg, ev);
}, 'highlight next occurence of current selected word');

key.setViewKey(['C-c', '#'], function (ev, arg) {
    ext.exec("previous-occur", arg, ev);
}, 'highlight previous occurence of current selected word');

key.setViewKey(["C-x", 'b'], function (ev, arg) {
    ext.exec("tanything", arg);
}, "view all tabs", true);



key.setViewKey(['C-c', 'C-a'], function (ev, arg) {
    ext.exec('increase-digit-in-url', arg, ev);
}, 'Increase last digit in the URL (go to next page)');

key.setViewKey(['C-c', 'C-d'], function (ev, arg) {
    ext.exec('decrease-digit-in-url', arg, ev);
}, 'Decrease last digit in the URL (go to prev page)');

key.setEditKey(["C-x", '8', "'"], function(ev, arg) {
    inputChars(ev, "「");
}, "Input 「");

key.setEditKey(["C-x", '9', "'"], function(ev, arg) {
    inputChars(ev, "」");
}, "Input  」 ");

key.setEditKey(["C-x", '8', '"'], function(ev, arg) {
    inputChars(ev, "『");
}, "Input 『");

key.setEditKey(["C-x", '9', '"'], function(ev, arg) {
    inputChars(ev, "』");
}, "Input 』");

key.setEditKey(["C-x", '8', '.'], function(ev, arg) {
    inputChars(ev, "·");
}, "Input ·");

key.setEditKey(["C-x", '8', '-'], function(ev, arg) {
    inputChars(ev, "…");
}, "Input …");

key.setGlobalKey(["<C-f10>", 'p'], function(ev, arg) {
    toggleproxy.toggleProxy();
}, "Toggle proxy.");

key.setGlobalKey(["<C-f10>", 'c'], function(ev, arg) {
    addon9408.ctmain.btDoToggle();
}, "Toggle color.");

key.setGlobalKey(['C-<f10>', 'd'], function (ev, arg) {
    var val = util.getBoolPref("extensions.gdic_gtrans.enableSelect", false);
    val = !val;
    util.setBoolPref("extensions.gdic_gtrans.enableSelect", val);
    var wm = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator);
    var enumerator = wm.getEnumerator("navigator:browser");
    while (enumerator.hasMoreElements()) {
        var win = enumerator.getNext();
        if (win && win.gd12) {
            win.gd12.prefs.read();
        }
    }
    display.echoStatusBar("select-to-translate now set to " + val , 3000);
}, 'Toggle \'select-to-translate\' of Wiktionary & Google Translate extension.');

key.setGlobalKey(['C-<f10>', 'D'], function (ev, arg) {
    var prefkey = "extensions.gdic_gtrans.enableDoubleClick";
    var val = util.getBoolPref(prefkey, false);
    val = !val;
    util.setBoolPref(prefkey, val);
    var wm = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator);
    var enumerator = wm.getEnumerator("navigator:browser");
    while (enumerator.hasMoreElements()) {
        var win = enumerator.getNext();
        if (win && win.gd12) {
            win.gd12.prefs.read();
        }
    }
    display.echoStatusBar("double-click-to-translate now set to " + val , 3000);
}, 'Toggle \'double-click-to-translate\' of Wiktionary & Google Translate extension.');


key.setViewKey('M-=', function(ev, arg) {
    ext.exec("count-region", arg, ev);
}, "Count selected or all chars in current editbox.");

key.setGlobalKey(['<f5>', 't'], function (ev, arg) {
    if (typeof(listAllTabsMenu) != "undefined")
        listAllTabsMenu.onCtrlTabKeycommand();  //List All Tabs Menu extension
    else
        allTabs.open();
}, 'select tab in of current tab group(firefox 4+)');

key.setGlobalKey(['<f5>', 'T'], function (ev, arg) {
    gPano.pane.toggleOpen();
}, 'select tab (pano extension)');
    
    
key.setGlobalKey(['<f5>', 'b'], function(ev, arg) {
    ext.exec('bmany-list-all-bookmarks', arg, ev);
}, 'bmany - List all bookmarks.');

key.setGlobalKey(['<f5>', 'B'], function(ev, arg) {
    ext.exec('bmany-list-toolbar-bookmarks', arg, ev);
}, 'bmany - List toolbar bookmarks.');

key.setGlobalKey(['C-<f11>', 'm'], function(ev, arg) {
    SplitBrowser._browsers.forEach(function(aBrowser) {
        if (!aBrowser.contentCollapsed)
            aBrowser.collapse();
        else
            aBrowser.expand(true);
    });
}, "Expand/collapse subbrowsers (SplitBrowser addon)");

key.setGlobalKey(['<f9>', 'w'], function(ev, arg) {
    ext.exec('wiktionary-lookup-selection', arg, ev);
}, "Translation selection with Wiktionary & Google Translate.");
