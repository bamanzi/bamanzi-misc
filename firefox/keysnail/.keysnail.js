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
    SplitBrowser.addSubBrowser(window.content.location.href, SplitBrowser.activeSubBrowser, SplitBrowser.POSITION_BOTTOM);
}, 'split-window-vertically (Fox Splitter addon)');

ext.add("split-window-horizontally", function() {
    SplitBrowser.addSubBrowser(window.content.location.href, SplitBrowser.activeSubBrowser, SplitBrowser.POSITION_RIGHT);
}, 'split-window-horizontally (Fox Splitter addon)');

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
    if (result.responseText) {
        command.setClipboardText(result.responseText);
        display.echoStatusBar("Short URL copied into clipboard: " + result.responseText, 3000);
    }
    else
        display.echoStatusBar("is.gd service failed: " + result.statusText, 3000);
}, "Shorten current page's URL with http://is.gd service");


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
               || (links[i].text = '< 前一页')
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
               || (links[i].text == '后一页 >')
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

ext.add("next-occur", function() {
    var word = getBrowserSelection();
    if (word) {
        gFindBar._findField.value = word;
        gFindBar._highlightDoc(true, word);
    }
    gFindBar.onFindAgainCommand(false);
}, 'highlight next occurence of current selected word');

ext.add("previous-occur", function() {
    var word = getBrowserSelection();
    if (word) {
        gFindBar._findField.value = word;
        gFindBar._highlightDoc(true, word);
    }
    gFindBar.onFindAgainCommand(true);
}, 'highlight previous occurence of current selected word');

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

key.setGlobalKey(['C-x', 'k'], function (ev) {
    ext.exec("delete-window", arg, ev);
}, 'Close tab (or \'window\', if Fox Splitter addon installed)');

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    ext.exec("other-window", arg, ev);
}, 'other-window (Fox Splitter addon)', true);

key.setGlobalKey([['C-x', 'K'], ['C-x', '5', '0']], function (ev) {
    closeWindow(true);
}, 'Close the window');

key.setGlobalKey(['C-x', 'n'], function (ev) {
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
    ext.exec("next-occur", ev);
}, 'highlight next occurence of current selected word');

key.setViewKey(['C-c', '#'], function (ev, arg) {
    ext.exec("previous-page", ev);
}, 'highlight previous occurence of current selected word');

key.setViewKey(["C-x", 'b'], function (ev, arg) {
    ext.exec("tanything", arg);
}, "view all tabs", true);

key.setGlobalKey(['C-x', 'p'], function (ev, arg) {
    splitpannel.toggle(window._content.document.location, true, 'right');
}, 'Open Split Panel and load current URL in it .');

key.setGlobalKey(['C-x', 'i'], function (ev, arg) {
    splitpannel.toggle('http://space.cnblogs.com/mi/', true, 'right');
}, 'Open Split Panel and load http://space.cnblogs.com/mi/ in it .');

key.setGlobalKey(['C-x', 't'], function (ev, arg) {
    splitpannel.toggle("http://translate.google.com/", true, 'right');
}, 'Open Split Panel and load http://translate.google.com/ in it .');
