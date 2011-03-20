key.setGlobalKey(['C-x', '0'], function (ev, arg) {
    SplitBrowser.activeBrowserCloseWindow();
}, 'Close current active "window" (Fox Splitter addon)');

key.setGlobalKey(['C-x', '1'], function (ev, arg) {
    var url = SplitBrowser.activeBrowser != gBrowser ? SplitBrowser.activeSubBrowser.src : null;

    var browsers = SplitBrowser.browsers;
    for (var i = 0; i < browsers.length; ++i)
        browsers[i].close();

    if (url) window.loadURI(url);
}, 'close-other-window (Fox Splitter addon)');

key.setGlobalKey(['C-x', '2'], function (ev, arg) {
    SplitBrowser.addSubBrowser(window.content.location.href,
                               SplitBrowser.activeSubBrowser,
                               SplitBrowser.POSITION_BOTTOM);
}, 'split-window-vertically (Fox Splitter addon)');

key.setGlobalKey(['C-x', '3'], function (ev, arg) {
    SplitBrowser.addSubBrowser(window.content.location.href,
                               SplitBrowser.activeSubBrowser,
                               SplitBrowser.POSITION_RIGHT);
}, 'split-window-horizontally (Fox Splitter addon)');

key.setGlobalKey(['C-x', 'k'], function (ev) {
    if ( typeof(SplitBrowser) == "undefined" ) {
        BrowserCloseTabOrWindow();
    }
    else
    {
        var b = SplitBrowser.activeBrowser;
        if (b.mTabs.length > 1) {
            b.removeTab(b.mCurrentTab);
        } else if (b === gBrowser) {
            gBrowser.removeTab(gBrowser.mCurrentTab);
        }
   }  
}, 'Close tab (or \'window\', if SplitBrowser addon installed)');

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    function focusSubBrowserById(aId) {
        SplitBrowser.getSubBrowserById(aId).browser.contentWindow.focus();
    }

    var browsers = SplitBrowser.browsers;

    if (SplitBrowser.activeBrowser === gBrowser) {
        focusSubBrowserById(browsers[(arg == null) ? 0 : browsers.length - 1].id);
        return;
    }

    var id = SplitBrowser.activeSubBrowser.id;

    for (var i = 0; i < browsers.length; i++) {
        if (browsers[i].id == id)
            break;
    }

    var nextIndex = (arg == null) ? i + 1 : i - 1;
    if (nextIndex >= browsers.length || nextIndex < 0)
        gBrowser.contentWindow.focus();
    else
        focusSubBrowserById(browsers[nextIndex].id);
}
, 'other-window (Fox Splitter addon)', true);

key.setGlobalKey(['C-x', 'K'], function (ev) {
    closeWindow(true);
}, 'Close the window');

key.setGlobalKey(['C-x', '5', '0'], function (ev) {
    closeWindow(true);
}, 'Close the window');