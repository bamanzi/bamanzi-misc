// Info

let PLUGIN_INFO =
<KeySnailPlugin>
    <name>Find</name>
    <description>Find from KeySnail</description>
    <description lang="ja">ページ内やすべてのタブをテキスト検索</description>
    <updateURL>https://gist.github.com/raw/905297/find.ks.js</updateURL>
    <iconURL>https://sites.google.com/site/958site/Home/files/find.ks.png</iconURL>
    <author>958</author>
    <version>0.1.9</version>
    <license>MIT</license>
    <minVersion>1.8.0</minVersion>
    <include>main</include>
    <detail lang="ja"><![CDATA[
=== 使い方 ===
カレントタブやすべてのタブ内のテキストなどを検索します
上記エクステ入力後、promptに検索文字列を入力してください

XUL/Migemo がインストールされていると、 Migemo による検索を行います

=== コマンド ===
勝手に「find」という組み込みコマンドを追加します

>||
find word [-l | -lt | -lu] [*]

 word: 検索文字列

 -l: リンクテキストとリンクのURLを検索
 -lt: リンクテキストを検索
 -lu: リンク URL を検索

 *: 全てのタブを検索
||<

以下のような感じで使ってください

カレントタブのテキストを検索
>||
find hoge
||<

カレントタブのリンクテキストを検索
>||
find hoge -lt
||<

カレントタブのリンク URL を検索
>||
find hoge -lu
||<

カレントタブのリンクテキストと URL を検索
>||
find hoge -l
||<

全てのタブのテキストを検索
>||
find hoge *
||<

全てのタブのリンクテキストを検索
>||
find hoge -lt *
||<

全てのタブのリンク URL を検索
>||
find hoge -lu *
||<

全てのタブのリンクテキストと URL を検索
>||
find hoge -l *
||<

=== 設定 ===
キーバインドを変更したい場合は、次のような設定を .keysnail.js の PRESERVE エリアへ
>||
plugins.options['find.keymap'] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "SPC"   : "prompt-next-page",
    "b"     : "prompt-previous-page",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "q"     : "prompt-cancel",
};
||<
]]></detail>
</KeySnailPlugin>;

// Option

let pOptions = plugins.setupOptions("find", {
    "keymap": {
        preset: {
            "C-z"   : "prompt-toggle-edit-mode",
            "SPC"   : "prompt-next-page",
            "b"     : "prompt-previous-page",
            "j"     : "prompt-next-completion",
            "k"     : "prompt-previous-completion",
            "g"     : "prompt-beginning-of-candidates",
            "G"     : "prompt-end-of-candidates",
            "q"     : "prompt-cancel",
        },
        description: M({
            ja: "メイン画面の操作用キーマップ",
            en: "Local keymap for manipulation"
        })
    },
    'text_pre_pos_length': {
        preset: 30,
        description: M({
            ja: "テキスト検索時に、前後の文脈を省略する際の文字数 (初期値: 30)",
            en: "Number of characters when context when text about is retrieved is omitted (default: 30)"
        })
    },
    'command': {
        preset: 'find',
        description: M({
            ja: "追加するコマンド (初期値: find)",
            en: "Added command (default: find)"
        })
    },
    'result_view_widths': {
        preset: [80, 20],
        description: M({
            ja: "検索結果における文字列とタブ名の幅 (初期値: [80, 20])",
            en: "Proportion of search result text and tab name (default: [80, 20])"
        })
    }
}, PLUGIN_INFO);

// Add ext

const MAX_PRE_POST_TEXT_LEN = pOptions['text_pre_pos_length'];

let gPrompt = {
    get visible() !document.getElementById("keysnail-prompt").hidden,
    focus  : function () {
        document.getElementById("keysnail-prompt-textbox").focus();
    },
    forced : false,
    close  : function () {
        if (!gPrompt.forced)
            return;

        gPrompt.forced = false;

        if (gPrompt.visible)
            prompt.finish(true);
    }
};

function makeKeywordRegExp(word)
    (window.xulMigemoCore) ?
        new RegExp(window.xulMigemoCore.getRegExp(word), "ig") : new RegExp(word, "ig");

function grepText(word, isAll) {
    let _nsIFind = Cc['@mozilla.org/embedcomp/rangefind;1'].getService(Ci.nsIFind);
    _nsIFind.findBackwards = false;
    _nsIFind.caseSensitive = true;

    let _wordExp = makeKeywordRegExp(word);

    let _collection = [];

    if (isAll) {
        let tabs = gBrowser.mTabContainer.childNodes;
        for (let i = 0; i < tabs.length; i++) {
            Array.prototype.push.apply(_collection, _grepTargetWindow(tabs[i].linkedBrowser.contentWindow, i));
        }
    } else {
        _collection = _grepTargetWindow(window.content, gBrowser.mTabContainer.selectedIndex);
    }
    if (_collection.length > 0)
        _showSelector();

    function _grepTargetWindow(win, tabIndex){
        let collection = _grep(win, tabIndex);
        for (let i = 0; i < win.frames.length; i++) {
            if (win.frames[i])
                Array.prototype.push.apply(collection, _grepTargetWindow(win.frames[i], tabIndex));
        }
        return collection;
    }

    function _grep(win, tabIndex) {
        let doc = win.document;
        let title = doc.title;
        let favicon = util.getFaviconPath(win.location.href);

        let collection = [];

        $X('//text()[not(ancestor::script or ancestor::style or ancestor::aside or ancestor::head)]|//input[not(@hidden)]|//textarea', doc).forEach(function(node)
        {
            let text = (node.nodeType == 3) ? node.textContent : node.value;

            if (!_wordExp.test(text)) return;

            let arResult = text.match(_wordExp);

            if (!arResult || arResult.length == 0) return;
            let findRange = doc.createRange();
            findRange.selectNode(node);

            let startPt = doc.createRange();
            startPt.selectNode(node);
            startPt.collapse(true);

            let endPt = doc.createRange();
            endPt.selectNode(node);
            endPt.collapse(false);

            try{
                for (let i = 0, offset = 0; i < arResult.length; i++) {
                    let rsltRange = _nsIFind.Find(arResult[i], findRange, startPt, endPt);
                    if (rsltRange == null)
                        break;

                    let desc = (function(){
                        let ret;
                        let orgText = text;
                        if (orgText.length > MAX_PRE_POST_TEXT_LEN * 2) {
                            let sttOver = (rsltRange.startOffset > MAX_PRE_POST_TEXT_LEN);
                            let endOver = (orgText.length - rsltRange.endOffset > MAX_PRE_POST_TEXT_LEN);
                            ret = (sttOver ? '... ' : '') +
                                orgText.substring(
                                    sttOver ? rsltRange.startOffset - MAX_PRE_POST_TEXT_LEN : 0,
                                    endOver ? rsltRange.endOffset + MAX_PRE_POST_TEXT_LEN : orgText.length) +
                                (endOver ? ' ...' : '');
                        } else {
                            ret = orgText;
                        }
                        return ret;
                    })();
                    collection.push([desc, favicon, title, tabIndex, rsltRange]);
                    offset = rsltRange.endOffset;
                    startPt.setStart(node, offset);
                    startPt.setEnd(node, offset);
                }
            }catch(e){ }

            endPt.detach();
            startPt.detach();
            findRange.detach();
        });

        return collection;
    }

    function _showSelector() {
        prompt.selector({
            message     : "["+_collection.length+"] "+"pattern:",
            collection  : _collection,
            flags       : [0, ICON | IGNORE, 0, IGNORE | HIDDEN, IGNORE | HIDDEN],
            style       : [style.prompt.description],
            header      : ["Text", 'Tab'],
            width       : pOptions['result_view_widths'],
            keymap      : pOptions["keymap"],
            onChange    : function (arg) {
                if (!arg.row) return;

                if (gBrowser.mTabContainer.selectedIndex != arg.row[3]) {
                    gBrowser.mTabContainer.selectedIndex = arg.row[3];
                    gPrompt.focus();
                }
                try {
                    selector.selectAndScroll(arg.row[4]);
                } catch (e) {
                    display.echoStatusBar(M({ja: '非表示テキストです', en: 'Not visible'}), 3000);
                }
            },
            onFinish   : function() {
                selector.destroy();
                for (let i = 0; i < _collection.length; i++)
                    _collection[i][4].detach();
                _collection = null;
            },
            actions    : [
                [
                    function (index) {
                        let doc = _collection[index][4].startContainer.ownerDocument;
                        let nodes = $X(
                            'ancestor-or-self::a | ancestor-or-self::input | ancestor-or-self::textarea | ancestor-or-self::*[@onclick or @onmouseover or @onmousedown or @onmouseup or @oncommand or @role="link"]',
                            _collection[index][4].startContainer);
                        if (nodes.length > 0) {
                            if (plugins.hok)
                                plugins.hok.followLink(nodes[0], 4);
                            else
                                nodes[0].focus();
                        }
                    }, "Open or Focus", "Open-or-focus"
                ]
            ]
        });
    }
}

function grepLink(word, isAll, isTitle, isURL) {
    let _wordExp = makeKeywordRegExp(word);

    let _collection = [];

    if (isAll) {
        let tabs = gBrowser.mTabContainer.childNodes;
        for (let i = 0; i < tabs.length; i++) {
            Array.prototype.push.apply(_collection, _grepTargetWindow(tabs[i].linkedBrowser.contentWindow, i));
        }
    } else {
        _collection = _grepTargetWindow(window.content, gBrowser.mTabContainer.selectedIndex);
    }
    if (_collection.length > 0)
        _showSelector();

    function _grepTargetWindow(win, tabIndex){
        let collection = _grep(win, tabIndex);
        for (let i = 0; i < win.frames.length; i++) {
            if (win.frames[i])
                Array.prototype.push.apply(collection, _grepTargetWindow(win.frames[i], tabIndex));
        }
        return collection;
    }

    function _grep(win, tabIndex) {
        let doc = win.document;
        let title = doc.title;
        let favicon = util.getFaviconPath(win.location.href);

        let collection = [];

        function mapText(node) {
            let text = node.textContent;
            if (_wordExp.test(text)) {
                let range = doc.createRange();
                range.selectNodeContents(node);
                collection.push([text || node.href, node.href, favicon, title, tabIndex, range]);
            }
        }

        function mapURL(node) {
            let url = node.href;
            if (_wordExp.test(url)) {
                let range = doc.createRange();
                range.selectNodeContents(node);
                collection.push([node.textContent || url, url, favicon, title, tabIndex, range]);
            }
        }

        function mapTextAndURL(node) {
            let target = node.textContent + ' ' + node.href;
            if (_wordExp.test(target)) {
                let range = doc.createRange();
                range.selectNodeContents(node);
                collection.push([node.textContent || node.href, node.href, favicon, title, tabIndex, range]);
            }
        }

        let fn;
        if (isTitle && isURL)   fn = mapTextAndURL;
        else if (isURL)         fn = mapURL;
        else                    fn = mapText;

        Array.slice(doc.querySelectorAll('a[href]')).forEach(fn);

        return collection;
    }

    function _showSelector() {
        prompt.selector({
            message     : "["+_collection.length+"] "+"pattern:",
            collection  : _collection,
            flags       : [0, 0, ICON | IGNORE, 0, IGNORE | HIDDEN, IGNORE | HIDDEN],
            style       : [style.prompt.description, style.prompt.url, style.prompt.description],
            header      : ["Text", "URL", 'Tab'],
            width       : [
                pOptions['result_view_widths'][0] / 2,
                pOptions['result_view_widths'][0] / 2,
                pOptions['result_view_widths'][1],
            ],
            keymap      : pOptions["keymap"],
            onChange    : function (arg) {
                if (!arg.row) return;

                if (gBrowser.mTabContainer.selectedIndex != arg.row[4]) {
                    gBrowser.mTabContainer.selectedIndex = arg.row[4];
                    gPrompt.focus();
                }
                try {
                    selector.selectAndScroll(arg.row[5]);
                } catch(e) {
                    display.echoStatusBar(M({ja: '非表示リンクです', en: 'Not visible'}), 3000);
                }
            },
            onFinish    : function() {
                for (let i = 0; i < _collection.length; i++)
                    _collection[i][5].detach();
                selector.destroy();
                _collection = null;
            },
            actions     : [
                [
                    function (index) {
                        let node = _collection[index][5].startContainer;
                        if (plugins.hok)
                            plugins.hok.followLink(node, 4);
                        else
                            node.focus();
                    }, "Open or Focus", "Open-or-focus"
                ]
            ]
        });
    }
};

plugins.withProvides(function (provide) {
    function checkArgAndGo(arg, fn, mustWord, args) {
        if (typeof arg === 'string' && (!mustWord || arg.length > 0))
            fn(arg, args);
        else
            prompt.read("Find:", function (word) {
                if (!mustWord || word)
                    fn.apply(this, [word].concat(args || []));
            }, null, null, null, 0);
    }

    provide("find-current-tab",
        function (ev, arg) {
            checkArgAndGo(arg, grepText, true);
        },
        M({ja: "Find - 現在のタブを検索", en: "Find - find current tab"}));

    provide("find-all-tab",
        function (ev, arg) {
            checkArgAndGo(arg, grepText, true, [true]);
        },
        M({ja: "Find - 全てのタブを検索", en: "find - find all tab"}));

    provide("find-current-tab-link-text",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [false, true]);
        },
        M({ja: "Find - 現在のタブのリンクテキストを検索", en: "Find - find current tab link text"}));

    provide("find-all-tab-link-text",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [false, true]);
        },
        M({ja: "Find - 全てのタブのリンクテキストを検索", en: "Find - find all tab link text"}));

    provide("find-current-tab-link-url",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [false, false, true]);
        },
        M({ja: "Find - 現在のタブのリンク URL を検索", en: "Find - find current tab link url"}));

    provide("find-all-tab-link-url",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [true, false, true]);
        },
        M({ja: "Find - 全てのタブのリンク URL を検索", en: "Find - find all tab link url"}));

    provide("find-current-tab-link-text-and-url",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [false, true, true]);
        },
        M({ja: "Find - 現在のタブのリンクテキストと URL を検索", en: "Find - find current tab link text and url"}));

    provide("find-all-tab-link-text-and-url",
        function (ev, arg) {
            checkArgAndGo(arg, grepLink, false, [true, true, true]);
        },
        M({ja: "Find - 全てのタブのリンクテキストと URL を検索", en: "Find - find all tab link text and url"}));
}, PLUGIN_INFO);

shell.add([pOptions['command']], "find-text-or-link",
    function(args, extra) {
        function paramParser(arg) {
            const _param = ['*', '-l', '-lu', '-lt'];

            const _paramToCommand = {
                'find-current-tab'                  : [false, false, false, false],
                'find-all-tab'                      : [true,  false, false, false],
                'find-current-tab-link-text'        : [false, false, false, true ],
                'find-all-tab-link-text'            : [true,  false, false, true ],
                'find-current-tab-link-url'         : [false, false, true,  false],
                'find-all-tab-link-url'             : [true,  false, true,  false],
                'find-current-tab-link-text-and-url': [false, true,  false, false],
                'find-all-tab-link-text-and-url'    : [true,  true,  false, false]
            };

            let flags = [false, false, false, false];
            for (let i = 1; i < arg.length; i++) {
                for (let p = 0; p < _param.length; p++) {
                    if (arg[i] == _param[p]) {
                        flags[p] = true;
                        break;
                    }
                }
            }

            let ret = '';
            for (let i in _paramToCommand) {
                let result = true;
                for (let j = 0; j < flags.length; j++) {
                    if (flags[j] != _paramToCommand[i][j]) {
                        result = false;
                        break;
                    }
                }
                if (result == true) {
                    ret = i;
                }
            }
            return ret;
        }
        let cmd;
        if (args.length == 1) {
            cmd = "find-current-tab";
        } else {
            cmd  = paramParser(args);
        }
        ext.exec(cmd, args[0]);
    },
    { argCount: '*' }, true
);

let selector = {
    _lastSelection: null,

    // via XULMigemo
    _getSelectionController: function(aTarget)
        (aTarget instanceof Ci.nsIDOMNSEditableElement) ?
            aTarget
                .QueryInterface(Ci.nsIDOMNSEditableElement)
                .editor
                .selectionController
            :
            aTarget
                .QueryInterface(Ci.nsIInterfaceRequestor)
                .getInterface(Ci.nsIWebNavigation)
                .QueryInterface(Ci.nsIDocShell)
                .QueryInterface(Ci.nsIInterfaceRequestor)
                .getInterface(Ci.nsISelectionDisplay)
                .QueryInterface(Ci.nsISelectionController),

    selectAndScroll: function(range) {
        if (selector._lastSelection)
            selector._lastSelection.removeAllRanges();

        let elem = range.startContainer;
        let win = elem.ownerDocument.defaultView;
        let target = $X('ancestor::textarea | ancestor::input', elem)[0] || win;
        let sc = selector._getSelectionController(target);

        try{
            sc.setDisplaySelection(sc.SELECTION_ATTENTION);
            sc.repaintSelection(sc.SELECTION_NORMAL);
        }catch(e){ }

        let selection = sc.getSelection(sc.SELECTION_NORMAL);
        selector._lastSelection = selection;

        selection.addRange(range);

        let selPrivate = (Ci.nsISelection2) ?
            selection.QueryInterface(Ci.nsISelection2) : selection.QueryInterface(Ci.nsISelectionPrivate);

        selPrivate.scrollIntoView(
            Ci.nsISelectionController.SELECTION_ANCHOR_REGION,
                true, 50, 50);
    },

    destroy: function(range) {
        if (selector._lastSelection)
            selector._lastSelection.removeAllRanges();
        selector._lastSelection = null;
    }
};

function $X(exp, context, resolver, result_type) {
    context || (context = document);
    var Doc = context.ownerDocument || context;
    var result = Doc.evaluate(exp, context, resolver, result_type || XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
    if (result_type) return result;
    for (var i = 0, len = result.snapshotLength, res = new Array(len); i < len; i++) {
        res[i] = result.snapshotItem(i);
    }
    return res;
}
