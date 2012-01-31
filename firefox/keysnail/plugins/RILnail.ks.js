// Info

let PLUGIN_INFO =
<KeySnailPlugin>
    <name>RILnail</name>
    <description>RIL from KeySnail</description>
    <description lang="ja">RIL 拡張と連携</description>
    <author>958</author>
    <iconURL>https://sites.google.com/site/958site/Home/files/RILnail.ks.png</iconURL>
    <updateURL>https://gist.github.com/raw/895703/RILnail.ks.js</updateURL>
    <version>0.1.10</version>
    <license>MIT</license>
    <minVersion>1.8.0</minVersion>
    <include>main</include>
    <detail lang="ja"><![CDATA[
=== 使い方 ===
Firefox 拡張 Read It Later が必須です
https://addons.mozilla.org/ja/firefox/addon/read-it-later/

HoK 拡張ヒントモードを使って、リンクをRITに追加することも

キーマップを変更したい人は、次のような設定を .keysnail.js の PRESERVE エリアへ

>||
plugins.options["ril.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",
    "j"     : "prompt-next-completion",
    "k"     : "prompt-previous-completion",
    "g"     : "prompt-beginning-of-candidates",
    "G"     : "prompt-end-of-candidates",
    "D"     : "prompt-cancel",
    // RILnail specific actions
    "o"     : "open,c",
    "O"     : "open-background,c",
    "d"     : "delete"
};
||<
]]></detail>
</KeySnailPlugin>;

// Option

let pOptions = plugins.setupOptions("ril", {
    "list_sort_type" : {
        preset: 1,
        description: M({
            en: "List sort type 0:ASC by date 1:DESC by date (Default: 1)",
            ja: "リストの並び順 0:日付昇順 1:日付降順 (Default: 1)"})
    },
    "hok_add_ril" : {
        preset: 'l',
        description: M({
            en: "Key bound to `Append RIL` in the HoK extended hint mode (Default: l)",
            ja: "HoK 拡張ヒントモードにおいて `リンクを RIL に追加` へ割り当てるキー (デフォルト: l)"})
    },
    "hok_add_ril_multi" : {
        preset: 'L',
        description: M({
            en: "Key bound to `Append RIL multiple hints` in the HoK extended hint mode (Default: L)",
            ja: "HoK 拡張ヒントモードにおいて `連続してリンクを RIL に追加` へ割り当てるキー (デフォルト: L)"})
    },
    "keymap" : {
        preset: {
            "C-z"   : "prompt-toggle-edit-mode",
            "j"     : "prompt-next-completion",
            "k"     : "prompt-previous-completion",
            "g"     : "prompt-beginning-of-candidates",
            "G"     : "prompt-end-of-candidates",
            "D"     : "prompt-cancel",
            // RILnail specific actions
            "o"     : "open,c",
            "O"     : "open-background,c",
            "d"     : "delete"
        },
        description: M({
            ja: "メイン画面の操作用キーマップ",
            en: "Local keymap for manipulation"
        })
    }
}, PLUGIN_INFO);

// Extend HoK

hook.addToHook('PluginLoaded', function () {
    if (!plugins.hok)
        return;

    let AddRIL = function (elem) {
        if (elem) {
            if (!elem)
              throw "Not on a link";
            var link = RIL.bubbleToTagName(elem, 'A');
            var title = RIL.APP.stripTags(link.innerHTML);
            if (title.length == 0) {
              if (link.firstChild) {
                //If it doesn't have content then it's probably an image link, so check the image for a title or alt label first
                if (link.firstChild.title.length > 0) {
                  title = link.firstChild.title;
                } else if (link.firstChild.alt.length > 0) {
                  title = link.firstChild.title;
                }
              }
            }
            RIL.saveLink(link.href, title, RIL.xul('clickToSaveTags').value);
            display.echoStatusBar("Append RIL - " + title, 3000);
        }
    };

    plugins.hok.addActions([
        // append RIL
        [pOptions["hok_add_ril"],
         M({ja: "リンクを RIL に追加", en: "Append RIL"}),
         AddRIL],
        // append RIL multiple hints
        [pOptions["hok_add_ril_multi"],
         M({ja: "連続してリンクを RIL に追加", en: "Append RIL multiple hints"}),
         AddRIL, false, true]
    ]);
});

// Add ext

plugins.withProvides(function (provide) {
    provide("ril-append",
        function (ev, arg) {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (!item) {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append RIL - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "RIL - 現在のタブを RIL に追加", en: "RIL - Append current tab"}));
    provide("ril-remove",
        function (ev, arg) {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (item) {
                RIL.markCurrentAsRead()
                display.echoStatusBar("Remove RIL - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "RIL - 現在のタブを RIL から削除", en: "RIL - Remove current tab"}));
    provide("ril-toggle",
        function (ev, arg) {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (item) {
                RIL.markCurrentAsRead()
                display.echoStatusBar("Remove RIL - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            } else {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append RIL - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
        },
        M({ja: "RIL - 現在のタブを RIL に追加 または 削除", en: "RIL - Append or remove current tab"}));
    provide("ril-append-and-close",
        function (ev, arg) {
            let item = RIL.APP.LIST.itemByUrl(window.content.location.href);
            if (!item) {
                let itemId = RIL.addCurrent();
                display.echoStatusBar("Append RIL - " + window.content.document.wrappedJSObject.title.toString() || "", 3000);
            }
            if (arg || !item) {
                gBrowser.removeTab(gBrowser.mCurrentTab);
            }
        },
        M({ja: "RIL - 現在のタブを RIL に追加してタブを閉じる", en: "RIL - Append current tab and close"}));
    provide("ril-show-reading-list",
        function (ev, arg) {
            let collection = [];
            for (let i = 0; i < RIL.APP.LIST.list.length; i++) {
                let url = RIL.APP.LIST.list[i].url;
                let favicon = util.getFaviconPath(url);
                collection.push([
                    favicon,
                    html.unEscapeTag(RIL.APP.LIST.list[i].title),
                    url,
                    toAgo(RIL.APP.LIST.list[i].timeUpdated * 1000),
                    RIL.APP.LIST.list[i].timeUpdated,
                    RIL.APP.LIST.list[i].itemId]);
            }
            let sortType = pOptions['list_sort_type']
            collection.sort(function(a,b){
               return (sortType == 0) ? ((a[4] < b[4]) ? 1 : -1) : ((a[4] < b[4]) ? -1 : 1);
            });
            prompt.selector(
                {
                    message    : "pattern:",
                    collection : collection,
                    flags      : [ICON | IGNORE, 0, 0, 0, HIDDEN | IGNORE, HIDDEN | IGNORE],
                    style      : [style.prompt.description, style.prompt.url],
                    header     : ["Title", "Url", "Date"],
                    width      : [45, 45, 10],
                    keymap     : pOptions["keymap"],
                    actions    : [
                        [function (aIndex, items) {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 openUILinkIn(url, "tab");
                             }
                         },
                         M({ja: '選択中アイテムを新しいタブで開く',
                            en: "Open in new tab"}),
                         "open"],
                        [function (aIndex, items) {
                             if (items.length > 0 && aIndex >= 0) {
                                 let url = items[aIndex][2];
                                 openUILinkIn(url, "tabshifted");
                             }
                         },
                         M({ja: '選択中アイテムを新しいバックグラウンドタブで開く',
                            en: "Open in background new tab"}),
                         "open-background"],
                        [function (aIndex, items) {
                             if (items.length > 0 && aIndex >= 0)
                                 removeItem(items, aIndex, true);
                         },
                         M({ja: '選択中アイテムを削除する',
                            en: "Delete"}),
                         "delete,c"]
                    ]
                }
            );
            function removeItem(items, aIndex, deep) {
                 if (deep) {
                     RIL.APP.LIST.mark(items[aIndex][5], true);
                     RIL.APP.LIST.endBatchAndRefresh();
                 }
                 items.splice(aIndex, 1);
                 if (items.length == 0)
                     prompt.finish();
                 else
                     prompt.refresh();
            }
        },
        M({ja: "RIL - リストを表示", en: "RIL - Show reading list"}));
}, PLUGIN_INFO);


// Util

// timeago: a jQuery plugin
// http://timeago.yarp.com/
function toAgo(time){
	var strings = {
		suffixAgo: "ago",
		suffixFromNow: "from now",
		seconds: "less than a minute",
		minute: "about a minute",
		minutes: "%d minutes",
		hour: "about an hour",
		hours: "about %d hours",
		day: "a day",
		days: "%d days",
		month: "about a month",
		months: "%d months",
		year: "about a year",
		years: "%d years"
	};
	var $l = strings;
	var prefix = $l.prefixAgo;
	var suffix = $l.suffixAgo;

	var distanceMillis = new Date().getTime() - time;

	var seconds = distanceMillis / 1000;
	var minutes = seconds / 60;
	var hours = minutes / 60;
	var days = hours / 24;
	var years = days / 365;

	function substitute(string, number) {
		var value = ($l.numbers && $l.numbers[number]) || number;
		return string.replace(/%d/i, value);
	}

	var words = seconds < 45 && substitute($l.seconds, Math.round(seconds)) ||
		seconds < 90 && substitute($l.minute, 1) ||
		minutes < 45 && substitute($l.minutes, Math.round(minutes)) ||
		minutes < 90 && substitute($l.hour, 1) ||
		hours < 24 && substitute($l.hours, Math.round(hours)) ||
		hours < 48 && substitute($l.day, 1) ||
		days < 30 && substitute($l.days, Math.floor(days)) ||
		days < 60 && substitute($l.month, 1) ||
		days < 365 && substitute($l.months, Math.floor(days / 30)) ||
		years < 2 && substitute($l.year, 1) ||
		substitute($l.years, Math.floor(years));

	return [prefix, words, suffix].join(" ").trim();
}
