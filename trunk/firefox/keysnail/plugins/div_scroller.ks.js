var PLUGIN_INFO =
<KeySnailPlugin>
<name>Div Scroller</name>
<description lang="ja">スクロールができる div 要素などでスクロールする</description>
<license>Creative Commons 2.1 (Attribution + Share Alike)</license>
<minVersion>1.8.0</minVersion>
<author>958</author>
<version>0.0.1</version>
<detail><![CDATA[
=== 流用元 ===
scroll_div.js at master from vimpr/vimperator-plugins - GitHub
https://github.com/vimpr/vimperator-plugins/blob/master/scroll_div.js
]]></detail>
</KeySnailPlugin>;

(function () {

  // スクロール可能か？
  function isScrollable (elem) {
    const re = /auto|scroll/i;
    let s = elem.ownerDocument.defaultView.getComputedStyle(elem, '');
    if (elem.scrollHeight <= elem.clientHeight)
      return false;
    return ['overflow', 'overflowY', 'overflowX'].some(function (n)
      s[n] && re.test(s[n]));
  }

  // 光らせる
  function flashElement (elem) {
    let indicator = elem.ownerDocument.createElement('div');
    let rect = elem.getBoundingClientRect();
    indicator.id = 'nyantoro-element-indicator';
    let style = 'background-color: blue; opacity: 0.5; z-index: 999;' +
                'position: fixed; ' +
                'top: ' + rect.top + 'px;' +
                'height:' + elem.clientHeight + 'px;'+
                'left: ' + rect.left + 'px;' +
                'width: ' + elem.clientWidth + 'px';
    indicator.setAttribute('style', style);
    elem.appendChild(indicator);
    setTimeout(function () elem.removeChild(indicator), 500);
  }

  // スクロール可能な要素のリストを返す
  function scrollableElements () {
    let result = [];
    let doc = content.document;
    let r = doc.evaluate('//div|//ul', doc, null, 7, null)
    for (let i = 0, l = r.snapshotLength; i < l; i++) {
      let elem = r.snapshotItem(i);
      if (isScrollable(elem))
        result.push(elem);
    }
    return result;
  }

  // スクロール対象を変更
  function shiftScrollElement (n) {
    let doc = content.document;
    let idx = doc.__div_scroller_index || 0;
    let es = scrollableElements();
    if (es.length <= 0)
      display.echoStatusBar('scrollable element not found');
    idx += (n || 1);
    if (idx < 0)
      idx = es.length - 1;
    if (idx >= es.length)
      idx = 0;
    content.document.__div_scroller_index = idx;
    flashElement(es[idx]);
  }

  // 現在のスクロール対象を返す
  function currentElement () {
    let es = scrollableElements();
    let idx = content.document.__div_scroller_index || 0;
    return es[idx];
  }

  // スクロールする
  function scroll (down) {
    let elem = currentElement();
    if (elem)
      elem.scrollTop += Math.max(30, elem.clientHeight - 20) * (down ? 1 : -1);
  }

  plugins.withProvides(function (provide) {
    provide("div-scroller-scroll-down",
        function (ev, arg) {
            scroll(true, typeof arg == 'number' ? arg : 1);
        },
        "Scroll down");

    provide("div-scroller-scroll-up",
        function (ev, arg) {
            scroll(false, typeof arg == 'number' ? arg : 1);
        },
        "Scroll up");

    provide("div-scroller-shift-scroll-element-inclement",
        function (ev, arg) {
            shiftScrollElement(1);
        },
        "Shift scroll element");

    provide("div-scroller-shift-scroll-element-declement",
        function (ev, arg) {
            shiftScrollElement(-1);
        },
        "Shift scroll element");
  }, PLUGIN_INFO);

})();