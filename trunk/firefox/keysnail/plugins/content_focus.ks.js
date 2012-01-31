var PLUGIN_INFO =
<KeySnailPlugin>
  <name>content focus</name>
  <name lang="ja">content focus</name>
  <description>focus to content with onload event</description>
  <version>0.0.1</version>
  <updateURL>https://github.com/basyura/content-focus/raw/master/content_focus.ks.js</updateURL>
  <iconURL>http://github.com/basyura/content-focus/raw/master/icon.png</iconURL>
  <author mail="basyura@gmail.com" homepage="http://github.com/basyura/content-focus">basyura</author>
  <license>The MIT License</license>
  <license lang="ja">MIT ライセンス</license>
  <minVersion>1.8.8</minVersion>
  <include>main</include>
  <provides>
  </provides>
	<options>
		<option>
			<name>content_focus.white_list</name>
			<type>array</type>
			<description>white list</description>
			<description lang="ja">content focus しない url のリスト</description>
		</option>
		<option>
			<name>content_focus.delay_time</name>
			<type>integer</type>
			<description>delay time[ms] to execute. : default 100[ms]</description>
			<description lang="ja">実行までの遅延時間 [ms] : default 100[ms]</description>
		</option>
	</options>
  <detail><![CDATA[

=== 使い方 ===

>||
plugins.options["content_focus.white_list"] = [
  'http://java.sun.com/j2se/',
  '^http://www.livedoor'
];
||<
	
    ]]></detail>
</KeySnailPlugin>;

gBrowser.addEventListener('load' , function(event) {
    let doc = event.originalTarget;
    if (doc != gBrowser.contentDocument) {
      return;
    }
    var white_list = plugins.options["content_focus.white_list"]
    if (white_list) {
      var url = doc.location.href;
      for (var i = 0 , max = white_list.length ; i < max  ; i++) {
        if(url.match(new RegExp(white_list[i]))) {
          return;
        }
      }
    }
    var delay = plugins.options["content_focus.delay_time"] || 100;
    setTimeout(function () {
        let ele = document.commandDispatcher.focusedElement;
        if (ele) {
          ele.blur();
        }
        gBrowser.focus();
        content.focus();
      } , delay);
  } , true);
