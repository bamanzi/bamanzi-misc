
Run jslint on the command line fast (with v8 engine)
================================================

jslint-v8 is a modern and extreamly fast runner for the popular jslint
JavaScript style checker. [jslint][] is implemented by the JavaScript guru
Douglas Crockford in JavaScript itself.

Credits
--------
* this work was inspired by [jsbeautify][] implementation.
* original jslint-v8 implementation by [jlbfalcao][]
* vim support and Rakefile by [Vladimir Dobriakov][] AKA [geekQ][]


Build
-----

[Download][v8 doc] and compile v8 JavaScript engine according official
manual as shared library. On my 64bit Ubuntu Linux I have used:

    scons library=shared arch=x64

Copy the v8 shared library to /usr/lib or whatever place is appropriate
for your OS:

    sudo cp libv8.so /usr/lib

Compile jslint-v8

    V8_BASEDIR=/your/path/to/v8-trunk rake compile

If you do not have ruby/rake you can run g++ directly, please look
inside Rakefile.


Run on console
--------------

    jslint --browser file.js

checks the style for `file.js`. You can provide all the known jslint
switches on the command line. Here `--browser` indicates that e.g.
XMLHttpRequest object should be allowed.


Run from vim
------------

Set up `jslint` as make program in .vimrc:

    autocmd BufRead,BufNewFile *.js,*.json setlocal makeprg=jslint\ --vim\ \%

`--vim` provides error message formatting suitable for parsing in vim.

Now you can check your JavaScript easily with `:make` or even
automatically on every file save or load.

See a screenshot for [usage inside vim][screenshot].

[v8 doc]: http://code.google.com/apis/v8/build.html
[jsbeautify]: http://blog.slashpoundbang.com/post/2488598258/running-javascript-from-the-command-line-with-v8
[jlbfalcao]: https://github.com/jlbfalcao/jslint-v8
[Vladimir Dobriakov]: http://www.mobile-web-consulting.de
[jslint]: http://www.jslint.com/
[geekQ]: http://www.geekQ.net/
[screenshot]: http://www.mobile-web-consulting.de/post/4745654954/run-jslint-fast-v8-engine-for-vim
