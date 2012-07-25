
.PHONY: clean dummy install

dummy:
	echo Usage:
	echo sudo make install

clean:
	rm -rf *~
	rm jshint.js
  
jshint.js:
	wget http://jshint.com/jshint.js

install: clean jshint.js
	cp jshint    /usr/local/bin/jshint
	cp jshint.js /usr/local/bin/jshint.js
