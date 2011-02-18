default: jar

test jar::
	lein $@

push: clean
	lein jar
	ln -s clojurejs-*.jar clojurejs.jar
	lein push

clean:
	rm -f *.jar
