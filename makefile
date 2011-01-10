default:
	lein jar

push: clean
	lein jar
	ln -s clojurejs-*.jar clojurejs.jar
	lein push

clean:
	rm -f *.jar
