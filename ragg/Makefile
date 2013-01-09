doc:
	scribble  ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/   --dest-name index.html manual.scrbl

web: doc
	scp -r * hashcollision.org:webapps/htdocs/ragg/
