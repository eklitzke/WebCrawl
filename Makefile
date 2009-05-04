crawl:
	ghc --make -threaded crawl.hs

clean:
	-find . -regex '.*\.\(hi\|o\)' -delete
	-rm -f crawl

.PHONY: crawl clean
