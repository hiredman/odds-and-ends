.PHONY: fetch split
.SUFFIXES: .mov .url .html

fetch:
	 fetch http://images.apple.com/trailers/home/rss/newtrailers.rss

urls: fetch
	 egrep -o "<link>.*</link>" newtrailers.rss|cut -d '>' -f 2|cut -d '<' -f 1 > urls

split: urls
	for i in `cat urls`; do echo $${i} > `echo $${i}| cut -d \/ -f 6`.url; done

videos:
	mkdir videos

.url.html:
	fetch -o $(.TARGET:S/mov/html/) `cat $(.IMPSRC)`/hd

.html.mov:
	fetch -o $(.TARGET) `egrep  -o "[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+" $(.IMPSRC)\
	  |grep 1080p.mov|tail -1|cut -d \" -f 1|sed "s/1080p/h1080p/"`
