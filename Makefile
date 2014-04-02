
SOURCES := $(wildcard */*.md)
TARGETS := $(patsubst %.md, %.html, $(SOURCES))

all: index.html $(TARGETS)
.PHONY: all

%.html: %.md $(MAKEFILE_LIST)
	pandoc -s -t html -o $@ $<

index.md: $(SOURCES) $(MAKEFILE_LIST)
	rm -f $@~
	echo '% CS240h notes' > $@~
	echo '' >> $@~
	for file in $(SOURCES); do \
		html=$$(echo $$file | sed -e 's/\.md$$/.html/'); \
		sed -ne "1{s/^% */* [/; s|\$$|]($$html)|p;}" $$file >> $@~; \
	done
	mv -f $@~ $@

clean:
	rm -f *~ .*~ */*~ $(TARGETS) index.{md,html}
.PHONY: clean

update:
	git pull
	$(MAKE) all
.PHONY: update
