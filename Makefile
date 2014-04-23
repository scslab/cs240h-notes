
SOURCES := $(sort $(wildcard */*.md))
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
		topic=$$(sed -ne '1s/^% *//p' $$file); \
		if test -z "$$topic"; then \
			echo Missing \"% topic\" as first line of $$file >&2; \
			exit 1; \
		fi; \
		echo "* [$$topic]($$html)" >> $@~; \
	done
	mv -f $@~ $@

#sed -ne "1{s/^% */* [/; s|\$$|]($$html)|p;}" $$file >> $@~; \

clean:
	rm -f *~ .*~ */*~ $(TARGETS) index.{md,html}
.PHONY: clean

update:
	git pull
	$(MAKE) all
.PHONY: update
