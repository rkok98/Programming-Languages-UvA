
JFLAGS = -g
JAVAC  = javac
JAVA   = java

JAVAS   = $(shell find ./ -type f -name '*.java')
CLASSES = $(patsubst %.java,%.class,$(JAVAS))

all: 	$(CLASSES)


# Makefile pattern to generate a *.class file for each *.java file
%.class: %.java
	$(JAVAC) $(FLAGS) $<


# Generate *.dot file for all java classes
diagrams_dot:
	cd ./individual && ./generate_diagrams.sh;

DOTS = $(shell find ./ -type f -name '*.dot')
PDFS = $(patsubst %.dot,%.pdf,$(DOTS))

diagrams: 	$(PDFS)

# Makefile pattern to generate a *.pdf file for each *.dot file
%.pdf: %.dot
	dot -Tpdf $< > $@


TESTS_JAVAS   = $(shell find ./tests/ -type f -name 'Test_*.java')
TESTS_CLASSES = $(patsubst %.java,%.class,$(TESTS_JAVAS))

test:	$(CLASSES)
	@for test in $(TESTS_CLASSES) ; do \
	  echo "##### running test: $$test" ;\
          dir=`dirname $$test` ;\
	  base=`basename $$test` ;\
	  file=`echo $${base%.*}` ;\
	  $(JAVA) -classpath ./:$$dir $$file ;\
	done

doc:
	mkdir -p doc
	javadoc -d doc $(JAVAS)

doc_clean:
	rm -rf doc


clean:  doc_clean
	$(RM) $(CLASSES)
	$(RM) $(DOTS)
	$(RM) $(PDFS)
