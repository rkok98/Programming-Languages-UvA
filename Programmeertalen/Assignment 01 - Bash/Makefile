
JFLAGS = -g
JAVAC  = javac
JAVA   = java

JAVAS   = $(shell find ./ -type f -name '*.java')
CLASSES = $(patsubst %.java,%.class,$(JAVAS))

all: 	$(CLASSES)


# Makefile pattern to generate a *.class file for each *.java file
%.class: %.java
	$(JAVAC) $(FLAGS) $<


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

diagrams_dot:
	cd ./individual && ./generate_diagrams.sh;
