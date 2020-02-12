
JFLAGS = -g
JAVAC  = javac
JAVA   = java

# Hier worden alle variabelen aangemaakt die van belang zijn.

HC_LOGS = $(shell find ./team/hc-logs -type f -name '*.*')
SA_LOGS = $(shell find ./team/sa-logs -type f -name '*.*')
PDFS    = $(shell find ./team/plots -type f -name '*.pdf')
JAVAS   = $(shell find ./ -type f -name '*.java')
CLASSES = $(patsubst %.java,%.class,$(JAVAS))

# Onderstaande bestanden zijn voor het testscript dat er al stond.

all: 	$(CLASSES)

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

# Rules below are added and don't belong to the testscript.
# These rules execute the hc files in order.

hc: hc_statistics
	./team/hc_plots.sh

hc_statistics: hc_experiments
	./team/hc_statistics.sh

hc_experiments:
	./team/hc_experiments.sh

# These rules execute the sa files in order.

sa: sa_statistics
	./team/sa_plots.sh

sa_statistics: sa_experiments
	./team/sa_statistics.sh

sa_experiments:
	./team/sa_experiments.sh

# This last rule removes all generated files.

clean: doc_clean
	$(RM) $(CLASSES)
	rm -rf $(HC_LOGS)
	rm -rf $(SA_LOGS)
	rm -rf $(PDFS)


