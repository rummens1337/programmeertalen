
JFLAGS = -g
JAVAC  = javac
JAVA   = java

# Hier worden alle variabelen aangemaakt die van belang zijn.

HC_LOGS = $(shell find ./team/hc-logs -type f -name '*.*')
SA_LOGS = $(shell find ./team/sa-logs -type f -name '*.*')
JAVAS   = $(shell find ./ -type f -name '*.java')
CLASSES = $(patsubst %.java,%.class,$(JAVAS))

# Generates the hill climbing files in order.
hc: 
	javac Main.java
	./team/hc_experiments.sh
	./team/hc_statistics.sh
	./team/hc_plots.sh

# Generates the simulated annealing files in order.
sa:
	javac Main.java
	./team/sa_experiments.sh
	./team/sa_statistics.sh
	./team/sa_plots.sh

# Removes all generated files.
clean: doc_clean
	$(RM) $(CLASSES)
	rm -rf $(HC_LOGS)
	rm -rf $(SA_LOGS)

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


