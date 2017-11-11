.PHONY: all assembly run clean test

SBT = $(shell which sbt)
ifeq ($(SBT),)
	$(error "You need 'sbt' somewhere; grab a launcher from https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt")
endif

all: assembly

assembly:
	$(SBT) assembly

run:
	$(SBT) 'runMain org.spurint.slox.Lox $(SCRIPT)'

test:
	./run_tests.sh

clean:
	$(SBT) clean
