.PHONY: all assembly run clean

SBT = $(shell which sbt)
ifeq ($(SBT),)
	$(error "Uou need 'sbt' somewhere; grab a launcher from https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt")
endif

all: assembly

assembly:
	$(SBT) assembly

run:
	$(SBT) 'runMain org.spurint.slox.Lox $(SCRIPT)'

clean:
	$(SBT) clean
