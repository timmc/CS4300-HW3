
SHELL:=/bin/bash

build:
	lein compile | grep -v 'at clojure.'

test:
	lein test | grep -v 'at clojure.'

run:
	lein run

reflections:
	lein clean
	lein compile | sed 's|Reflection warning, ||' | sed "s| can't be resolved\\.||"

todo:
	grep 'TODO' -nR */ || true
	grep 'FIXME' -nR */ || true
	grep 'XXX' -nR */ || true

clean: pkg-clean

PROJNAME := mccormack_t_HW3
PACKAGE_FILE := $(PROJNAME).tar.gz
PKGDIR := pkg/$(PROJNAME)

pkg: pkg-clean
	mkdir -p $(PKGDIR)/
	cp README-CS4300 $(PKGDIR)/README.txt
	cp -r src/ $(PKGDIR)/src
	cp -r test/ $(PKGDIR)/test
	cp -r doc/ $(PKGDIR)/doc
	cp project.clj deploy/{run,setup}.sh $(PKGDIR)/
	find ./pkg -name '*~' -delete
	tar -czf $(PACKAGE_FILE) --directory pkg/ $(PROJNAME)/

pkg-clean:
	rm -f $(PROJNAME).tar.gz
	rm -rf pkg
	lein clean

CCIS_MACHINE := timepilot.ccs.neu.edu

clean-deploy:
	ssh $(CCIS_MACHINE) 'rm -rf ~/private/CS4300-deploy'

deploy-ready: clean-deploy
	ssh $(CCIS_MACHINE) 'ls -ld ~/private/ | grep drwx------ &>/dev/null'
	ssh $(CCIS_MACHINE) 'mkdir -p ~/private/CS4300-deploy'

deploy: pkg deploy-ready
	scp $(PACKAGE_FILE) deploy/test.sh '$(CCIS_MACHINE):~/private/CS4300-deploy/'
	ssh $(CCIS_MACHINE) 'cd ~/private/CS4300-deploy && tar -xzf $(PACKAGE_FILE)'

test-deploy: deploy
	ssh -X $(CCIS_MACHINE) '~/private/CS4300-deploy/test.sh'

.PHONY: pkg build test run todo deploy doc

.SILENT:

