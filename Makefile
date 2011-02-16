
SHELL:=/bin/bash

build:
	lein compile | grep -v 'at clojure.'

test:
	lein test | grep -v 'at clojure.'

run:
	lein run $(n)

todo:
	grep 'TODO' -R */ || true
	grep 'FIXME' -R */ || true
	grep 'XXX' -R */ || true

clean: pkg-clean

PROJNAME := mccormack_t_HW3
PACKAGE_FILE := $(PROJNAME).tar.gz
PKGDIR := pkg/$(PROJNAME)

pkg: pkg-clean
	mkdir -p $(PKGDIR)/
	cp README-CS4300 $(PKGDIR)/README.txt
	cp -r src/ $(PKGDIR)/src
	cp project.clj deploy/{run,setup}.sh $(PKGDIR)/
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

.PHONY: pkg build test run todo deploy

.SILENT:

