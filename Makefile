VERSION=$(shell grep -P -o '(?<=define-package "pdee" ")[0-9.]+' pdee-pkg.el)
RELEASEDIR=pdee-$(VERSION)
BUILDDIR=build/$(RELEASEDIR)

release: clean package

all:
	@echo "What do you want from me?"

tag:
	$(shell git tag $(VERSION))

clean:
	-@ rm -rf build

package:
	-@ mkdir -p $(BUILDDIR)
	-@ cp *.el $(BUILDDIR)
	-@ cp ChangeLog CONTRIBUTORS COPYING INSTALL README NEWS $(BUILDDIR)
	-@ cp -r doc extensions python-libs python-modes scripts snippets $(BUILDDIR)
	-@ cd build && tar -cf pdee-$(VERSION).tar $(RELEASEDIR)
