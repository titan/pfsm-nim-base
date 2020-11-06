include .config
NAME-LINK=$(subst _,-,$(NAME))

PKGPREFIX=Pfsm
TARGET=$(BUILDDIR)/build/ttc/$(PKGPREFIX)/Nim.ttc
SRCS=$(wildcard $(PKGPREFIX)/*.idr)
DSTSRCS=$(addprefix $(BUILDDIR)/$(PKGPREFIX)/, $(notdir $(SRCS)))
DSTSRCS+=$(BUILDDIR)/$(PKGPREFIX)/Nim.idr
PRJCONF=$(NAME-LINK).ipkg

all: $(TARGET)

install: $(TARGET)
	cd $(BUILDDIR); idris2 --install $(PRJCONF); cd -

$(TARGET): $(SRCS:%=$(BUILDDIR)/%) $(BUILDDIR)/$(PRJCONF) | prebuild
	cd $(BUILDDIR); idris2 --build $(PRJCONF); cd -

$(SRCS): %: $(BUILDDIR)/% | prebuild
	cp $< $@

$(BUILDDIR)/%.idr: %.idr | prebuild
	cp $< $@

$(BUILDDIR)/$(PRJCONF): $(PRJCONF) | prebuild
	cp $< $@

prebuild:
ifeq "$(wildcard $(BUILDDIR)/$(PKGPREFIX))" ""
	@mkdir -p $(BUILDDIR)/$(PKGPREFIX)
endif

clean:
	@rm -rf $(BUILDDIR)

.PHONY: all clean install prebuild .config
