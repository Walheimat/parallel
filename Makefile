PACKAGE_NAME=parallel

include dinghy/emacs-package.mk

# -- Utility

.PHONY: update-version
update-version:
	$(UPDATE_VERSION) Cask
	$(UPDATE_VERSION) $(PACKAGE_NAME).el
