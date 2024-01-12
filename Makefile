PACKAGE_NAME=partial-recall
PACKAGE_SUFFIX=tar
CURRENT_PACKAGE_VERSION=0.9.2
UPDATE_VERSION_FILES=Cask \
					 partial-recall.el \
					 partial-recall-menu.el \
                     partial-recall-x.el

PACIFY_DEPS=partial-recall.el \
			partial-recall-menu.el \
			partial-recall-x.el

include dinghy/emacs-package.mk
