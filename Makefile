PKG_DIR = feature-mode-0.4
FILES_TO_PACK = feature-mode.el feature-mode-pkg.el i18n.yml snippets support

package:
	mkdir $(PKG_DIR)
	cp -R $(FILES_TO_PACK) $(PKG_DIR)
	tar -cf $(PKG_DIR).tar $(PKG_DIR)

clean::
	rm -rf $(PKG_DIR)
	rm -f $(PKG_DIR).tar

test::
	bundle exec cucumber -t ~@wip

clean::
	rm -rf tmp

distclean::
	rm -f *~ */*~
