PKG_DIR = feature-mode-0.4
FILES_TO_PACK = feature-mode.el feature-mode-pkg.el i18n.yml snippets support

package:
	mkdir $(PKG_DIR)
	cp -R $(FILES_TO_PACK) $(PKG_DIR)
	tar -cf $(PKG_DIR).tar $(PKG_DIR)

clean:
	rm -r $(PKG_DIR)
	rm $(PKG_DIR).tar
