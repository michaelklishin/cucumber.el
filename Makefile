PKG_DIR = feature-mode-0.6.1
FILES_TO_PACK = feature-mode.el feature-mode-pkg.el gherkin-languages.json snippets support

package:
	mkdir $(PKG_DIR)
	cp -R $(FILES_TO_PACK) $(PKG_DIR)
	tar -cf $(PKG_DIR).tar $(PKG_DIR)

clean::
	rm -rf $(PKG_DIR)
	rm -f $(PKG_DIR).tar

test::
	bundle exec cucumber -t 'not @wip'

clean::
	rm -rf tmp

distclean::
	rm -f *~ */*~

sh:
	docker compose run --rm app bash
