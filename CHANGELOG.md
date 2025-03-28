Emacs Feature Mode Change Log
=============================

Version 0.6.1 <2025-03-28 Fri>
-----------------------------

* Use newer translations from Gherkin.
* Rework font locking.
* Support the "Rule" keyword.

Version 0.6 <2025-03-27 Thu>
-----------------------------

* Upgrade dependencies (Ruby 3, Gherkin 27, Cucumber 9).
* Adapt and fix `feature-goto-step-definition`.

Version 0.5 (skipped)
------------------------------

* Added setting `feature-rake-command`, and changed the default
  setting of `feature-cucumber-command` to be to run just cucumber.
* Changed behavior of running cucumber to run `feature-rake-command`
  if a Rakefile is present, otherwise `feature-cucumber-command`.
* Prefix command run with `bundle exec ` when Gemfile is present.
* Support for latest version cucumber gem
