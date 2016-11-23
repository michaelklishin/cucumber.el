Emacs Feature Mode Change Log
=============================

Version 0.5 (not yet released)
------------------------------

* Added setting `feature-rake-command`, and changed the default
  setting of `feature-cucumber-command` to be to run just cucumber.
* Changed behavior of running cucumber to run `feature-rake-command`
  if a Rakefile is present, otherwise `feature-cucumber-command`.
* Prefix command run with `bundle exec ` when Gemfile is present.
