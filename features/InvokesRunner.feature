Feature: Invokes Runner
  As an emacs user
  I want a feature mode that easily invokes the bdd runner
  So that I can quickly test my changes

  Scenario: Uses rake when Rakefile present
    Given a file "Rakefile" with:
      """
      require 'cucumber/rake/task'
      Cucumber::Rake::Task.new
      """
    And a file "Gemfile" does not exist
    And an empty file "features/test.feature"
    When I run `emacs --batch -l ../../feature-mode.el features/test.feature --eval '(progn (feature-mode) (feature-verify-all-scenarios-in-buffer) (set-buffer "*compilation*") (princ (buffer-string)))'`
    Then the output should match:
      """
      ^rake cucumber CUCUMBER_OPTS="" FEATURE=".+test.feature"
      """

  Scenario: Uses cucumber when no Rakefile present
    Given a file "Rakefile" does not exist
    And a file "Gemfile" does not exist
    And an empty file "features/test.feature"
    When I run `emacs --batch -l ../../feature-mode.el features/test.feature --eval '(progn (feature-mode) (feature-verify-all-scenarios-in-buffer) (set-buffer "*compilation*") (princ (buffer-string)))'`
    Then the output should match:
      """
      ^cucumber .+test.feature
      """
