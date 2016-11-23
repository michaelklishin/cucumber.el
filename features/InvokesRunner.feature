Feature: Invokes Runner
  As an emacs user
  I want a feature mode that easily invokes the bdd runner
  So that I can quickly test my changes

  Scenario: Uses rake when Rakefile present
    Given an empty file "Rakefile"
    And a file "Gemfile" does not exist
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^rake cucumber CUCUMBER_OPTS="" FEATURE=".+test.feature"
      """

  Scenario: Uses cucumber when no Rakefile present
    Given a file "Rakefile" does not exist
    And a file "Gemfile" does not exist
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^cucumber .+test.feature
      """

  Scenario: Uses bundler exec cucumber when Gemfile present but no Rakefile
    Given a file "Rakefile" does not exist
    And an empty file "Gemfile"
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^bundle exec cucumber .+test.feature
      """

  Scenario: Uses bundler exec rake when Gemfile and Rakefile present
    Given an empty file "Rakefile"
    And an empty file "Gemfile"
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^bundle exec rake cucumber CUCUMBER_OPTS="" FEATURE=".+test.feature"
      """
