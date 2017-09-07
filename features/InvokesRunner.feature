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

  Scenario: Uses docker-compose with rake when docker-compose.yml and Rakefile present
    Given an empty file "Rakefile"
    Given an empty file "docker-compose.yml"
    And a file "Gemfile" does not exist
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^docker-compose run app rake cucumber CUCUMBER_OPTS="" FEATURE="features/test.feature"
      """

  Scenario: Uses docker-compose when docker-compose.yml present but no Gemfile
    Given a file "Rakefile" does not exist
    And a file "Gemfile" does not exist
    And an empty file "docker-compose.yml"
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^docker-compose run app cucumber\s*\"features/test.feature\"
      """

  Scenario: Uses docker-compose with bundler exec cucumber when Gemfile and docker-compose.yml present but no Rakefile
    Given a file "Rakefile" does not exist
    And an empty file "Gemfile"
    And an empty file "docker-compose.yml"
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^docker-compose run app bundle exec cucumber\s*\"features/test.feature\"
      """

  Scenario: Uses docker-compose with bundler exec rake when Gemfile and docker-compose.yml and Rakefile present
    Given an empty file "Rakefile"
    And an empty file "Gemfile"
    And an empty file "docker-compose.yml"
    And an empty file "features/test.feature"
    When I invoke feature-verify-all-scenarios-in-buffer on "features/test.feature"
    Then the output should match:
      """
      ^docker-compose run app bundle exec rake cucumber CUCUMBER_OPTS="" FEATURE="features/test.feature"
      """
