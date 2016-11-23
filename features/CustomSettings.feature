Feature: Custom Settings
  As an emacs user
  I need a feature mode that allows custom settings
  So that I can adjust its behavior to my preferences

  @wip
  Scenario: Custom scenario indent-level
    Given "feature-indent-level" set to 0
    When the file is indented
    Then the scenarios are at the same level as the feature

  @wip
  Scenario: Disable back-dent mode
    Given "feature-enable-back-denting" is set to "nil"
    When I press tab on a line
    And I press tab on the same line
    Then it keeps the line's indentation at the max
