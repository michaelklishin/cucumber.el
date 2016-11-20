Feature: Indentation support
  As an emacs user
  I need a feature mode that properly indents Gherkin
  So that I can more easily write feature files

Scenario: Custom scenario indent-level
  Given "feature-indent-level" set to 0
  When the file is indented
  Then the scenarios are at the same level as the feature

Scenario: Disable back-dent mode
  Given "feature-enable-back-denting" is set to "nil"
  When I press tab on a line
  And I press tab on the same line
  Then it keeps the line's indentation at the max

Scenario: Pystring support
  Given a pystring:
    """
    example pystring 1
    """
  And another pystring:
    """
    example pystring 2
    """
  When the file is indented
  Then the pystrings are indented under their steps

Scenario: Step table support
  Given a table:
    | column |
    | data 1 |
  And another table:
    | column |
    | data 2 |
  When the file is indented
  Then the tables are indented under their steps

@tag1
@tag2
Scenario: Tag support
  Given a scenario prefixed with some tags
  When the file is indented
  Then the tags are at the same indent level as the scenario

Scenario Outline: Examples table support
  Given a scenario with examples <param1> and <param2>
  When the file is indented
  Then the example table is indented under "Examples:"

  Examples:
    | param1 | param2 |
    | foo    | bar    |
    | baz    | bat    |
