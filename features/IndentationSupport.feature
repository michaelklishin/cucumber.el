Feature: Indentation support
  As an emacs user
  I need a feature mode that properly indents Gherkin
  So that I can more easily write feature files
  
  Scenario: Indents tables
    Given a file "tables.feature" with:
      """
      Feature:
      Scenario Outline: Table indentation support
      Given a table:
      | column |
      | data 1 |
      And another table:
      | column |
      | data 2 |
      And examples with <param1> and <param2>
      When the file is indented
      Then the tables are indented under their step or examples heading
      Examples:
      | param1 | param2 |
      | foo    | bar    |
      | baz    | bat    |
      """
    When I run `emacs --batch -l ../../feature-mode.el tables.feature --eval "(progn (feature-mode) (indent-region (point-min) (point-max) nil) (princ (buffer-string)))"`
    Then the output should contain:
      """
      Feature:
        Scenario Outline: Table indentation support
          Given a table:
            | column |
            | data 1 |
          And another table:
            | column |
            | data 2 |
          And examples with <param1> and <param2>
          When the file is indented
          Then the tables are indented under their step or examples heading
          Examples:
            | param1 | param2 |
            | foo    | bar    |
            | baz    | bat    |
      """

  @wip
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

  @wip
  @tag1
  @tag2
  Scenario: Tag support
    Given a scenario prefixed with some tags
    When the file is indented
    Then the tags are at the same indent level as the scenario
