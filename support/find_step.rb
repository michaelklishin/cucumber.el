# frozen_string_literal: true

require 'rubygems'

gem 'ruby_parser'
gem 'cucumber-gherkin'
gem 'cucumber'

require 'ruby_parser'
require 'yaml'
require 'gherkin'
require 'cucumber'

# Extracts step information
class StepExtractor
  def initialize(ast)
    @ast = ast
  end

  # Returns a Hash describing step at the line or nil if nothing
  # executable found
  def step_at(line)
    @ast.feature.children.each do |element|
      node = element.scenario || element.background
      next unless node

      node.steps.each do |step|
        next unless step.location.line == line

        return step_result(step, node)
      end
    end
    nil
  end

  # Return hash of step info
  def step_result(step, node)
    result = { 'name' => step.text }
    return result unless node.respond_to?(:examples) && node&.examples

    return result if node.examples[0].nil?

    add_example_info(node, result)
  end

  # Adds extracted example info to a result
  def add_example_info(node, result)
    headers = example_headers(node)
    result['examples'] = []
    node.examples[0].table_body.each do |row|
      example = {}
      row.cells.each_with_index { |cell, i| example[headers[i]] = cell.value }
      result['examples'] << example
    end
    result
  end

  # Returns an array for example table headers
  def example_headers(node)
    node.examples[0].table_header.cells.map(&:value)
  end
end

# Class to represent a step
class Step
  attr_reader :file, :line, :regexp

  def initialize(regexp, file, line)
    @file = file
    @line = line
    self.regexp = regexp
  end

  def regexp=(value)
    @regexp = convert_to_regexp(value)
  end

  # Converts a given value to a RegExp
  def convert_to_regexp(value)
    case value
    when String
      create_regexp(value)
    when Regexp
      value
    else
      warn "Warning: #{file}:#{line} expected Regexp or String, got "\
        "#{value.class} #{value.inspect}"
      Regexp.new(/^INVALID PARAM$/)
    end
  end

  # Return a regexp created from a string
  def create_regexp(value)
    pieces = []
    regexp = value.dup
    regexp.gsub!(/\$\w+/) do |match|
      pieces << match
      'TOKEN'
    end
    regexp.gsub!(/(?=\{)(.*?)(\})/, 'TOKEN')
    regexp = Regexp.escape(regexp)
    regexp.gsub!(/TOKEN/) { |_| '(.*)' }
    Regexp.new("^#{regexp}$")
  end

  def match?(text)
    @regexp.match(text)
  end
end

# Step parser
class StepParser
  attr_accessor :steps, :file

  def initialize(file, keywords)
    @file = file
    @steps = []
    @keywords = keywords
    extract_steps(RubyParser.new.parse(File.read(file)))
  end

  def extract_steps(sexp)
    return unless sexp.is_a?(Sexp)

    case sexp.first
    when :block
      sexp[1..-1].each { |child_sexp| extract_steps(child_sexp) }
    when :iter
      step = create_step(sexp[1])
      @steps << step if step
    else
      sexp.each { |child| extract_steps(child) }
    end
  end

  def create_step(sexp)
    return unless sexp[0] == :call && @keywords.include?(sexp[2])

    Step.new(sexp[3][1], file, sexp.line)
  end
end

def results_to_alist(results)
  results.uniq! { |(_, file, line)| [file, line] }
  pairs = results.map do |(input, file, line)|
    "(#{input.inspect} . #{[file, line].join(':').inspect})"
  end
  "(#{pairs.join})"
end

iso_code, feature_path, line, step_search_path, step_search_path_extra, = ARGV
token_scanner = Gherkin::TokenScanner.new(IO.read(feature_path))
ast_builder = Gherkin::AstBuilder.new(Cucumber::Messages::IdGenerator::UUID.new)
parser = Gherkin::Parser.new(ast_builder)
extractor = StepExtractor.new(parser.parse(token_scanner))

step_info = extractor.step_at(line.to_i)
raise "Error: No step at #{feature_path}:#{line}." unless step_info

inputs = if step_info['examples']
           # If the step has example placeholders, we should substitute them
           # and provide user a choice
           holders = step_info['examples'].first.keys.map { |key| /<(#{key})>/ }
           step_info['examples'].map do |example|
             step_info['name'].gsub(Regexp.union(holders)) do |p|
               example[p.gsub(/[<>]/, '')]
             end
           end.uniq
         else
           [step_info['name']]
         end
quick_exit = inputs.size == 1

results = []
dialect = Gherkin::Dialect.for(iso_code)
keywords = [
  dialect.given_keywords[1].strip.to_sym,
  dialect.when_keywords[1].strip.to_sym,
  dialect.and_keywords[1].strip.to_sym,
  dialect.then_keywords[1].strip.to_sym,
  dialect.but_keywords[1].strip.to_sym
]
files = Dir[step_search_path]
files.push(*Dir[step_search_path_extra]) unless step_search_path_extra.nil?

files.each do |file|
  StepParser.new(file, keywords).steps.each do |step|
    inputs.each do |input_text|
      next unless step.match?(input_text)

      results.push([input_text, step.file, step.line])
      if quick_exit
        print results_to_alist(results)
        exit
      end
    end
  end
end
print results_to_alist(results)
