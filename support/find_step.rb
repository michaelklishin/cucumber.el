require 'rubygems'
gem 'ruby_parser'
# >= 2.11.8 because of https://github.com/cucumber/gherkin/commit/90121f513000b89fce4d24c4e7dfdae00a5b177f
gem 'gherkin', '>= 2.11.8'

require 'ruby_parser'
require 'yaml'
require 'gherkin'
require 'gherkin/formatter/json_formatter'

class StepExtractor < Gherkin::Formatter::JSONFormatter
  def initialize
    super(StringIO.new)
  end

  # returns a Hash describing step at the line or nil if nothing
  # executable found
  def step_at(line)
    @feature_hashes[0]['elements'].each do |element|
      element['steps'].each do |step|
        if step['line'] == line
          if element['examples']
            rows = element['examples'][0]['rows'].each
            header = rows.next['cells']
            examples = []
            loop do
              row = rows.next['cells']
              example = {}
              row.each_with_index do |val, idx|
                example[header[idx]] = val
              end
              examples.push(example)
            end
            step['examples'] = examples
          end
          return step
        end
      end
    end
    nil
  end
end

class Step
  attr_reader :file, :line, :regexp
  def initialize(regexp, file, line)
    @file, @line = file, line
    self.regexp = regexp
  end

  def regexp=(value)
    @regexp =
      case value
      when String
        pieces, regexp = [], value.dup
        regexp.gsub!(/\$\w+/) { |match| pieces << match; "TOKEN" }
        regexp = Regexp.escape(regexp)
        regexp.gsub!(/TOKEN/) { |match| "(.*)" }
        Regexp.new("^#{regexp}$")
      when Regexp
        value
      else
        STDERR.puts "Warning: invalid parameter to Given/When/Then on #{file}:#{line}.  Expected Regexp or String, got #{value.class} #{value.inspect}"
        Regexp.new(/^INVALID PARAM$/)
      end
  end

  def match?(text)
    @regexp.match(text)
  end
end

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
      sexp[1..-1].each do |child_sexp|
        extract_steps(child_sexp)
      end
    when :iter
      child_sexp = sexp[1]
      return unless child_sexp[0] == :call && @keywords.include?(child_sexp[2])
      regexp = child_sexp[3][1]
      @steps << Step.new(regexp, file, child_sexp.line)
    else
      sexp.each do |child|
        extract_steps(child)
      end
    end
  end
end

iso_code, feature_path, line, = ARGV
extractor = StepExtractor.new
parser = Gherkin::Parser::Parser.new(extractor, true, 'root', false, iso_code)
parser.parse(IO.read(feature_path), feature_path, 0)
step_info = extractor.step_at(line.to_i)
inputs = if step_info['examples']
  # If the step has example placeholders, we should substitute them
  # and provide user a choice
  placeholders = step_info['examples'].first.keys.map { |key| %r(<(#{key})>) }
  step_info['examples'].map do |example|
    step_info['name'].gsub(Regexp.union(placeholders)) do |p|
      example[p.gsub(/[<>]/, '')]
    end
  end.uniq
else
  [step_info['name']]
end
quick_exit = inputs.size == 1

def results_to_alist(results)
  results.uniq! { |(_, file, line)| [file, line]}
  pairs = results.reduce("") do |acc, (input, file, line)|
    acc << sprintf("(%s . %s)", input.inspect, [file, line].join(":").inspect)
  end
  "(#{pairs})"
end

results = []
keywords = Gherkin::I18n.get(iso_code).step_keywords[1..-1].map { |k| k.strip.to_sym }
files = Dir["features/**/*steps.rb"]
files.each_with_index do |file, i|
  StepParser.new(file, keywords).steps.each do |step|
    inputs.each do |input_text|
      if step.match?(input_text)
        results.push([input_text, step.file, step.line])
        if quick_exit
          print results_to_alist(results)
          exit
        end
      end
    end
  end
end
print results_to_alist(results)
