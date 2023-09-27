# frozen_string_literal: true

require "prettier_print"
require "prism"

module Prism
  class ParseResult
    # Format the syntax tree using the default options.
    def format
      attach_comments!
      options = Formatter::Options.new
      formatter = Formatter.new(source.source, [], 80, options: options)
      formatter.format(value)
      formatter.flush
      formatter.output.join
    end
  end

  TESTS = {}

  class Node
    def self.test(output, input = output, &block)
      (TESTS[self] ||= []) << [output, input, block]
    end
  end
end

require "syntax_tree/prism/formatter"
require "syntax_tree/prism/nodes"
