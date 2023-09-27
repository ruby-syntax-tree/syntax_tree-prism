# frozen_string_literal: true

module Prism
  # A slightly enhanced PrettierPrint that knows how to format recursively
  # including comments.
  class Formatter < PrettierPrint
    # Unfortunately, Gem::Version.new is not ractor-safe because it performs
    # global caching using a class variable. This works around that by just
    # setting the instance variables directly.
    class SemanticVersion < ::Gem::Version
      def initialize(version)
        @version = version
        @segments = nil
      end
    end

    # We want to minimize as much as possible the number of options that are
    # available in syntax tree. For the most part, if users want non-default
    # formatting, they should override the format methods on the specific nodes
    # themselves. However, because of some history with prettier and the fact
    # that folks have become entrenched in their ways, we decided to provide a
    # small amount of configurability.
    class Options
      attr_reader :quote, :trailing_comma, :disable_auto_ternary, :target_ruby_version

      def initialize(quote: :default, trailing_comma: :default, disable_auto_ternary: :default, target_ruby_version: :default)
        @quote =
          if quote == :default
            # We ship with a single quotes plugin that will define this
            # constant. That constant is responsible for determining the default
            # quote style. If it's defined, we default to single quotes,
            # otherwise we default to double quotes.
            defined?(SINGLE_QUOTES) ? "'" : "\""
          else
            quote
          end

        @trailing_comma =
          if trailing_comma == :default
            # We ship with a trailing comma plugin that will define this
            # constant. That constant is responsible for determining the default
            # trailing comma value. If it's defined, then we default to true.
            # Otherwise we default to false.
            defined?(TRAILING_COMMA)
          else
            trailing_comma
          end

        @disable_auto_ternary =
          if disable_auto_ternary == :default
            # We ship with a disable ternary plugin that will define this
            # constant. That constant is responsible for determining the default
            # disable ternary value. If it's defined, then we default to true.
            # Otherwise we default to false.
            defined?(DISABLE_AUTO_TERNARY)
          else
            disable_auto_ternary
          end

        @target_ruby_version =
          if target_ruby_version == :default
            # The default target Ruby version is the current version of Ruby.
            # This is really only used for very niche cases, and it shouldn't be
            # used by most users.
            SemanticVersion.new(RUBY_VERSION)
          else
            target_ruby_version
          end
      end
    end

    COMMENT_PRIORITY = 1
    HEREDOC_PRIORITY = 2

    attr_reader :source, :stack

    # These options are overridden in plugins to we need to make sure they are
    # available here.
    attr_reader :quote, :trailing_comma, :disable_auto_ternary, :target_ruby_version

    alias trailing_comma? trailing_comma
    alias disable_auto_ternary? disable_auto_ternary

    def initialize(source, *args, options: Options.new)
      super(*args)

      @source = source
      @stack = []

      # Memoizing these values to make access faster.
      @quote = options.quote
      @trailing_comma = options.trailing_comma
      @disable_auto_ternary = options.disable_auto_ternary
      @target_ruby_version = options.target_ruby_version
    end

    def self.format(source, node, base_indentation = 0)
      q = new(source, [])
      q.format(node)
      q.flush(base_indentation)
      q.output.join
    end

    # Print out a slice of the given location, and handle any attached trailing
    # comments that may be present.
    def loc(location)
      text(location.slice)
      location.comments.each { |comment| comment.format(self) }
    end

    def format(node, stackable: true)
      stack << node if stackable
      doc = nil

      # If there are comments, then we're going to format them around the node
      # so that they get printed properly.
      if (comments = node.location.comments).any?
        trailing = []
        last_leading = nil

        # First, we're going to print all of the comments that were found before
        # the node. We'll also gather up any trailing comments that we find.
        comments.each do |comment|
          if comment.location.start_offset < node.location.start_offset
            text(comment.location.slice)
            breakable(force: true)
            last_leading = comment
          else
            trailing << comment
          end
        end

        doc = node.format(self)

        # Print all comments that were found after the node.
        trailing.each do |comment|
          case comment.type
          when :inline
            line_suffix(priority: COMMENT_PRIORITY) do
              comment.trailing? ? text(" ") : breakable
              text(comment.location.slice)
              break_parent
            end
          when :__END__
            trim
            text(comment.slice)
          when :embdoc
            breakable_force
            trim
            text(comment.slice)
          end
        end
      else
        doc = node.format(self)
      end

      stack.pop if stackable
      doc
    end

    def format_each(nodes)
      nodes.each { |node| format(node) }
    end

    def grandparent
      stack[-3]
    end

    def parent
      stack[-2]
    end

    def parents
      stack[0...-1].reverse_each
    end

    # This is a simplified version of prettyprint's group. It doesn't provide
    # any of the more advanced options because we don't need them and they take
    # up expensive computation time.
    def group
      contents = []
      doc = Group.new(0, contents: contents)

      groups << doc
      target << doc

      with_target(contents) { yield }
      groups.pop
      doc
    end

    # A similar version to the super, except that it calls back into the
    # separator proc with the instance of `self`.
    def seplist(list, sep = nil, iter_method = :each)
      first = true
      list.__send__(iter_method) do |*v|
        if first
          first = false
        elsif sep
          sep.call(self)
        else
          comma_breakable
        end
        yield(*v)
      end
    end

    # This is a much simplified version of prettyprint's text. It avoids
    # calculating width by pushing the string directly onto the target.
    def text(string)
      target << string
    end

    ############################################################################
    # Format patterns                                                          #
    ############################################################################

    PARENTHESES_BYPASS = [
      ClassVariableReadNode,
      ConstantReadNode,
      FalseNode,
      FloatNode,
      GlobalVariableReadNode,
      ImaginaryNode,
      InstanceVariableReadNode,
      IntegerNode,
      LocalVariableReadNode,
      NilNode,
      RationalNode,
      SelfNode,
      TrueNode
    ]

    # foo or bar
    def format_binary(left, operator_loc, right)
      group do
        format(left)
        text(" ")
        loc(operator_loc)
        indent do
          breakable_space
          format(right)
        end
      end
    end

    # foo { bar # comment }
    def format_body(body, comments, force_break = true)
      break_parent if force_break && (body || comments.any?)

      indent do
        if body
          breakable_empty
          format(body)
        end

        comments.each do |comment|
          breakable_force
          text(comment.slice)
        end
      end
    end

    # foo[bar, baz]
    def format_body_list_empty(list, comments)
      indent do
        if list.any?
          breakable_empty
          seplist(list) { |item| format(item) }
        end

        comments.each do |comment|
          breakable_force
          text(comment.slice)
        end
      end
    end

    # foo { bar, baz }
    def format_body_list_space(list, comments)
      indent do
        if list.any?
          breakable_space
          seplist(list) { |item| format(item) }
        end

        comments.each do |comment|
          breakable_force
          text(comment.slice)
        end
      end
    end

    # This is a very specific behavior where you want to force a newline, but
    # don't want to force the break parent.
    HEREDOC_SEPARATOR = PrettierPrint::Breakable.new(" ", 1, indent: false, force: true).freeze

    # <<~FOO FOO
    def format_heredoc(node, parts)
      group do
        text(node.opening)
        line_suffix(priority: HEREDOC_PRIORITY) do
          group do
            target << HEREDOC_SEPARATOR

            parts.each do |part|
              if part.is_a?(StringNode) || part.is_a?(XStringNode)
                value = part.content
                first = true

                value.each_line(chomp: true) do |line|
                  if first
                    first = false
                  else
                    target << HEREDOC_SEPARATOR
                  end

                  text(line)
                end

                target << HEREDOC_SEPARATOR if value.end_with?("\n")
              else
                format(part)
              end
            end

            text(node.closing.chomp)
          end
        end
      end
    end

    # next foo
    def format_jump(keyword, arguments)
      if !arguments
        text(keyword)
      elsif arguments.arguments.length == 1
        argument = arguments.arguments.first

        case argument
        when ParenthesesNode
          if (body = argument.body).is_a?(StatementsNode) && body.body.length == 1 && (first = body.body.first) && PARENTHESES_BYPASS.include?(first.class)
            text("#{keyword} ")
            format(first)
          else
            group do
              text(keyword)
              format(argument)
            end
          end
        when *PARENTHESES_BYPASS
          text("#{keyword} ")
          format(argument)
        else
          group do
            text(keyword)
            if_break { text("(") }.if_flat { text(" ") }
  
            indent do
              breakable_empty
              format(argument)
            end
  
            if_break do
              breakable_empty
              text(")")
            end
          end
        end
      else
        group do
          text(keyword)
          if_break { text("(") }.if_flat { text(" ") }

          indent do
            breakable_empty
            format(arguments)
          end

          if_break do
            breakable_empty
            text(")")
          end
        end
      end
    end

    # *foo
    def format_prefix(operator_loc, value)
      if value
        group do
          loc(operator_loc)
          nest(operator_loc.length) do
            breakable_empty
            format(value)
          end
        end
      else
        loc(operator_loc)
      end
    end

    # foo = bar
    def format_write(operator_loc, value)
      group do
        yield
        text(" ")
        loc(operator_loc)

        indent do
          breakable_space
          format(value)
        end
      end
    end
  end
end
