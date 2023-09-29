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
      separator = HEREDOC_SEPARATOR

      group do
        text(node.opening)
        line_suffix(priority: HEREDOC_PRIORITY) do
          group do
            target << separator

            parts.each do |part|
              case part.type
              when :string_node, :x_string_node
                value = part.content
                first = true

                value.each_line(chomp: true) do |line|
                  if first
                    first = false
                  else
                    target << separator
                  end

                  text(line)
                end

                target << separator if value.end_with?("\n")
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

        case argument.type
        when :parentheses_node
          body = argument.body

          if body.is_a?(StatementsNode) && body.body.length == 1
            case (first = body.body.first).type
            when :class_variable_read_node, :constant_read_node, :false_node,
                 :float_node, :global_variable_read_node, :imaginary_node,
                 :instance_variable_read_node, :integer_node,
                 :local_variable_read_node, :nil_node, :rational_node,
                 :self_node, :true_node
              text("#{keyword} ")
              format(first)
            when :array_node
              if first.elements.length > 1
                group do
                  text(keyword)
                  if_break { text("[") }.if_flat { text(" ") }

                  indent do
                    breakable_empty
                    seplist(first.elements) { |element| format(element) }
                  end

                  if_break do
                    breakable_empty
                    text("]")
                  end
                end
              else
                text("#{keyword} ")
                format(first)
              end
            else
              group do
                text(keyword)
                format(argument)
              end
            end
          else
            group do
              text(keyword)
              format(argument)
            end
          end
        when :class_variable_read_node, :constant_read_node, :false_node,
             :float_node, :global_variable_read_node, :imaginary_node,
             :instance_variable_read_node, :integer_node,
             :local_variable_read_node, :nil_node, :rational_node, :self_node,
             :true_node
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
      indent_value = true
      current = value

      # Here, determine whether or not we should attempt to indent the value of
      # the write. For certain nodes we can to avoid this because it will look
      # strange.
      until current.nil?
        case current.type
        when :array_node
          indent_value = current.opening_loc.nil?
          break
        when :hash_node, :lambda_node
          indent_value = false
          break
        when :string_node, :x_string_node, :interpolated_string_node, :interpolated_x_string_node
          indent_value = current.opening_loc ? !current.opening.start_with?("<<") : true
          break
        when :call_node
          current = current.receiver
        when :interpolated_symbol_node
          indent_value = current.opening_loc ? !current.opening.start_with?("%s") : true
          break
        else
          break
        end
      end

      group do
        yield
        text(" ")
        loc(operator_loc)

        if indent_value
          indent do
            breakable_space
            format(value)
          end
        else
          text(" ")
          format(value)
        end
      end
    end

    # If you have a modifier statement (for instance a modifier if statement or
    # a modifier while loop) there are times when you need to wrap the entire
    # statement in parentheses. This occurs when you have something like:
    #
    #     foo[:foo] =
    #       if bar?
    #         baz
    #       end
    #
    # Normally we would shorten this to an inline version, which would result in:
    #
    #     foo[:foo] = baz if bar?
    #
    # but this actually has different semantic meaning. The first example will
    # result in a nil being inserted into the hash for the :foo key, whereas the
    # second example will result in an empty hash because the if statement
    # applies to the entire assignment.
    #
    # We can fix this in a couple of ways. We can use the then keyword, as in:
    #
    #     foo[:foo] = if bar? then baz end
    #
    # But this isn't used very often. We can also just leave it as is with the
    # multi-line version, but for a short predicate and short value it looks
    # verbose. The last option and the one used here is to add parentheses on
    # both sides of the expression, as in:
    #
    #     foo[:foo] = (baz if bar?)
    #
    # This approach maintains the nice conciseness of the inline version, while
    # keeping the correct semantic meaning.
    def format_flat_parentheses
      case parent&.type
      when :arguments_node, 
            :call_and_write_node, :call_or_write_node, :call_operator_write_node,
            :class_variable_write_node, :class_variable_and_write_node, :class_variable_or_write_node, :class_variable_operator_write_node,
            :constant_write_node, :constant_and_write_node, :constant_or_write_node, :constant_operator_write_node,
            :constant_path_write_node, :constant_path_and_write_node, :constant_path_or_write_node, :constant_path_operator_write_node,
            :global_variable_write_node, :global_variable_and_write_node, :global_variable_or_write_node, :global_variable_operator_write_node,
            :instance_variable_write_node, :instance_variable_and_write_node, :instance_variable_or_write_node, :instance_variable_operator_write_node,
            :local_variable_write_node, :local_variable_and_write_node, :local_variable_or_write_node, :local_variable_operator_write_node,
            :multi_write_node,
            :assoc_node, :call_node, :defined_node
        text("(")
        yield
        text(")")
      else
        yield
      end
    end
  end
end
