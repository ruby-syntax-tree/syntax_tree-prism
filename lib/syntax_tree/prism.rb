# frozen_string_literal: true

require_relative "prism/prettier_print"
require "prism"

module Prism
  class Format < PrettierPrint
    COMMENT_PRIORITY = 1
    HEREDOC_PRIORITY = 2

    attr_reader :source, :stack
    attr_reader :quote

    def initialize(source, *args, quote: "\"")
      @source = source
      @stack = []

      @quote = quote

      super(*args)
    end

    # alias $foo $bar
    # ^^^^^^^^^^^^^^^
    def visit_alias_global_variable_node(node)
      group do
        text("alias ")
        visit(node.new_name)
        nest(6) do
          breakable_space
          visit(node.old_name)
        end
      end
    end

    # alias foo bar
    # ^^^^^^^^^^^^^
    def visit_alias_method_node(node)
      new_name = node.new_name
      old_name = node.old_name

      group do
        text("alias ")

        if new_name.is_a?(SymbolNode)
          text(new_name.value)
          new_name.location.comments.each { |comment| visit_comment(comment) }
        else
          visit(new_name)
        end

        nest(6) do
          breakable_space
          if old_name.is_a?(SymbolNode)
            text(old_name.value)
          else
            visit(old_name)
          end
        end
      end
    end

    # foo => bar | baz
    #        ^^^^^^^^^
    def visit_alternation_pattern_node(node)
      group do
        visit(node.left)
        text(" ")
        visit_location(node.operator_loc)
        breakable_space
        visit(node.right)
      end
    end

    # a and b
    # ^^^^^^^
    def visit_and_node(node)
      visit_binary(node.left, node.operator_loc, node.right)
    end

    # []
    # ^^
    def visit_array_node(node)
      opening_loc = node.opening_loc
      elements = node.elements

      # If we have no opening location, then this is an implicit array by virtue
      # of an assignment operator. In this case we will print out the elements
      # of the array separated by commas and newlines.
      if opening_loc.nil?
        group { seplist(elements) { |element| visit(element) } }
        return
      end

      # If this is a specially formatted array, we will leave it be and format
      # it according to how the source has it formatted.
      opening = opening_loc.slice
      if opening.start_with?("%")
        group do
          text(opening)
          indent do
            breakable_empty
            seplist(elements, -> { breakable_space }) { |element| visit(element) }
          end
          breakable_empty
          text(node.closing)
        end
        return
      end

      # If this array has no comments on the start of the end location and it
      # has more than 2 elements, we'll check if we can automatically convert it
      # into a %w or %i array.
      closing_loc = node.closing_loc
      if opening_loc.comments.empty? && closing_loc.comments.empty? && elements.length >= 2
        if elements.all? { |element| element.is_a?(StringNode) && element.location.comments.empty? && !element.content.match?(/[\s\[\]\\]/) }
          group do
            text("%w[")
            indent do
              breakable_empty
              seplist(elements, -> { breakable_space }) { |element| text(element.content) }
            end
            breakable_empty
            text("]")
          end
          return
        elsif elements.all? { |element| element.is_a?(SymbolNode) && element.location.comments.empty? }
          group do
            text("%i[")
            indent do
              breakable_empty
              seplist(elements, -> { breakable_space }) { |element| text(element.value) }
            end
            breakable_empty
            text("]")
          end
          return
        end
      end

      # Otherwise we'll format the array normally.
      group do
        visit_location(opening_loc)
        visit_elements(elements, closing_loc.comments)
        breakable_empty
        text("]")
      end
    end

    # foo => [bar]
    #        ^^^^^
    def visit_array_pattern_node(node)
      constant = node.constant
      opening_loc = node.opening_loc

      group do
        visit(constant) unless constant.nil?
        text("[")
        opening_loc.comments.each { |comment| visit_comment(comment) } unless opening_loc.nil?
        visit_elements([*node.requireds, *node.rest, *node.posts], node.closing_loc&.comments || [])
        breakable_empty
        text("]")
      end
    end

    # foo(bar)
    #     ^^^
    def visit_arguments_node(node)
      seplist(node.arguments) { |argument| visit(argument) }
    end

    # { a: 1 }
    #   ^^^^
    def visit_assoc_node(node)
      if node.value.is_a?(HashNode)
        visit_assoc_node_inner(node)
      else
        group { visit_assoc_node_inner(node) }
      end
    end

    # Visit an assoc node and format the key and value.
    private def visit_assoc_node_inner(node)
      operator_loc = node.operator_loc
      value = node.value

      if operator_loc.nil?
        visit(node.key)
        visit_assoc_node_value(value) if !value.nil? && !value.is_a?(ImplicitNode)
      else
        visit(node.key)
        text(" ")
        visit_location(operator_loc)
        visit_assoc_node_value(value)
      end
    end

    # Visit the value of an association node.
    private def visit_assoc_node_value(node)
      if indent_write?(node)
        indent do
          breakable_space
          visit(node)
        end
      else
        text(" ")
        visit(node)
      end
    end

    # Visit an assoc node and format the key as a label.
    private def visit_assoc_node_label(node)
      if node.operator_loc.nil?
        visit(node)
      else
        case (key = node.key).type
        when :interpolated_symbol_node
          opening = key.opening

          if opening.start_with?("%")
            text("#{quote}")
            key.parts.each { |part| visit(part) }
            text("#{quote}:")
          else
            group do
              text(key.opening[1..])
              key.parts.each { |part| visit(part) }
              text(key.closing)
              text(":")
            end
          end
        when :symbol_node
          text(key.value)
          text(":")
        else
          raise "Unexpected key: #{key.inspect}"
        end

        visit_assoc_node_value(node.value)
      end
    end

    # Visit an assoc node and format the key as a rocket.
    private def visit_assoc_node_rocket(node)
      case (key = node.key).type
      when :interpolated_symbol_node
        opening = key.opening

        if opening.start_with?("%")
          visit(key)
        else
          group do
            text(":")
            text(opening)
            key.parts.each { |part| visit(part) }
            text(key.closing.chomp(":"))
          end
        end
      when :symbol_node
        text(":")
        text(key.value)
      else
        visit(key)
      end

      text(" ")
      operator_loc = node.operator_loc
      operator_loc ? visit_location(operator_loc) : text("=>")
      visit_assoc_node_value(node.value)
    end

    # def foo(**); bar(**); end
    #                  ^^
    #
    # { **foo }
    #   ^^^^^
    def visit_assoc_splat_node(node)
      visit_prefix(node.operator_loc, node.value)
    end

    # $+
    # ^^
    def visit_back_reference_read_node(node)
      text(node.slice)
    end

    # begin end
    # ^^^^^^^^^
    def visit_begin_node(node)
      begin_keyword_loc = node.begin_keyword_loc
      statements = node.statements

      rescue_clause = node.rescue_clause
      else_clause = node.else_clause
      ensure_clause = node.ensure_clause

      if begin_keyword_loc.nil?
        group do
          visit(statements) unless statements.nil?

          unless rescue_clause.nil?
            nest(-2) do
              breakable_force
              visit(rescue_clause)
            end
          end

          unless else_clause.nil?
            nest(-2) do
              breakable_force
              visit(else_clause)
            end
          end

          unless ensure_clause.nil?
            nest(-2) do
              breakable_force
              visit(ensure_clause)
            end
          end
        end
      else
        group do
          visit_location(begin_keyword_loc)

          unless statements.nil?
            indent do
              breakable_force
              visit(statements)
            end
          end

          unless rescue_clause.nil?
            breakable_force
            visit(rescue_clause)
          end

          unless else_clause.nil?
            breakable_force
            visit(else_clause)
          end

          unless ensure_clause.nil?
            breakable_force
            visit(ensure_clause)
          end

          breakable_force
          text("end")
        end
      end
    end

    # foo(&bar)
    #     ^^^^
    def visit_block_argument_node(node)
      visit_prefix(node.operator_loc, node.expression)
    end

    # foo { |; bar| }
    #          ^^^
    def visit_block_local_variable_node(node)
      text(node.name.name)
    end

    # A block on a keyword or method call.
    def visit_block_node(node)
      parameters = node.parameters
      body = node.body
      opening = node.opening

      # If this is nested anywhere inside of a Command or CommandCall node, then
      # we can't change which operators we're using for the bounds of the block.
      previous = nil
      break_opening, break_closing, flat_opening, flat_closing =
        if stack[0...-1].reverse_each.any? do |parent|
             case parent.type
             when :statements_node
               break false
             when :call_node
               break true if parent.arguments && parent.opening_loc.nil?
               break false if !parent.opening_loc.nil?
             end

             previous = parent
             false
           end

          block_close = opening == "do" ? "end" : "}"
          [opening, block_close, opening, block_close]
        elsif %i[forwarding_super_node super_node].include?(stack[-2].type)
          %w[do end do end]
        elsif stack[0...-1].reverse_each.any? do |parent|
                case parent.type
                when :parentheses_node, :statements_node
                  break false
                when :if_node, :unless_node, :while_node, :until_node
                  break true if parent.predicate == previous
                end

                previous = parent
                false
              end
          %w[{ } { }]
        else
          %w[do end { }]
        end

      parent = stack[-2]

      # If the receiver of this block a call without parentheses, so we need to
      # break the block.
      if parent.is_a?(CallNode) && parent.arguments && parent.opening_loc.nil?
        break_parent
        visit_block_node_break(node, break_opening, break_closing)
      else
        group do
          if_break { visit_block_node_break(node, break_opening, break_closing) }.if_flat do
            text(flat_opening)

            if parameters.is_a?(BlockParametersNode)
              text(" ")
              visit(parameters)
            end
            
            breakable_space if body || node.closing_loc.comments.any?
            visit_body(body, node.closing_loc.comments, false)
            breakable_space if parameters || body

            text(flat_closing)
          end
        end
      end
    end

    # Visit a block node in the break form.
    private def visit_block_node_break(node, break_opening, break_closing)
      parameters = node.parameters

      text(break_opening)
      node.opening_loc.comments.each { |comment| visit_comment(comment) }

      if parameters.is_a?(BlockParametersNode)
        text(" ")
        visit(parameters)
      end

      visit_body(node.body, node.closing_loc.comments, false)
      breakable_space
      text(break_closing)
    end

    # def foo(&bar); end
    #         ^^^^
    def visit_block_parameter_node(node)
      name = node.name

      group do
        visit_location(node.operator_loc)

        if name
          nest(1) do
            breakable_empty
            text(name.name)
          end
        end
      end
    end

    # A block's parameters.
    def visit_block_parameters_node(node)
      parameters = node.parameters
      locals = node.locals
      opening_loc = node.opening_loc

      group do
        if parameters || locals.any?
          if opening_loc
            visit_location(opening_loc)
          else
            text("(")
          end
        end

        remove_breaks(visit(parameters)) if parameters

        if locals.any?
          text("; ")
          seplist(locals) { |local| visit(local) }
        end

        text(node.closing || ")") if parameters || locals.any?
      end
    end

    # break
    # ^^^^^
    #
    # break foo
    # ^^^^^^^^^
    def visit_break_node(node)
      visit_jump("break", node.arguments)
    end

    ATTACH_DIRECTLY = %i[array_node hash_node string_node interpolated_string_node x_string_node interpolated_x_string_node if_node unless_node].freeze

    # foo
    # ^^^
    #
    # foo.bar
    # ^^^^^^^
    #
    # foo.bar() {}
    # ^^^^^^^^^^^^
    def visit_call_node(node)
      receiver = node.receiver
      call_operator = node.call_operator
      message = node.message
      name = node.name

      opening_loc = node.opening_loc
      closing_loc = node.closing_loc

      arguments = [*node.arguments&.arguments]
      block = node.block

      if block.is_a?(BlockArgumentNode)
        arguments << block
        block = nil
      end

      unless node.safe_navigation?
        case name
        when :!
          if message == "not"
            if receiver
              group do
                text("not")

                if opening_loc
                  visit_location(opening_loc)
                else
                  if_break { text("(") }.if_flat { text(" ") }
                end

                indent do
                  breakable_empty
                  visit(receiver)
                end

                if closing_loc
                  breakable_empty
                  visit_location(closing_loc)
                else
                  if_break do
                    breakable_empty
                    text(")")
                  end
                end
              end
            else
              text("not()")
            end

            return
          end

          if arguments.empty? && block.nil?
            visit_prefix(node.message_loc, receiver)
            return
          end
        when :+@, :-@, :~
          if arguments.empty? && block.nil?
            visit_prefix(node.message_loc, receiver)
            return
          end
        when :+, :-, :*, :/, :==, :!=, :>, :<, :>=, :<=, :<=>, :<<, :>>
          if arguments.length == 1 && block.nil?
            visit_binary(receiver, node.message_loc, arguments.first)
            return
          end
        when :**
          if arguments.length == 1 && block.nil?
            group do
              visit(receiver)
              text("**")
              indent do
                breakable_empty
                seplist(arguments) { |argument| visit(argument) }
              end
            end

            return
          end
        when :[]
          group do
            visit(receiver)
            text("[")

            if arguments.any?
              indent do
                breakable_empty
                seplist(arguments) { |argument| visit(argument) }
              end

              breakable_empty
            end

            text("]")

            if block
              text(" ")
              visit(block)
            end
          end

          return
        when :[]=
          if arguments.any?
            group do
              *before, after = arguments

              group do
                visit(receiver)
                text("[")

                if before.any?
                  indent do
                    breakable_empty
                    seplist(before) { |argument| visit(argument) }
                  end
                  breakable_empty
                end

                text("]")
              end

              text(" ")
              group do
                text("=")
                indent do
                  breakable_space
                  visit(after)
                end
              end

              if block
                text(" ")
                visit(block)
              end
            end
          else
            group do
              visit(receiver)
              text("[]")

              if block
                text(" ")
                visit(block)
              end
            end
          end

          return
        when :to, :to_not, :not_to
          # Very special handling here for RSpec. Methods on expectation objects
          # are almost always used without parentheses. This can result in
          # pretty ugly formatting, because the DSL gets super confusing.
          if opening_loc.nil?
            group do
              visit(receiver) if receiver
              visit_call_node_call_operator(node.call_operator_loc) if node.call_operator_loc
              visit_location(node.message_loc) if node.message_loc
              visit_call_node_rhs(node, 0)
            end

            return
          end
        end
      end

      # Now that we've passed through all of the special handling for specific
      # method names, we can handle the general case of a method call. In this
      # case we'll first build up a call chain for all of the calls in a row.
      # This could potentially be just a single method call.
      chain = [node]
      current = node

      while (receiver = current.receiver).is_a?(CallNode)
        chain.unshift(receiver)
        current = receiver
      end

      chain.unshift(receiver) if receiver

      if chain.length > 1
        if !ATTACH_DIRECTLY.include?(receiver&.type) &&
           chain[0...-1].all? do |node|
            !node.is_a?(CallNode) ||
              ((!node.arguments || node.name == :[]) && !node.block && !node.location.comments.any? && !node.call_operator_loc&.comments&.any? && !node.message_loc&.comments&.any?)
           end &&
           !chain[-1].call_operator_loc&.comments&.any? &&
           !chain[-1].message_loc&.comments&.any?
          # Special handling here for the case that we have a call chain that is
          # just method names and operators, ending with a call that has
          # anything else. In this case we'll put everything on the same line
          # and break the chain at the end. This can look like:
          #
          #     foo.bar.baz { |qux| qux }
          #
          # In this case if it gets broken, we don't want multiple lines of
          # method calls, instead we want to only break the block at the end,
          # like:
          #
          #     foo.bar.baz do |qux|
          #       qux
          #     end
          #
          group do
            *rest, last = chain

            doc =
              nest(0) do
                visit(rest.shift) unless rest.first.is_a?(CallNode)

                rest.each do |node|
                  visit_call_node_call_operator(node.call_operator_loc)

                  if node.name == :[]
                    visit_call_node_rhs(node, 0)
                  else
                    visit_location(node.message_loc)
                  end
                end

                visit_call_node_call_operator(last.call_operator_loc)
              end

            group do
              visit_location(last.message_loc) if last.message_loc && last.name != :[]
              visit_call_node_rhs(last, last_position(doc) + (last.message&.length || 0) + 1)
            end
          end
        else
          # Otherwise we'll break the chain at each node, indenting all of the
          # calls beyond the first one by one level of indentation.
          group do
            first, *rest = chain

            # If a call operator has a trailing comment on it, then we need to
            # put it on the previous line. In this case we need to communicate
            # to the next iteration in the loop that we have already printed the
            # call operator.
            call_operator_printed = false

            case first.type
            when :call_node
              # If the first node in the chain is a call node, we only need to
              # print the message because we will not have a receiver and we
              # will handle the arguments and block in the loop below.
              visit_location(first.message_loc)
              visit_call_node_rhs(first, first.message&.length + 1)

              # If the first call in the chain has a trailing comment on its
              # call operator, then we need to print it within this group.
              if (call_operator_loc = rest.first&.call_operator_loc)&.trailing_comments&.any? || rest.first&.message_loc&.leading_comments.any?
                call_operator_printed = true
                visit_call_node_call_operator(call_operator_loc)
              end

              if first.block.is_a?(BlockNode)
                node = rest.shift

                group do
                  node.location.leading_comments.each { |comment| visit_comment(comment) }
                  visit_call_node_call_operator(node.call_operator_loc) unless call_operator_printed
                  visit_location(node.message_loc) if node.message_loc && node.name != :[]
                  visit_call_node_rhs(node, (message&.length || 0) + 2)
                  node.location.trailing_comments.each { |comment| visit_comment(comment) }

                  # If the first call in the chain has a trailing comment on its
                  # call operator, then we need to print it within this group.
                  if (call_operator_loc = rest.first&.call_operator_loc)&.trailing_comments&.any? || rest.first&.message_loc&.leading_comments&.any?
                    call_operator_printed = true
                    visit_call_node_call_operator(call_operator_loc)
                  else
                    call_operator_printed = false
                  end
                end
              end
            when *ATTACH_DIRECTLY
              # Certain nodes we want to attach our message directly to them,
              # because it looks strange to have a message on a separate line.
              group do
                visit(first)
                node = rest.shift

                group do
                  node.location.leading_comments.each { |comment| visit_comment(comment) }
                  visit_call_node_call_operator(node.call_operator_loc)
                  visit_location(node.message_loc) if node.message_loc && node.name != :[]
                  visit_call_node_rhs(node, (message&.length || 0) + 2)
                  node.location.trailing_comments.each { |comment| visit_comment(comment) }

                  # If the first call in the chain has a trailing comment on its
                  # call operator, then we need to print it within this group.
                  if (call_operator_loc = rest.first&.call_operator_loc)&.trailing_comments&.any? || rest.first&.message_loc&.leading_comments&.any?
                    call_operator_printed = true
                    visit_call_node_call_operator(call_operator_loc)
                  end
                end
              end
            else
              # Otherwise, we'll format the receiver of the first member of the
              # call chain and then indent all of the calls by one level.
              visit(first)

              # If the first call in the chain has a trailing comment on its
              # call operator, then we need to print it within this group.
              if (call_operator_loc = rest.first.call_operator_loc)&.trailing_comments&.any? || rest.first.message_loc&.leading_comments&.any?
                call_operator_printed = true
                visit_call_node_call_operator(call_operator_loc)
              end
            end

            indent do
              rest.each_with_index do |node, index|
                if node.name == :not && (receiver = node.receiver).is_a?(CallNode) && receiver.name == :where && !receiver.arguments && !receiver.block
                  # Generally we will always break the chain at each node.
                  # However, there is some nice behavior here if we have a call
                  # chain with `where.not` in it (common in Rails). In that case
                  # it's nice to keep the `not` on the same line as the `where`.
                elsif call_operator_printed && node.message_loc&.leading_comments&.any? { |comment| comment.is_a?(EmbDocComment) }
                  # If we have already printed the call operator and the message
                  # location has a leading embdoc comment, then we already have
                  # a newline printed in this chain.
                elsif node.name == :[]
                  # If this is a call to `[]`, then we don't want to break the
                  # chain here, because we want to effectively treat it as a
                  # postfix operator.
                else
                  breakable_empty
                end

                group do
                  node.location.leading_comments.each { |comment| visit_comment(comment) }
                  visit_call_node_call_operator(node.call_operator_loc) unless call_operator_printed
                  visit_location(node.message_loc) if node.message_loc && node.name != :[]
                  visit_call_node_rhs(node, (node.message&.length || 0) + 2)
                  node.location.trailing_comments.each { |comment| visit_comment(comment) }

                  # If the call operator has a trailing comment, then we need
                  # to print it within this group.
                  if (call_operator_loc = rest[index + 1]&.call_operator_loc)&.trailing_comments&.any? || rest[index + 1]&.message_loc&.leading_comments&.any?
                    call_operator_printed = true
                    visit_call_node_call_operator(call_operator_loc)
                  else
                    call_operator_printed = false
                  end
                end
              end
            end
          end
        end
      else
        # If there is no call chain, then it's not possible that there's a
        # receiver. In this case we'll visit the message and then the arguments
        # and block.
        group do
          visit_location(node.message_loc)
          visit_call_node_rhs(node, node.message.length + 1)
        end
      end
    end

    private def visit_call_node_call_operator(location)
      visit_location(location, location.slice == "&." ? "&." : ".") if location
    end

    private def visit_call_node_rhs(node, position)
      arguments = [*node.arguments&.arguments]
      block = node.block

      if block.is_a?(BlockArgumentNode)
        arguments << block
        block = nil
      end

      if arguments.length == 1 && node.name.end_with?("=") && block.nil?
        text(" =")
        indent do
          breakable_space
          seplist(arguments) { |argument| visit(argument) }
        end
      elsif !node.opening_loc.nil? && arguments.any? && !node.closing_loc.nil?
        group do
          visit_location(node.opening_loc)
          indent do
            breakable_empty
            seplist(arguments) { |argument| visit(argument) }
          end
          breakable_empty
          visit_location(node.closing_loc)
        end
      elsif arguments.any?
        text(" ")
        visit_call_node_command_arguments(node, arguments, position)
      elsif node.opening_loc && node.closing_loc
        text("()")
      end

      if block
        text(" ")
        visit(block)
      end
    end

    # Align the contents of the given node with the last position. This is used
    # to align method calls without parentheses.
    private def visit_call_node_command_arguments(node, arguments, position)
      if node.arguments && node.arguments.arguments.length == 1
        argument = node.arguments.arguments.first

        case argument.type
        when :def_node
          seplist(arguments) { |argument| visit(argument) }
          return
        when :call_node
          if argument.opening_loc.nil?
            visit_call_node_command_arguments(argument, arguments, position)
            return
          end
        end
      end

      nest(position > (maxwidth / 2) ? 0 : position) do
        seplist(arguments) { |argument| visit(argument) }
      end
    end

    # foo.bar += baz
    # ^^^^^^^^^^^^^^^
    def visit_call_operator_write_node(node)
      receiver = node.receiver
      call_operator_loc = node.call_operator_loc

      visit_write(node.operator_loc, node.value) do
        group do
          if receiver
            visit(receiver)
            visit_call_node_call_operator(call_operator_loc)
          end

          text(node.message)
        end
      end
    end

    # foo.bar &&= baz
    # ^^^^^^^^^^^^^^^
    alias visit_call_and_write_node visit_call_operator_write_node

    # foo.bar ||= baz
    # ^^^^^^^^^^^^^^^
    alias visit_call_or_write_node visit_call_operator_write_node

    # foo.bar, = 1
    # ^^^^^^^
    def visit_call_target_node(node)
      group do
        visit(node.receiver)
        visit_location(node.call_operator_loc)
        text(node.message)
      end
    end

    # foo => bar => baz
    #        ^^^^^^^^^^
    def visit_capture_pattern_node(node)
      visit_binary(node.value, node.operator_loc, node.target)
    end

    # case foo; when bar; end
    # ^^^^^^^^^^^^^^^^^^^^^^^
    def visit_case_node(node)
      predicate = node.predicate
      consequent = node.consequent

      group do
        group do
          visit_location(node.case_keyword_loc)

          if predicate
            text(" ")
            nest(5) { visit(predicate) }
          end
        end

        breakable_force
        seplist(node.conditions, -> { breakable_force }) do |condition|
          visit(condition)
        end

        if consequent
          breakable_force
          visit(consequent)
        end

        indent do
          node.end_keyword_loc.comments.each do |comment|
            breakable_force
            text(comment.location.slice)
          end
        end

        breakable_force
        text("end")
      end
    end

    # case foo; in bar; end
    # ^^^^^^^^^^^^^^^^^^^^^
    alias visit_case_match_node visit_case_node

    # class Foo; end
    # ^^^^^^^^^^^^^^
    def visit_class_node(node)
      class_keyword_loc = node.class_keyword_loc
      inheritance_operator_loc = node.inheritance_operator_loc
      superclass = node.superclass

      group do
        group do
          visit_location(class_keyword_loc)

          if class_keyword_loc.comments.any?
            indent do
              breakable_space
              visit(node.constant_path)
            end
          else
            text(" ")
            visit(node.constant_path)
          end

          if superclass
            text(" ")
            visit_location(inheritance_operator_loc)

            if inheritance_operator_loc.comments.any?
              indent do
                breakable_space
                visit(superclass)
              end
            else
              text(" ")
              visit(superclass)
            end
          end
        end

        visit_body(node.body, node.end_keyword_loc.comments)
        breakable_force
        text("end")
      end
    end

    # @@foo
    # ^^^^^
    def visit_class_variable_read_node(node)
      text(node.name.name)
    end

    # @@foo = 1
    # ^^^^^^^^^
    #
    # @@foo, @@bar = 1
    # ^^^^^  ^^^^^
    def visit_class_variable_write_node(node)
      visit_write(node.operator_loc, node.value) { text(node.name.name) }
    end

    # @@foo += bar
    # ^^^^^^^^^^^^
    alias visit_class_variable_operator_write_node visit_class_variable_write_node

    # @@foo &&= bar
    # ^^^^^^^^^^^^^
    alias visit_class_variable_and_write_node visit_class_variable_write_node

    # @@foo ||= bar
    # ^^^^^^^^^^^^^
    alias visit_class_variable_or_write_node visit_class_variable_write_node

    # @@foo, = bar
    # ^^^^^
    alias visit_class_variable_target_node visit_class_variable_read_node

    # Foo
    # ^^^
    def visit_constant_read_node(node)
      text(node.name.name)
    end

    # Foo = 1
    # ^^^^^^^
    #
    # Foo, Bar = 1
    # ^^^  ^^^
    def visit_constant_write_node(node)
      visit_write(node.operator_loc, node.value) { text(node.name.name) }
    end

    # Foo += bar
    # ^^^^^^^^^^^
    alias visit_constant_operator_write_node visit_constant_write_node

    # Foo &&= bar
    # ^^^^^^^^^^^^
    alias visit_constant_and_write_node visit_constant_write_node

    # Foo ||= bar
    # ^^^^^^^^^^^^
    alias visit_constant_or_write_node visit_constant_write_node

    # Foo, = bar
    # ^^^
    alias visit_constant_target_node visit_constant_read_node

    # Foo::Bar
    # ^^^^^^^^
    def visit_constant_path_node(node)
      parent = node.parent

      group do
        visit(parent) if parent
        visit_location(node.delimiter_loc)
        indent do
          breakable_empty
          visit(node.child)
        end
      end
    end

    # Foo::Bar = 1
    # ^^^^^^^^^^^^
    #
    # Foo::Foo, Bar::Bar = 1
    # ^^^^^^^^  ^^^^^^^^
    def visit_constant_path_write_node(node)
      visit_write(node.operator_loc, node.value) { visit(node.target) }
    end

    # Foo::Bar += baz
    # ^^^^^^^^^^^^^^^
    alias visit_constant_path_operator_write_node visit_constant_path_write_node

    # Foo::Bar &&= baz
    # ^^^^^^^^^^^^^^^^
    alias visit_constant_path_and_write_node visit_constant_path_write_node

    # Foo::Bar ||= baz
    # ^^^^^^^^^^^^^^^^
    alias visit_constant_path_or_write_node visit_constant_path_write_node

    # Foo::Bar, = baz
    # ^^^^^^^^
    alias visit_constant_path_target_node visit_constant_path_node

    # This is not actually a node in the tree, but we insert it in the
    # formatting process so that it looks somewhat like a node. This makes the
    # handling of newlines in statements consistent.
    def visit_data_node(node)
      text(node.location.slice)
    end

    # def foo; end
    # ^^^^^^^^^^^^
    #
    # def self.foo; end
    # ^^^^^^^^^^^^^^^^^
    def visit_def_node(node)
      receiver = node.receiver
      name_loc = node.name_loc
      parameters = node.parameters
      lparen_loc = node.lparen_loc
      rparen_loc = node.rparen_loc

      group do
        group do
          group do
            text("def")
            text(" ") if !receiver.nil? || name_loc.comments.none? { |comment| comment.location.start_offset < name_loc.start_offset }

            group do
              if receiver
                visit(receiver)
                text(".")
              end

              visit_location(name_loc)
            end
          end

          if parameters
            lparen_loc ? visit_location(lparen_loc) : text("(")

            if parameters
              indent do
                breakable_empty
                visit(parameters)
              end
            end

            breakable_empty
            text(")")

            # Very specialized behavior here where inline comments do not force
            # a break parent. This should probably be an option on
            # visit_location.
            rparen_loc&.comments&.each do |comment|
              if comment.is_a?(InlineComment)
                line_suffix(priority: COMMENT_PRIORITY) do
                  comment.trailing? ? text(" ") : breakable
                  text(comment.location.slice)
                end
              else
                breakable_force
                trim
                text(comment.location.slice.rstrip)
              end
            end
          else
            visit_location(lparen_loc) if lparen_loc
            breakable_empty if lparen_loc&.comments&.any?
            visit_location(rparen_loc) if rparen_loc
          end
        end

        if node.equal_loc
          text(" ")
          visit_location(node.equal_loc)
          text(" ")
          visit(node.body)
        else
          visit_body(node.body, node.end_keyword_loc.comments)
          breakable_force
          text("end")
        end
      end
    end

    # defined? a
    # ^^^^^^^^^^
    #
    # defined?(a)
    # ^^^^^^^^^^^
    def visit_defined_node(node)
      group do
        text("defined?")
        if (lparen_loc = node.lparen_loc)
          visit_location(node.lparen_loc)
        else
          text("(")
        end

        visit_body(node.value, node.rparen_loc&.comments || [], false)
        breakable_empty
        text(")")
      end
    end

    # if foo then bar else baz end
    #                 ^^^^^^^^^^^^
    def visit_else_node(node)
      group do
        visit_location(node.else_keyword_loc)
        visit_body(node.statements, node.end_keyword_loc.comments)
      end
    end

    # "foo #{bar}"
    #      ^^^^^^
    def visit_embedded_statements_node(node)
      group do
        visit_location(node.opening_loc)

        if (statements = node.statements)
          indent do
            breakable_empty
            visit(statements)
          end
          breakable_empty
        end

        text("}")
      end
    end

    # "foo #@bar"
    #      ^^^^^
    def visit_embedded_variable_node(node)
      group do
        text("\#{")
        indent do
          breakable_empty
          visit(node.variable)
        end
        breakable_empty
        text("}")
      end
    end

    # begin; foo; ensure; bar; end
    #             ^^^^^^^^^^^^
    def visit_ensure_node(node)
      group do
        visit_location(node.ensure_keyword_loc)
        visit_body(node.statements, node.end_keyword_loc.comments)
      end
    end

    # false
    # ^^^^^
    def visit_false_node(node)
      text("false")
    end

    # foo => [*, bar, *]
    #        ^^^^^^^^^^^
    def visit_find_pattern_node(node)
      constant = node.constant

      group do
        visit(constant) if constant
        text("[")

        indent do
          breakable_empty
          seplist([node.left, *node.requireds, node.right]) { |element| visit(element) }
        end

        breakable_empty
        text("]")
      end
    end

    # if foo .. bar; end
    #    ^^^^^^^^^^
    def visit_flip_flop_node(node)
      left = node.left
      right = node.right

      group do
        visit(left) if left
        text(" ")
        visit_location(node.operator_loc)

        if right
          indent do
            breakable
            visit(right)
          end
        end
      end
    end

    # 1.0
    # ^^^
    def visit_float_node(node)
      text(node.slice)
    end

    # for foo in bar do end
    # ^^^^^^^^^^^^^^^^^^^^^
    def visit_for_node(node)
      group do
        text("for ")
        group { visit(node.index) }
        text(" in ")
        group { visit(node.collection) }
        visit_body(node.statements, node.end_keyword_loc.comments)
        breakable_force
        text("end")
      end
    end

    # def foo(...); bar(...); end
    #                   ^^^
    def visit_forwarding_arguments_node(node)
      text("...")
    end

    # def foo(...); end
    #         ^^^
    alias visit_forwarding_parameter_node visit_forwarding_arguments_node

    # super
    # ^^^^^
    #
    # super {}
    # ^^^^^^^^
    def visit_forwarding_super_node(node)
      block = node.block

      if block
        group do
          text("super ")
          visit(block)
        end
      else
        text("super")
      end
    end

    # $foo
    # ^^^^
    def visit_global_variable_read_node(node)
      text(node.name.name)
    end

    # $foo = 1
    # ^^^^^^^^
    #
    # $foo, $bar = 1
    # ^^^^  ^^^^
    def visit_global_variable_write_node(node)
      visit_write(node.operator_loc, node.value) { text(node.name.name) }
    end

    # $foo += bar
    # ^^^^^^^^^^^
    alias visit_global_variable_operator_write_node visit_global_variable_write_node

    # $foo &&= bar
    # ^^^^^^^^^^^^
    alias visit_global_variable_and_write_node visit_global_variable_write_node

    # $foo ||= bar
    # ^^^^^^^^^^^^
    alias visit_global_variable_or_write_node visit_global_variable_write_node

    # $foo, = bar
    # ^^^^
    alias visit_global_variable_target_node visit_global_variable_read_node

    # {}
    # ^^
    def visit_hash_node(node)
      elements = node.elements

      if elements.any? { |element| element.value.is_a?(ImplicitNode) }
        visit_hash_node_layout(node) { |element| visit(element) }
      elsif elements.all? { |element| !element.is_a?(AssocNode) || element.operator_loc.nil? || element.key.is_a?(InterpolatedSymbolNode) || (element.key.is_a?(SymbolNode) && (value = element.key.value) && value.match?(/^[_A-Za-z]/) && !value.end_with?("=")) }
        visit_hash_node_layout(node) { |element| visit_assoc_node_label(element) }
      else
        visit_hash_node_layout(node) { |element| visit_assoc_node_rocket(element) }
      end
    end

    # Visit a hash node and yield out each plain association element for
    # formatting by the caller.
    private def visit_hash_node_layout(node)
      elements = node.elements

      group do
        visit_location(node.opening_loc)
        indent do
          if elements.any?
            breakable_space
            seplist(elements) do |element|
              if element.is_a?(AssocNode)
                yield element
              else
                visit(element)
              end
            end
          end

          node.closing_loc.comments.each do |comment|
            breakable_force
            text(comment.location.slice)
          end
        end

        elements.any? ? breakable_space : breakable_empty
        text("}")
      end
    end

    # foo => {}
    #        ^^
    def visit_hash_pattern_node(node)
      constant = node.constant
      opening_loc = node.opening_loc
      closing_loc = node.closing_loc

      elements = [*node.elements, *node.rest]

      if constant
        group do
          visit(constant)
          text("[")
          opening_loc.comments.each { |comment| visit_comment(comment) } if opening_loc
          visit_elements(elements, closing_loc&.comments || [])
          breakable_empty
          text("]")
        end
      else
        group do
          text("{")
          opening_loc.comments.each { |comment| visit_comment(comment) } if opening_loc
          visit_elements_spaced(elements, closing_loc&.comments || [])
          elements.any? ? breakable_space : breakable_empty
          text("}")
        end
      end
    end

    # if foo then bar end
    # ^^^^^^^^^^^^^^^^^^^
    #
    # bar if foo
    # ^^^^^^^^^^
    #
    # foo ? bar : baz
    # ^^^^^^^^^^^^^^^
    def visit_if_node(node)
      if_keyword_loc = node.if_keyword_loc
      if_keyword = node.if_keyword

      statements = node.statements
      consequent = node.consequent

      if if_keyword == "elsif"
        # If we get here, then this is an if node that was expressed as an elsif
        # clause in a larger chain. In this case we can simplify formatting
        # because there are many things we don't need to check.
        group do
          visit_location(if_keyword_loc)
          text(" ")
          nest(6) { visit(node.predicate) }

          if consequent
            visit_body(statements, [], true)
            breakable_force
            visit(consequent)
          else
            visit_body(statements, node.end_keyword_loc.comments, true)
          end
        end
      elsif !if_keyword_loc
        # If there is no keyword location, then this if node was expressed as a
        # ternary. In this case we know quite a bit about the structure of the
        # node and will format it quite differently.
        truthy = statements.body.first
        falsy = consequent.statements.body.first

        if stack[-2].is_a?(ParenthesesNode) || forced_ternary?(truthy) || forced_ternary?(falsy)
          group { visit_ternary_node_flat(node, truthy, falsy) }
        else
          group do
            if_break { ensure_parentheses { visit_ternary_node_break(node, truthy, falsy) } }
              .if_flat { visit_ternary_node_flat(node, truthy, falsy) }
          end
        end
      elsif !statements || consequent || contains_conditional?(statements.body.first)
        # If there are no statements, no consequent clause, or the body of the
        # node has a conditional, then we will format the node in a break form,
        # which is to say the keyword first.
        group do
          visit_if_node_break(node)
          break_parent
        end
      elsif contains_write?(node.predicate) || contains_write?(statements)
        # If the predicate or the body of the node contains a write, then
        # changing the form of the conditional could impact the meaning of the
        # expression. In this case we will respect the form of the source.
        if node.end_keyword_loc.nil?
          group { visit_if_node_flat(node) }
        else
          group do
            visit_if_node_break(node)
            break_parent
          end
        end
      else
        # Otherwise, we will attempt to format the node in the flat form if it
        # fits, and otherwise we will break it into multiple lines.
        group do
          if_break { visit_if_node_break(node) }
            .if_flat { ensure_parentheses { visit_if_node_flat(node) } }
        end
      end
    end

    private def forced_ternary?(node)
      case node.type
      when :alias_node, :alias_global_variable_node, :break_node, :if_node,
           :unless_node, :lambda_node, :multi_write_node, :next_node,
           :rescue_modifier_node, :return_node, :super_node,
           :forwarding_super_node, :undef_node, :yield_node, :return_node,
           :call_and_write_node, :call_or_write_node, :call_operator_write_node,
           :class_variable_write_node, :class_variable_and_write_node, :class_variable_or_write_node, :class_variable_operator_write_node,
           :constant_write_node, :constant_and_write_node, :constant_or_write_node, :constant_operator_write_node,
           :constant_path_write_node, :constant_path_and_write_node, :constant_path_or_write_node, :constant_path_operator_write_node,
           :global_variable_write_node, :global_variable_and_write_node, :global_variable_or_write_node, :global_variable_operator_write_node,
           :instance_variable_write_node, :instance_variable_and_write_node, :instance_variable_or_write_node, :instance_variable_operator_write_node,
           :local_variable_write_node, :local_variable_and_write_node, :local_variable_or_write_node, :local_variable_operator_write_node
        true
      when :call_node
        node.receiver && node.opening_loc.nil?
      when :string_node, :interpolated_string_node, :x_string_node, :interpolated_x_string_node
        node.heredoc?
      else
        false
      end
    end

    private def visit_ternary_node_flat(node, truthy, falsy)
      visit(node.predicate)
      text(" ?")
      indent do
        breakable_space
        visit(truthy)
        text(" :")
        breakable_space
        visit(falsy)
      end
    end

    private def visit_ternary_node_break(node, truthy, falsy)
      group do
        text("if ")
        nest(3) { visit(node.predicate) }
      end

      indent do
        breakable_space
        visit(truthy)
      end

      breakable_space
      text("else")

      indent do
        breakable_space
        visit(falsy)
      end

      breakable_space
      text("end")
    end

    # Visit an if node in the break form.
    private def visit_if_node_break(node)
      statements = node.statements
      consequent = node.consequent

      group do
        visit_location(node.if_keyword_loc)
        text(" ")
        nest(3) { visit(node.predicate) }
      end

      if consequent
        visit_body(statements, [], false)
        breakable_space
        visit(consequent)
      else
        visit_body(statements, node.end_keyword_loc&.comments || [], false)
      end

      breakable_space
      text("end")
    end

    # Visit an if node in the flat form.
    private def visit_if_node_flat(node)
      visit(node.statements)
      text(" if ")
      visit(node.predicate)
    end

    # 1i
    def visit_imaginary_node(node)
      text(node.slice)
    end

    # { foo: }
    #   ^^^^
    def visit_implicit_node(node)
      # Nothing, because it represents implicit syntax.
    end

    # foo { |bar,| }
    #           ^
    def visit_implicit_rest_node(node)
      text(",")
    end

    # case foo; in bar; end
    # ^^^^^^^^^^^^^^^^^^^^^
    def visit_in_node(node)
      statements = node.statements

      group do
        text("in ")
        nest(3) { visit(node.pattern) }

        if statements
          indent do
            breakable_force
            visit(statements)
          end
        end
      end
    end

    # foo[bar] += baz
    # ^^^^^^^^^^^^^^^
    def visit_index_operator_write_node(node)
      arguments = [*node.arguments, *node.block]

      visit_write(node.operator_loc, node.value) do
        group do
          visit(node.receiver)
          visit_location(node.opening_loc)

          if arguments.any?
            indent do
              breakable_empty
              seplist(arguments) { |argument| visit(argument) }
            end
          end

          breakable_empty
          text("]")
        end
      end
    end

    # foo[bar] &&= baz
    # ^^^^^^^^^^^^^^^^
    alias visit_index_and_write_node visit_index_operator_write_node

    # foo[bar] ||= baz
    # ^^^^^^^^^^^^^^^^
    alias visit_index_or_write_node visit_index_operator_write_node

    # foo[bar], = 1
    # ^^^^^^^^
    def visit_index_target_node(node)
      group do
        visit(node.receiver)
        visit_location(node.opening_loc)

        if (arguments = (node.arguments&.arguments || [])).any?
          indent do
            breakable_empty
            seplist(arguments) { |argument| visit(argument) }
          end
        end

        breakable_empty
        text("]")
      end
    end

    # @foo
    # ^^^^
    def visit_instance_variable_read_node(node)
      text(node.name.name)
    end

    # @foo = 1
    # ^^^^^^^^
    #
    # @foo, @bar = 1
    # ^^^^  ^^^^
    def visit_instance_variable_write_node(node)
      visit_write(node.operator_loc, node.value) { text(node.name.name) }
    end

    # @foo += bar
    # ^^^^^^^^^^^
    alias visit_instance_variable_operator_write_node visit_instance_variable_write_node

    # @foo &&= bar
    # ^^^^^^^^^^^^
    alias visit_instance_variable_and_write_node visit_instance_variable_write_node

    # @foo ||= bar
    # ^^^^^^^^^^^^
    alias visit_instance_variable_or_write_node visit_instance_variable_write_node

    # @foo, = bar
    # ^^^^
    alias visit_instance_variable_target_node visit_instance_variable_read_node

    # 1
    # ^
    def visit_integer_node(node)
      slice = node.slice

      if slice.match?(/^[1-9]\d{4,}$/)
        # If it's a plain integer and it doesn't have any underscores separating
        # the values, then we're going to insert them every 3 characters
        # starting from the right.
        index = (slice.length + 2) % 3
        text("  #{slice}"[index..].scan(/.../).join("_").strip)
      else
        text(slice)
      end
    end

    # if /foo #{bar}/ then end
    #    ^^^^^^^^^^^^
    def visit_interpolated_match_last_line_node(node)
      visit_interpolated_regular_expression_node(node, node.parts)
    end

    # /foo #{bar}/
    # ^^^^^^^^^^^^
    def visit_interpolated_regular_expression_node(node)
      visit_regular_expression_node_parts(node, node.parts)
    end

    # "foo #{bar}"
    # ^^^^^^^^^^^^
    def visit_interpolated_string_node(node)
      parts = node.parts

      if node.heredoc?
        # First, if this interpolated string was expressed as a heredoc, then
        # we'll maintain that formatting and print it out again as a heredoc.
        visit_heredoc(node, parts)
      elsif parts.length > 1 && parts[0..1].all? { |part| (part.is_a?(StringNode) || part.is_a?(InterpolatedStringNode)) && !part.opening_loc.nil? }
        # Next, we'll check if this string is composed of multiple parts that
        # have their own opening. If it is, then this actually represents string
        # concatenation and not a single string literal. In this case we'll
        # format each on their own with appropriate spacing.
        group do
          visit(parts[0])
          if_break { text(" \\") }
          indent do
            breakable_space
            seplist(parts[1..], -> { if_break { text(" \\") }; breakable_space }) { |part| visit(part) }
          end
        end
      else
        # Finally, if it's a regular interpolated string, we'll forward this on
        # to our generic string node formatter.
        visit_string_node_parts(node, node.parts)
      end
    end

    # :"foo #{bar}"
    # ^^^^^^^^^^^^^
    def visit_interpolated_symbol_node(node)
      opening = node.opening
      parts = node.parts

      # First, we'll check if we don't have an opening. If we don't, then this
      # is inside of a %I literal and we should just print the parts as-is.
      return group { parts.each { |part| visit(part) } } if opening.nil?

      # If we're inside of an assoc node as the key, then it will handle
      # printing the : on its own since it could change sides.
      parent = stack[-2]
      hash_key = parent.is_a?(AssocNode) && parent.key == node

      # Here we determine the quotes to use for an interpolated symbol. It's
      # bound by a lot of rules because it could be in many different contexts
      # with many different kinds of escaping.
      opening_quote, closing_quote =
        if opening.start_with?("%s")
          # Here we're going to check if there is a closing character, a new
          # line, or a quote in the content of the dyna symbol. If there is,
          # then quoting could get weird, so just bail out and stick to the
          # original quotes in the source.
          matching = quotes_matching(opening[2])
          pattern = /[\n#{Regexp.escape(matching)}'"]/

          # This check is to ensure we don't find a matching quote inside of the
          # symbol that would be confusing.
          matched = parts.any? { |part| part.is_a?(StringNode) && part.content.match?(pattern) }

          if matched
            [opening, matching]
          elsif quotes_locked?(parts)
            ["#{":" unless hash_key}'", "'"]
          else
            ["#{":" unless hash_key}#{quote}", quote]
          end
        elsif quotes_locked?(parts)
          if hash_key
            if opening.start_with?(":")
              [opening[1..], "#{opening[1..]}:"]
            else
              [opening, node.closing]
            end
          else
            [opening, node.closing]
          end
        else
          [hash_key ? quote : ":#{quote}", quote]
        end

      group do
        text(opening_quote)
        parts.each do |part|
          if part.is_a?(StringNode)
            value = quotes_normalize(part.content, closing_quote)
            first = true

            value.each_line(chomp: true) do |line|
              if first
                first = false
              else
                breakable_return
              end

              text(line)
            end

            breakable_return if value.end_with?("\n")
          else
            visit(part)
          end
        end
        text(closing_quote)
      end
    end

    # `foo #{bar}`
    # ^^^^^^^^^^^^
    def visit_interpolated_x_string_node(node)
      if node.heredoc?
        visit_heredoc(node, node.parts)
      else
        group do
          text(node.opening)
          node.parts.each { |part| visit(part) }
          text(node.closing)
        end
      end
    end

    # it
    # ^^
    def visit_it_local_variable_read_node(node)
      text("it")
    end

    # foo(bar: baz)
    #     ^^^^^^^^
    def visit_keyword_hash_node(node)
      elements = node.elements

      case stack[-2]&.type
      when :break_node, :next_node, :return_node
        visit_keyword_hash_node_layout(node) { |element| visit(element) }
      else
        if elements.any? { |element| element.value.is_a?(ImplicitNode) }
          visit_keyword_hash_node_layout(node) { |element| visit(element) }
        elsif elements.all? { |element| !element.is_a?(AssocNode) || element.operator_loc.nil? || element.key.is_a?(InterpolatedSymbolNode) || (element.key.is_a?(SymbolNode) && (value = element.key.value) && value.match?(/^[_A-Za-z]/) && !value.end_with?("=")) }
          visit_keyword_hash_node_layout(node) { |element| visit_assoc_node_label(element) }
        else
          visit_keyword_hash_node_layout(node) { |element| visit_assoc_node_rocket(element) }
        end
      end
    end

    # Visit a keyword hash node and yield out each plain association element for
    # formatting by the caller.
    private def visit_keyword_hash_node_layout(node)
      seplist(node.elements) do |element|
        if element.is_a?(AssocNode)
          yield element
        else
          visit(element)
        end
      end
    end

    # def foo(**bar); end
    #         ^^^^^
    #
    # def foo(**); end
    #         ^^
    def visit_keyword_rest_parameter_node(node)
      name = node.name

      text("**")
      text(name.name) if name
    end

    # -> {}
    def visit_lambda_node(node)
      parameters = node.parameters
      body = node.body
      closing_comments = node.closing_loc.comments

      group do
        text("->")
        visit(parameters) if parameters.is_a?(BlockParametersNode)

        if body || closing_comments.any?
          text(" ")
          if_break do
            text("do")
            node.opening_loc.comments.each { |comment| visit_comment(comment) }

            indent do
              if body
                breakable_space
                visit(body)
              end

              closing_comments.each do |comment|
                breakable_force

                if comment.is_a?(InlineComment)
                  text(comment.location.slice)
                else
                  trim
                  text(comment.location.slice.rstrip)
                end
              end
            end

            breakable_space
            text("end")
          end.if_flat do
            if body
              text("{ ")
              visit(body)
              text(" }")
            else
              text(" {}")
            end
          end
        else
          text(" {}")
        end
      end
    end

    # foo
    # ^^^
    def visit_local_variable_read_node(node)
      text(node.name.name)
    end

    # foo = 1
    # ^^^^^^^
    #
    # foo, bar = 1
    # ^^^  ^^^
    def visit_local_variable_write_node(node)
      visit_write(node.operator_loc, node.value) { text(node.name.name) }
    end

    # foo += bar
    # ^^^^^^^^^^
    alias visit_local_variable_operator_write_node visit_local_variable_write_node

    # foo &&= bar
    # ^^^^^^^^^^^
    alias visit_local_variable_and_write_node visit_local_variable_write_node

    # foo ||= bar
    # ^^^^^^^^^^^
    alias visit_local_variable_or_write_node visit_local_variable_write_node

    # foo, = bar
    # ^^^
    alias visit_local_variable_target_node visit_local_variable_read_node

    # if /foo/ then end
    #    ^^^^^
    def visit_match_last_line_node(node)
      visit_regular_expression_node_parts(node, [bare_string(node, node.content_loc, node.location)])
    end

    # foo in bar
    # ^^^^^^^^^^
    def visit_match_predicate_node(node)
      pattern = node.pattern

      group do
        visit(node.value)
        text(" in")

        case pattern.type
        when :array_pattern_node, :hash_pattern_node, :find_pattern_node
          text(" ")
          visit(pattern)
        else
          indent do
            breakable_space
            visit(pattern)
          end
        end
      end
    end

    # foo => bar
    # ^^^^^^^^^^
    def visit_match_required_node(node)
      pattern = node.pattern

      group do
        visit(node.value)
        text(" =>")

        case pattern.type
        when :array_pattern_node, :hash_pattern_node, :find_pattern_node
          text(" ")
          visit(pattern)
        else
          indent do
            breakable_space
            visit(pattern)
          end
        end
      end
    end

    # /(?<foo>foo)/ =~ bar
    # ^^^^^^^^^^^^^^^^^^^^
    def visit_match_write_node(node)
      visit(node.call)
    end

    # A node that is missing from the syntax tree. This is only used in the
    # case of a syntax error. We'll format it as empty.
    def visit_missing_node(node)
    end

    # module Foo; end
    # ^^^^^^^^^^^^^^^
    def visit_module_node(node)
      module_keyword_loc = node.module_keyword_loc

      group do
        group do
          visit_location(module_keyword_loc)

          if module_keyword_loc.comments.any?
            indent do
              breakable_space
              visit(node.constant_path)
            end
          else
            text(" ")
            visit(node.constant_path)
          end
        end

        visit_body(node.body, node.end_keyword_loc.comments)
        breakable_force
        text("end")
      end
    end

    # foo, bar = baz
    # ^^^^^^^^
    def visit_multi_target_node(node)
      targets = [*node.lefts, *node.rest, *node.rights]
      implicit_rest = targets.pop if targets.last.is_a?(ImplicitRestNode)
      lparen_loc = node.lparen_loc

      group do
        if lparen_loc && node.rparen_loc
          visit_location(lparen_loc)
          indent do
            breakable_empty
            seplist(targets) { |target| visit(target) }
            visit(implicit_rest) if implicit_rest
          end
          breakable_empty
          text(")")
        else
          seplist(targets) { |target| visit(target) }
          visit(implicit_rest) if implicit_rest
        end
      end
    end

    # foo, bar = baz
    # ^^^^^^^^^^^^^^
    def visit_multi_write_node(node)
      targets = [*node.lefts, *node.rest, *node.rights]
      implicit_rest = targets.pop if targets.last.is_a?(ImplicitRestNode)
      lparen_loc = node.lparen_loc

      visit_write(node.operator_loc, node.value) do
        group do
          if lparen_loc && node.rparen_loc
            visit_location(lparen_loc)
            indent do
              breakable_empty
              seplist(targets) { |target| visit(target) }
              visit(implicit_rest) if implicit_rest
            end
            breakable_empty
            text(")")
          else
            seplist(targets) { |target| visit(target) }
            visit(implicit_rest) if implicit_rest
          end
        end
      end
    end

    # next
    # ^^^^
    #
    # next foo
    # ^^^^^^^^
    def visit_next_node(node)
      visit_jump("next", node.arguments)
    end

    # nil
    # ^^^
    def visit_nil_node(node)
      text("nil")
    end

    # def foo(**nil); end
    #         ^^^^^
    def visit_no_keywords_parameter_node(node)
      group do
        visit_location(node.operator_loc)
        nest(2) do
          breakable_empty
          text("nil")
        end
      end
    end

    # -> { _1 + _2 }
    # ^^^^^^^^^^^^^^
    def visit_numbered_parameters_node(node)
      raise "Visiting NumberedParametersNode is not supported."
    end

    # $1
    # ^^
    def visit_numbered_reference_read_node(node)
      text(node.slice)
    end

    # def foo(bar: baz); end
    #         ^^^^^^^^
    def visit_optional_keyword_parameter_node(node)
      group do
        text("#{node.name.name}:")
        indent do
          breakable_space
          visit(node.value)
        end
      end
    end

    # def foo(bar = 1); end
    #         ^^^^^^^
    def visit_optional_parameter_node(node)
      group do
        text("#{node.name.name} ")
        visit_location(node.operator_loc)
        indent do
          breakable_space
          visit(node.value)
        end
      end
    end

    # a or b
    # ^^^^^^
    def visit_or_node(node)
      visit_binary(node.left, node.operator_loc, node.right)
    end

    # def foo(bar, *baz); end
    #         ^^^^^^^^^
    def visit_parameters_node(node)
      parameters = node.compact_child_nodes
      implicit_rest = parameters.pop if parameters.last.is_a?(ImplicitRestNode)

      nest(0) do
        seplist(parameters) { |parameter| visit(parameter) }
        visit(implicit_rest) if implicit_rest
      end
    end

    # ()
    # ^^
    #
    # (1)
    # ^^^
    def visit_parentheses_node(node)
      group do
        visit_location(node.opening_loc)
        visit_body(node.body, node.closing_loc.comments, false)
        breakable_empty
        text(")")
      end
    end

    # foo => ^(bar)
    #        ^^^^^^
    def visit_pinned_expression_node(node)
      group do
        text("^")
        visit_location(node.lparen_loc)
        visit_body(node.expression, node.rparen_loc.comments, false)
        breakable_empty
        text(")")
      end
    end

    # foo = 1 and bar => ^foo
    #                    ^^^^
    def visit_pinned_variable_node(node)
      visit_prefix(node.operator_loc, node.variable)
    end

    # END {}
    def visit_post_execution_node(node)
      statements = node.statements
      closing_comments = node.closing_loc.comments

      group do
        text("END ")
        visit_location(node.opening_loc)

        if statements || closing_comments.any?
          indent do
            if statements
              breakable_space
              visit(statements)
            end

            closing_comments.each do |comment|
              breakable_empty
              text(comment.location.slice)
            end
          end

          breakable_space
        end

        text("}")
      end
    end

    # BEGIN {}
    def visit_pre_execution_node(node)
      statements = node.statements
      closing_comments = node.closing_loc.comments

      group do
        text("BEGIN ")
        visit_location(node.opening_loc)

        if statements || closing_comments.any?
          indent do
            if statements
              breakable_space
              visit(statements)
            end

            closing_comments.each do |comment|
              breakable_empty
              text(comment.location.slice)
            end
          end

          breakable_space
        end

        text("}")
      end
    end

    # The top-level program node.
    def visit_program_node(node)
      visit(node.statements)
      seplist(node.location.comments, -> { breakable_force }) do |comment|
        text(comment.location.slice.rstrip)
      end
      breakable_force
    end

    # 0..5
    # ^^^^
    def visit_range_node(node)
      left = node.left
      right = node.right

      group do
        visit(left) if left
        visit_location(node.operator_loc)
        visit(right) if right
      end
    end

    # 1r
    # ^^
    def visit_rational_node(node)
      text(node.slice)
    end

    # redo
    # ^^^^
    def visit_redo_node(node)
      text("redo")
    end

    # /foo/
    # ^^^^^
    def visit_regular_expression_node(node)
      visit_regular_expression_node_parts(node, [bare_string(node, node.content_loc, node.location)])
    end

    # def foo(bar:); end
    #         ^^^^
    def visit_required_keyword_parameter_node(node)
      text("#{node.name.name}:")
    end

    # def foo(bar); end
    #         ^^^
    def visit_required_parameter_node(node)
      text(node.name.name)
    end

    # foo rescue bar
    # ^^^^^^^^^^^^^^
    def visit_rescue_modifier_node(node)
      group do
        text("begin")
        indent do
          breakable_force
          visit(node.expression)
        end
        breakable_force
        visit_location(node.keyword_loc)
        text(" StandardError")
        indent do
          breakable_force
          visit(node.rescue_expression)
        end
        breakable_force
        text("end")
      end
    end

    # begin; rescue; end
    #        ^^^^^^^
    def visit_rescue_node(node)
      exceptions = node.exceptions
      operator_loc = node.operator_loc
      reference = node.reference

      statements = node.statements
      consequent = node.consequent

      group do
        group do
          visit_location(node.keyword_loc)

          if exceptions.any?
            text(" ")
            nest(7) { seplist(exceptions) { |exception| visit(exception) } }
          elsif reference.nil?
            text(" StandardError")
          end

          if reference
            text(" ")
            visit_location(operator_loc)

            if operator_loc.comments.any?
              indent do
                breakable_space
                visit(reference)
              end
            else
              text(" ")
              visit(reference)
            end
          end
        end

        if statements
          indent do
            breakable_force
            visit(statements)
          end
        end

        if consequent
          breakable_force
          visit(consequent)
        end
      end
    end

    # def foo(*bar); end
    #         ^^^^
    #
    # def foo(*); end
    #         ^
    def visit_rest_parameter_node(node)
      name = node.name

      if name
        group do
          visit_location(node.operator_loc)
          nest(1) do
            breakable_empty
            text(node.name.name)
          end
        end
      else
        visit_location(node.operator_loc)
      end
    end

    # retry
    # ^^^^^
    def visit_retry_node(node)
      text("retry")
    end

    # return
    # ^^^^^^
    #
    # return 1
    # ^^^^^^^^
    def visit_return_node(node)
      visit_jump("return", node.arguments)
    end

    # self
    # ^^^^
    def visit_self_node(node)
      text("self")
    end

    # class << self; end
    # ^^^^^^^^^^^^^^^^^^
    def visit_singleton_class_node(node)
      operator_loc = node.operator_loc

      group do
        group do
          text("class ")
          visit_location(operator_loc)

          if operator_loc.comments.any?
            indent do
              breakable_space
              visit(node.expression)
            end
          else
            text(" ")
            visit(node.expression)
          end
        end

        visit_body(node.body, node.end_keyword_loc.comments)
        breakable_force
        text("end")
      end
    end

    # __ENCODING__
    # ^^^^^^^^^^^^
    def visit_source_encoding_node(node)
      text("__ENCODING__")
    end

    # __FILE__
    # ^^^^^^^^
    def visit_source_file_node(node)
      text("__FILE__")
    end

    # __LINE__
    # ^^^^^^^^
    def visit_source_line_node(node)
      text("__LINE__")
    end

    # foo(*bar)
    #     ^^^^
    #
    # def foo((bar, *baz)); end
    #               ^^^^
    #
    # def foo(*); bar(*); end
    #                 ^
    def visit_splat_node(node)
      expression = node.expression
      operator_loc = node.operator_loc

      if expression
        group do
          text("*")
          operator_loc.comments.each { |comment| visit_comment(comment) }

          nest(1) do
            breakable_empty
            visit(expression)
          end
        end
      else
        text("*")
        operator_loc.comments.each { |comment| visit_comment(comment) }
      end
    end

    # A list of statements.
    def visit_statements_node(node)
      parent = stack[-2]

      line = nil
      previous_access_control = false

      node.body.each_with_index do |statement, index|
        if line.nil?
          visit(statement)
        elsif ((statement.location.start_line - line) > 1) || access_control?(statement) || previous_access_control
          breakable_force
          breakable_force
          visit(statement)
        elsif (statement.location.start_line != line) || !parent.is_a?(EmbeddedStatementsNode)
          breakable_force
          visit(statement)
        else
          text("; ")
          visit(statement)
        end

        line = statement.location.end_line
        previous_access_control = access_control?(statement)
      end
    end

    # "foo"
    # ^^^^^
    def visit_string_node(node)
      if node.heredoc?
        visit_heredoc(node, [node])
      else
        opening = node.opening
        content = node.content

        if !opening
          text(content)
        elsif opening == "?"
          if content.length == 1
            text("#{quote}#{content == quote ? "\\#{quote}" : content}#{quote}")
          else
            text("?#{content}")
          end
        else
          visit_string_node_parts(node, [node])
        end
      end
    end

    # super(foo)
    # ^^^^^^^^^^
    def visit_super_node(node)
      arguments = [*node.arguments]
      block = node.block

      if block.is_a?(BlockArgumentNode)
        arguments << block
        block = nil
      end

      group do
        text("super")

        if node.lparen_loc && node.rparen_loc
          text("(")

          if arguments.any?
            indent do
              breakable_empty
              seplist(arguments) { |argument| visit(argument) }
            end
            breakable_empty
          end

          text(")")
        elsif arguments.any?
          text(" ")
          nest(6) { seplist(arguments) { |argument| visit(argument) } }
        end

        if block
          text(" ")
          visit(block)
        end
      end
    end

    # :foo
    # ^^^^
    def visit_symbol_node(node)
      text(node.slice)
    end

    # true
    # ^^^^
    def visit_true_node(node)
      text("true")
    end

    # undef foo
    # ^^^^^^^^^
    def visit_undef_node(node)
      group do
        text("undef ")
        nest(6) do
          seplist(node.names) do |name|
            if name.is_a?(SymbolNode)
              text(name.value)

              if (comment = name.location.comments.first)
                visit_comment(comment)
              end
            else
              visit(name)
            end
          end
        end
      end
    end

    # unless foo; bar end
    # ^^^^^^^^^^^^^^^^^^^
    #
    # bar unless foo
    # ^^^^^^^^^^^^^^
    def visit_unless_node(node)
      statements = node.statements
      consequent = node.consequent

      if !statements || consequent || contains_conditional?(statements.body.first)
        group do
          visit_unless_node_break(node)
          break_parent
        end
      elsif contains_write?(node.predicate) || contains_write?(statements)
        if node.end_keyword_loc
          group do
            visit_unless_node_break(node)
            break_parent
          end
        else
          group { visit_unless_node_flat(node) }
        end
      else
        group do
          if_break { visit_unless_node_break(node) }
            .if_flat { ensure_parentheses { visit_unless_node_flat(node) } }
        end
      end
    end

    # Visit an unless node in the break form.
    private def visit_unless_node_break(node)
      statements = node.statements
      consequent = node.consequent

      group do
        visit_location(node.keyword_loc)
        text(" ")
        nest(3) { visit(node.predicate) }
      end

      if consequent
        visit_body(statements, [], false)
        breakable_space
        visit(consequent)
      else
        visit_body(statements, node.end_keyword_loc&.comments || [], false)
      end

      breakable_space
      text("end")
    end

    # Visit an unless node in the flat form.
    private def visit_unless_node_flat(node)
      visit(node.statements)
      text(" unless ")
      visit(node.predicate)
    end

    # until foo; bar end
    # ^^^^^^^^^^^^^^^^^
    #
    # bar until foo
    # ^^^^^^^^^^^^^
    def visit_until_node(node)
      statements = node.statements
      closing_loc = node.closing_loc

      if node.begin_modifier?
        group { visit_until_node_flat(node) }
      elsif statements.nil? || node.keyword_loc.comments.any? || closing_loc&.comments&.any?
        group do
          visit_until_node_break(node)
          break_parent
        end
      elsif contains_write?(node.predicate) || contains_write?(statements)
        if closing_loc
          group do
            visit_until_node_break(node)
            break_parent
          end
        else
          group { visit_until_node_flat(node) }
        end
      else
        group { if_break { visit_until_node_break(node) }.if_flat { visit_until_node_flat(node) } }
      end
    end

    # Visit an until node in the break form.
    private def visit_until_node_break(node)
      visit_location(node.keyword_loc)
      text(" ")
      nest(6) { visit(node.predicate) }
      visit_body(node.statements, node.closing_loc&.comments || [], false)
      breakable_space
      text("end")
    end

    # Visit an until node in the flat form.
    private def visit_until_node_flat(node)
      ensure_parentheses do
        visit(node.statements)
        text(" until ")
        visit(node.predicate)
      end
    end

    # case foo; when bar; end
    #           ^^^^^^^^^^^^^
    def visit_when_node(node)
      conditions = node.conditions
      statements = node.statements

      group do
        group do
          text("when ")
          nest(5) do
            seplist(conditions, -> { group { comma_breakable } }) do |condition|
              visit(condition)
            end

            # Very special case here. If you're inside of a when clause and the
            # last condition is an endless range, then you are forced to use the
            # "then" keyword to make it parse properly.
            last = conditions.last
            text(" then") if last.is_a?(RangeNode) && last.right.nil?
          end
        end

        if statements
          indent do
            breakable_force
            visit(statements)
          end
        end
      end
    end

    # while foo; bar end
    # ^^^^^^^^^^^^^^^^^^
    #
    # bar while foo
    # ^^^^^^^^^^^^^
    def visit_while_node(node)
      statements = node.statements
      closing_loc = node.closing_loc

      if node.begin_modifier?
        group { visit_while_node_flat(node) }
      elsif statements.nil? || node.keyword_loc.comments.any? || closing_loc&.comments&.any?
        group do
          visit_while_node_break(node)
          break_parent
        end
      elsif contains_write?(node.predicate) || contains_write?(statements)
        if closing_loc
          group do
            visit_while_node_break(node)
            break_parent
          end
        else
          group { visit_while_node_flat(node) }
        end
      else
        group { if_break { visit_while_node_break(node) }.if_flat { visit_while_node_flat(node) } }
      end
    end

    # Visit a while node in the flat form.
    private def visit_while_node_flat(node)
      ensure_parentheses do
        visit(node.statements)
        text(" while ")
        visit(node.predicate)
      end
    end

    # Visit a while node in the break form.
    private def visit_while_node_break(node)
      visit_location(node.keyword_loc)
      text(" ")
      nest(6) { visit(node.predicate) }
      visit_body(node.statements, node.closing_loc&.comments || [], false)
      breakable_space
      text("end")
    end

    # `foo`
    # ^^^^^
    def visit_x_string_node(node)
      if node.heredoc?
        visit_heredoc(node, [node])
      else
        text("`#{node.content}`")
      end
    end

    # yield
    # ^^^^^
    #
    # yield 1
    # ^^^^^^^
    def visit_yield_node(node)
      arguments = node.arguments

      if arguments.nil?
        text("yield")
      else
        lparen_loc = node.lparen_loc
        rparen_loc = node.rparen_loc

        group do
          text("yield")

          if lparen_loc
            visit_location(lparen_loc)
          else
            if_break { text("(") }.if_flat { text(" ") }
          end

          indent do
            breakable_empty
            visit(arguments)
          end
          breakable_empty

          if rparen_loc
            visit_location(rparen_loc)
          else
            if_break { text(")") }
          end
        end
      end
    end

    private

    ############################################################################
    # Doc builder methods                                                      #
    ############################################################################

    # This is a simplified version of prettyprint's group. It doesn't provide
    # any of the more advanced options because we don't need them and they take
    # up expensive computation time.
    def group
      contents = []
      doc = Group.new(contents: contents)

      groups << doc
      target << doc

      with_target(contents) { yield }
      groups.pop
      doc
    end

    # This is a much simplified version of prettier_print's text. It avoids
    # calculating width by pushing the string directly onto the target.
    def text(string)
      target << string
    end

    ############################################################################
    # Helper methods                                                           #
    ############################################################################

    # Returns whether or not the given statement is an access control statement.
    # Truthfully, we can't actually tell this for sure without performing method
    # lookup, but we assume none of these methods are overridden.
    def access_control?(statement)
      statement.is_a?(CallNode) &&
        statement.variable_call? &&
        %i[private protected public].include?(statement.name)
    end

    # There are times when it is useful to create string nodes so that
    # non-interpolated nodes can be formatted as if they were their interpolated
    # counterparts with a single part.
    def bare_string(node, content_loc, location)
      StringNode.new(node.send(:source), 0, nil, content_loc, nil, nil, location)
    end

    # True if the given node contains a conditional expression.
    def contains_conditional?(node)
      case node.type
      when :if_node, :unless_node
        true
      else
        false
      end
    end

    # True if the given node contains a write expression.
    def contains_write?(node)
      case node.type
      when :call_and_write_node, :call_or_write_node, :call_operator_write_node,
           :class_variable_write_node, :class_variable_and_write_node, :class_variable_or_write_node, :class_variable_operator_write_node,
           :constant_write_node, :constant_and_write_node, :constant_or_write_node, :constant_operator_write_node,
           :constant_path_write_node, :constant_path_and_write_node, :constant_path_or_write_node, :constant_path_operator_write_node,
           :global_variable_write_node, :global_variable_and_write_node, :global_variable_or_write_node, :global_variable_operator_write_node,
           :instance_variable_write_node, :instance_variable_and_write_node, :instance_variable_or_write_node, :instance_variable_operator_write_node,
           :local_variable_write_node, :local_variable_and_write_node, :local_variable_or_write_node, :local_variable_operator_write_node,
           :multi_write_node
        true
      when :class_node, :module_node, :singleton_class_node
        false
      else
        node.compact_child_nodes.any? { |child| contains_write?(child) }
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
    def ensure_parentheses
      case stack[-2]&.type
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

    # Returns whether or not the given node should be indented when it is
    # printed as the value of a write expression.
    def indent_write?(node)
      case node.type
      when :array_node
        node.opening_loc.nil?
      when :hash_node, :lambda_node, :parentheses_node
        false
      when :string_node, :x_string_node, :interpolated_string_node, :interpolated_x_string_node
        !node.heredoc?
      when :call_node
        node.receiver.nil? || indent_write?(node.receiver)
      when :interpolated_symbol_node
        node.opening_loc.nil? || !node.opening.start_with?("%s")
      else
        true
      end
    end

    # If there is some part of the string that matches an escape sequence or
    # that contains the interpolation pattern ("#{"), then we are locked into
    # whichever quote the user chose. (If they chose single quotes, then double
    # quoting would activate the escape sequence, and if they chose double
    # quotes, then single quotes would deactivate it.)
    def quotes_locked?(parts)
      parts.any? { |node| !node.is_a?(StringNode) || node.content.match?(/\\|#[@${]|#{quote}/) }
    end

    # The matching pairs of quotes that can be used with % literals.
    QUOTE_PAIRS = { "(" => ")", "[" => "]", "{" => "}", "<" => ">" }.freeze

    # Find the matching closing quote for the given opening quote.
    def quotes_matching(quote)
      QUOTE_PAIRS.fetch(quote) { quote }
    end

    # Escape and unescape single and double quotes as needed to be able to
    # enclose +content+ with +enclosing+.
    def quotes_normalize(content, enclosing)
      return content if enclosing != "\"" && enclosing != "'"
    
      content.gsub(/\\([\s\S])|(['"])/) do
        _match, escaped, quote = Regexp.last_match.to_a

        if quote == enclosing
          "\\#{quote}"
        elsif quote
          quote
        else
          "\\#{escaped}"
        end
      end
    end

    ############################################################################
    # Visit methods                                                            #
    ############################################################################

    # Visit a node and format it, including any comments that are found around
    # it that are attached to its location.
    def visit(node)
      stack << node

      node.location.leading_comments.each do |comment|
        if comment.is_a?(InlineComment)
          text(comment.location.slice)
          breakable(force: true)
        else
          breakable_force
          trim
          text(comment.location.slice.rstrip)
        end
      end

      doc = node.accept(self)

      node.location.trailing_comments.each do |comment|
        visit_comment(comment)
      end

      stack.pop
      doc
    end

    # Visit a binary expression, and format it with the given left and right
    # nodes, and the given operator location.
    def visit_binary(left, operator_loc, right)
      group do
        visit(left)
        text(" ")
        visit_location(operator_loc)
        indent do
          breakable_space
          visit(right)
        end
      end
    end

    # Visit the body of a node, and format it with the given comments.
    def visit_body(body, comments, force_break = true)
      break_parent if force_break && (body || comments.any?)

      indent do
        if body
          breakable_empty
          visit(body)
        end

        comments.each do |comment|
          if comment.is_a?(InlineComment)
            breakable_force
            text(comment.location.slice)
          else
            breakable_force
            trim
            text(comment.location.slice.rstrip)
          end
        end
      end
    end

    # Visit a comment and print it out.
    def visit_comment(comment)
      if !comment.is_a?(InlineComment)
        breakable_force
        trim
        text(comment.location.slice.rstrip)
      elsif comment.trailing?
        line_suffix(priority: COMMENT_PRIORITY) do
          text(" ")
          text(comment.location.slice)
          break_parent
        end
      else
        breakable
        text(comment.location.slice)
        break_parent
      end
    end

    # Visit a set of elements within a collection, along with the comments that
    # may be present within them.
    def visit_elements(elements, comments)
      indent do
        if elements.any?
          breakable_empty
          seplist(elements) { |element| visit(element) }
        end

        comments.each do |comment|
          breakable_force
          text(comment.location.slice)
        end
      end
    end

    # Visit a set of elements within a collection, along with the comments that
    # may be present within them. Additionally add a space before the start and
    # after the end of the collection.
    def visit_elements_spaced(elements, comments)
      indent do
        if elements.any?
          breakable_space
          seplist(elements) { |element| visit(element) }
        end

        comments.each do |comment|
          breakable_force
          text(comment.slice)
        end
      end
    end

    # Visit a heredoc node, and format it with the given parts.
    def visit_heredoc(node, parts)
      # This is a very specific behavior where you want to force a newline, but
      # don't want to force the break parent.
      separator = PrettierPrint::Breakable.new(" ", 1, indent: false, force: true)
      opening = node.opening

      # If the heredoc is indented, then we're going to need to reintroduce the
      # indentation to the parts of the heredoc.
      indent = ""
      if opening[2] == "~"
        parts.each do |part|
          if part.is_a?(StringNode) && !part.content.start_with?("\n")
            indent = part.content[/\A\s*/].delete_prefix(part.unescaped[/\A\s*/])
            break
          end
        end
      end

      group do
        text(opening)
        line_suffix(priority: HEREDOC_PRIORITY) do
          group do
            target << separator

            parts.each_with_index do |part, index|
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
                text(indent)
                visit(part)
              end
            end

            text(node.closing.chomp)
          end
        end
      end
    end

    # Visit a jump expression, which consists of a keyword followed by an
    # optional set of arguments.
    def visit_jump(keyword, arguments)
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
              visit(first)
            when :array_node
              if first.elements.length > 1
                group do
                  text(keyword)
                  if_break { text("[") }.if_flat { text(" ") }

                  indent do
                    breakable_empty
                    seplist(first.elements) { |element| visit(element) }
                  end

                  if_break do
                    breakable_empty
                    text("]")
                  end
                end
              else
                text("#{keyword} ")
                visit(first)
              end
            else
              group do
                text(keyword)
                visit(argument)
              end
            end
          else
            group do
              text(keyword)
              visit(argument)
            end
          end
        when :class_variable_read_node, :constant_read_node, :false_node,
             :float_node, :global_variable_read_node, :imaginary_node,
             :instance_variable_read_node, :integer_node,
             :local_variable_read_node, :nil_node, :rational_node, :self_node,
             :true_node
          text("#{keyword} ")
          visit(argument)
        when :array_node
          if argument.elements.length > 1
            group do
              text(keyword)
              if_break { text("[") }.if_flat { text(" ") }

              indent do
                breakable_empty
                seplist(argument.elements) { |element| visit(element) }
              end

              if_break do
                breakable_empty
                text("]")
              end
            end
          else
            text("#{keyword} ")
            visit(argument)
          end
        else
          group do
            text(keyword)
            if_break { text("(") }.if_flat { text(" ") }
  
            indent do
              breakable_empty
              visit(argument)
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
            visit(arguments)
          end

          if_break do
            breakable_empty
            text(")")
          end
        end
      end
    end

    # Print out a slice of the given location, and handle any attached trailing
    # comments that may be present.
    def visit_location(location, value = location.slice)
      trailing = []

      location.comments.each do |comment|
        if comment.location.start_offset < location.start_offset
          if comment.is_a?(InlineComment)
            text(comment.location.slice)
          else
            breakable_force
            trim
            text(comment.location.slice.rstrip)
          end
          breakable(force: true)
        else
          trailing << comment
        end
      end

      text(value)
      trailing.each { |comment| visit_comment(comment) }
    end

    # Visit a prefix expression, which consists of a single operator prefixing
    # a nested expression.
    def visit_prefix(operator_loc, value)
      if value
        group do
          visit_location(operator_loc)
          nest(operator_loc.length) do
            breakable_empty
            visit(value)
          end
        end
      else
        visit_location(operator_loc)
      end
    end

    # Visit the parts of a regular expression-like node.
    def visit_regular_expression_node_parts(node, parts)
      # If the first part of this regex is plain string content, we have a space
      # or an =, and we're contained within a command or command_call node, then
      # we want to use braces because otherwise we could end up with an
      # ambiguous operator, e.g. foo / bar/ or foo /=bar/
      ambiguous = (
        (part = parts.first) &&
        part.is_a?(StringNode) &&
        part.content.start_with?(" ", "=") &&
        stack[0...-1].reverse_each.any? do |parent|
          parent.is_a?(CallNode) && parent.arguments && parent.opening_loc.nil?
        end
      )

      braces = ambiguous || parts.any? { |part| part.is_a?(StringNode) && part.content.include?("/") }

      if braces && parts.any? { |part| part.is_a?(StringNode) && part.content.match?(/[{}]/) }
        group do
          text(node.opening)
          parts.each { |part| visit(part) }
          text(node.closing)
        end
      elsif braces
        group do
          text("%r{")

          if node.opening == "/"
            # If we're changing from a forward slash to a %r{, then we can
            # replace any escaped forward slashes with regular forward slashes.
            parts.each do |part|
              if part.is_a?(StringNode)
                first = true
                part.content.each_line(chomp: true) do |line|
                  if first
                    first = false
                  else
                    breakable_return
                  end

                  text(line.gsub("\\/", "/"))
                end
              else
                visit(part)
              end
            end
          else
            parts.each { |part| visit(part) }
          end

          text("}")
          text(node.closing[1..])
        end
      else
        group do
          text("/")
          parts.each { |part| visit(part) }
          text("/")
          text(node.closing[1..])
        end
      end
    end

    # Visit the parts of a string-like node.
    def visit_string_node_parts(node, parts)
      # First, if there is no opening quote, then this is either a part of a
      # %w/%W list or it is a string literal representing string concatenation.
      # Either way, we'll bail out and just print the string as is.
      opening = node.opening
      return group { parts.each { |part| visit(part) } } if opening.nil?

      # If we get here, then we're going to need to add quotes to the string. In
      # this case we'll determine which quotes to use. If it's possible for us
      # to switch the quotes, we'll use the preferred quote of the formatter and
      # re-escape the inner quotes. Otherwise, we'll use the same quotes as the
      # source.
      closing = node.closing
      opening_quote, closing_quote =
        if !quotes_locked?(parts)
          [quote, quote]
        elsif opening.start_with?("%")
          [opening, quotes_matching(opening[/%[qQ]?(.)/, 1])]
        else
          [opening, closing]
        end

      # Here we'll actually build the doc tree. This will involve a group that
      # is bound by the opening and closing quotes, and then we'll visit each
      # part of the string.
      group do
        text(opening_quote)

        parts.each do |part|
          if part.is_a?(StringNode)
            value = quotes_normalize(part.content, closing_quote)

            first = true
            value.each_line(chomp: true) do |line|
              if first
                first = false
              else
                breakable_return
              end

              text(line)
            end

            breakable_return if value.end_with?("\n")
          else
            visit(part)
          end
        end

        text(closing_quote)
      end
    end

    # Visit a write expression, and format it with the given operator and value.
    def visit_write(operator_loc, value)
      group do
        yield
        text(" ")
        visit_location(operator_loc)

        if indent_write?(value)
          indent do
            breakable_space
            visit(value)
          end
        else
          text(" ")
          visit(value)
        end
      end
    end
  end

  # This is a special node that we insert into the syntax tree to represent the
  # use of the __END__ syntax. We do this so that it can be treated effectively
  # as any other statement and all of the formatting logic can be reused.
  class DATANode
    attr_reader :location

    def initialize(location)
      @location = location
    end

    def accept(visitor)
      visitor.visit_data_node(self)
    end
  end

  class ParseResult
    def format
      attach_comments!
      formatter = Format.new(source.source, [], 80)

      root =
        if data_loc
          body = [*value.statements.body, DATANode.new(data_loc)]
          value.copy(statements: value.statements.copy(body: body))
        else
          value
        end

      root.accept(formatter)
      formatter.flush
      formatter.output.join
    end
  end
end
