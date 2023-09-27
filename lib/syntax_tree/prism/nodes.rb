# frozen_string_literal: true

module Prism
  class Comment
    def slice
      location.slice
    end

    def format(q)
      case type
      when :inline
        q.line_suffix(priority: Formatter::COMMENT_PRIORITY) do
          q.text(" ")
          q.text(slice)
          q.break_parent
        end
      end
    end
  end

  class AliasGlobalVariableNode
    # Represents the use of the `alias` keyword to alias a global variable.
    #
    #     alias $foo $bar
    #     ^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("alias ")
        q.format(new_name)
        q.nest(6) do
          q.breakable_space
          q.format(old_name)
        end
      end
    end
  end

  class AliasMethodNode
    # Represents the use of the `alias` keyword to alias a method.
    #
    #     alias foo bar
    #     ^^^^^^^^^^^^^
    def format(q)
      new_name = self.new_name
      old_name = self.old_name
      comment = nil

      q.group do
        q.text("alias ")

        if new_name.is_a?(SymbolNode)
          q.text("#{new_name.value}")
          new_name.location.comments.each { |comment| comment.format(q) }
        else
          q.format(new_name)
        end

        q.nest(6) do
          q.breakable_space
          if old_name.is_a?(SymbolNode)
            q.text(old_name.value)
          else
            q.format(old_name)
          end
        end
      end
    end
  end

  class AlternationPatternNode
    # Represents an alternation pattern in pattern matching.
    #
    #     foo => bar | baz
    #            ^^^^^^^^^
    def format(q)
      q.group do
        q.format(left)
        q.text(" ")
        q.loc(operator_loc)
        q.breakable_space
        q.format(right)
      end
    end
  end

  class AndNode
    # Represents the use of the `&&` operator or the `and` keyword.
    #
    #     left and right
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.format_binary(left, operator_loc, right)
    end
  end

  class ArgumentsNode
    # Represents a set of arguments to a method or a keyword.
    #
    #     return foo, bar, baz
    #            ^^^^^^^^^^^^^
    def format(q)
      q.seplist(arguments) { |argument| q.format(argument) }
    end
  end

  class ArrayNode
    # Represents an array literal. This can be a regular array using brackets or
    # a special array using % like %w or %i.
    #
    #     [1, 2, 3]
    #     ^^^^^^^^^
    def format(q)
      opening_loc = self.opening_loc

      unless opening_loc
        q.group { q.seplist(elements) { |element| q.format(element) } }
        return
      end

      opening = opening_loc.slice

      if opening.start_with?("%")
        q.group do
          q.text(opening)
          q.indent do
            q.breakable_empty
            q.seplist(elements, ->(q) { q.breakable_space }) { |element| q.format(element) }
          end
          q.breakable_empty
          q.text(closing)
        end
        return
      end

      closing_loc = self.closing_loc
      elements = self.elements

      if opening_loc.comments.empty? && closing_loc.comments.empty? && elements.length >= 2
        if elements.all? { |element| element.is_a?(StringNode) && element.location.comments.empty? && !element.content.match?(/[\s\[\]\\]/) }
          q.group do
            q.text("%w[")
            q.indent do
              q.breakable_empty
              q.seplist(elements, ->(q) { q.breakable_space }) do |element|
                q.text(element.content)
              end
            end
            q.breakable_empty
            q.text("]")
          end
          return
        elsif elements.all? { |element| element.is_a?(SymbolNode) && element.location.comments.empty? }
          q.group do
            q.text("%i[")
            q.indent do
              q.breakable_empty
              q.seplist(elements, ->(q) { q.breakable_space }) do |element|
                q.text(element.value)
              end
            end
            q.breakable_empty
            q.text("]")
          end
          return
        end
      end

      q.group do
        q.loc(opening_loc)
        q.format_body_list_empty(elements, closing_loc.comments)
        q.breakable_empty
        q.text("]")
      end
    end

    test("[]")
    test("[ # comment\n]")
    test("[\n  # comment1\n  # comment2\n]")
    test("[] # comment")
    test("[\n  {{long}}\n] # comment")

    test("[1, 2, 3]")
    test("[1, 2, 3]", "[1, 2, 3,]")
    test("[\n  1,\n  {{long}},\n  3\n]", "[1, {{long}}, 3]")

    test("%w[foo bar baz]")
    test("%w[\n  foo\n  {{long}}\n  baz\n]", "%w[foo {{long}} baz]")

    test("%W[foo bar baz]")
    test("%W[\n  foo\n  {{long}}\n  baz\n]", "%W[foo {{long}} baz]")

    test("%i[foo bar baz]")
    test("%i[\n  foo\n  {{long}}\n  baz\n]", "%i[foo {{long}} baz]")

    test("%I[foo bar baz]")
    test("%I[\n  foo\n  {{long}}\n  baz\n]", "%I[foo {{long}} baz]")

    test("foo = 1, 2, 3", &:value)
    test("foo =\n  1,\n  2, # comment\n  3", "foo = 1, 2, # comment\n3", &:value)
  end

  class ArrayPatternNode
    # Represents an array pattern in pattern matching.
    #
    #     foo in 1, 2
    #     ^^^^^^^^^^^
    #
    #     foo in [1, 2]
    #     ^^^^^^^^^^^^^
    #
    #     foo in *1
    #     ^^^^^^^^^
    #
    #     foo in Bar[]
    #     ^^^^^^^^^^^^
    #
    #     foo in Bar[1, 2, 3]
    #     ^^^^^^^^^^^^^^^^^^^
    def format(q)
      constant = self.constant
      opening_loc = self.opening_loc
      closing_loc = self.closing_loc

      q.group do
        q.format(constant) if constant
        q.text("[")
        opening_loc.comments.each { |comment| comment.format(self) } if opening_loc
        q.format_body_list_empty([*requireds, *rest, *posts], closing_loc ? closing_loc.comments : [])
        q.breakable_empty
        q.text("]")
      end
    end
  end

  class AssocNode
    # Represents a hash key/value pair.
    #
    #     { a => b }
    #       ^^^^^^
    def format(q)
      if operator_loc
        q.format(key)
        q.text(" #{operator}")
        q.breakable_space
        q.format(value)
      else
        q.format(key)

        if value && !value.is_a?(ImplicitNode)
          q.text(" ")
          q.format(value)
        end
      end
    end
  end

  class AssocSplatNode
    # Represents a splat in a hash literal.
    #
    #     { **foo }
    #       ^^^^^
    def format(q)
      q.format_prefix(operator_loc, value)
    end

    test("foo in { bar: :baz, ** }") { |node| node.pattern.assocs.last }
    test("{ **foo }") { |node| node.elements.first }
    test("{\n  ** # comment\n    foo\n}", "{ ** # comment\nfoo }") { |node| node.elements.first }
  end

  class BackReferenceReadNode
    # Represents reading a field out of the previous match.
    #
    #     $+
    #     ^^
    def format(q)
      q.text(slice)
    end

    test("$+")
    test("$+ # comment")
  end

  class BeginNode
    # Represents a begin statement.
    #
    #     begin
    #       foo
    #     end
    #     ^^^^^
    def format(q)
      begin_keyword_loc = self.begin_keyword_loc

      if begin_keyword_loc
        q.group do
          q.loc(begin_keyword_loc)

          if statements
            q.indent do
              q.breakable_force
              q.format(statements)
            end
          end

          if rescue_clause
            q.breakable_force
            q.format(rescue_clause)
          end

          if else_clause
            q.breakable_force
            q.format(else_clause)
          end

          if ensure_clause
            q.breakable_force
            q.format(ensure_clause)
          end

          q.breakable_force
          q.text("end")
        end
      else
        q.group do
          if statements
            q.format(statements)
          end

          if rescue_clause
            q.nest(-2) do
              q.breakable_force
              q.format(rescue_clause)
            end
          end

          if else_clause
            q.nest(-2) do
              q.breakable_force
              q.format(else_clause)
            end
          end

          if ensure_clause
            q.nest(-2) do
              q.breakable_force
              q.format(ensure_clause)
            end
          end
        end
      end
    end
  end

  class BlockArgumentNode
    # Represents block method arguments.
    #
    #     bar(&args)
    #         ^^^^^
    def format(q)
      q.format_prefix(operator_loc, expression)
    end
  end

  class BlockLocalVariableNode
    # Represents a block local variable.
    #
    #     a { |; b| }
    #            ^
    def format(q)
      q.text(name.name)
    end

    test("a { |; b| }") { |node| node.block.parameters.locals.first }
  end

  class BlockNode
    # Represents a block of ruby code.
    #
    #    [1, 2, 3].each { |i| puts x }
    #                   ^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q
          .if_break do
            q.text("do")

            if parameters
              q.text(" ")
              q.format(parameters)
            end

            if body
              q.indent do
                q.breakable_space
                q.format(body)
              end
            end

            q.breakable_space
            q.text("end")
          end
          .if_flat do
            q.text("{")

            if parameters
              q.text(" ")
              q.format(parameters)
            end

            if body
              q.text(" ")
              q.format(body)
            end

            q.text(" }")
          end
      end
    end
  end

  class BlockParameterNode
    # Represents a block parameter to a method, block, or lambda definition.
    #
    #     def a(&b)
    #           ^^
    #     end
    def format(q)
      name = self.name

      q.group do
        q.loc(operator_loc)

        if name
          q.nest(1) do
            q.breakable_empty
            q.text(name.name)
          end
        end
      end
    end
  end

  class BlockParametersNode
    # Represents a block's parameters declaration.
    #
    #     -> (a, b = 1; c) {}
    #     ^^^^^^^^^^^^^^^^^^^
    #
    #     foo do |a, b = 1; c|
    #            ^^^^^^^^^^^^^
    #     end
    def format(q)
      parameters = self.parameters
      locals = self.locals
      opening_loc = self.opening_loc

      q.group do
        if parameters || locals.any?
          if opening_loc
            q.loc(opening_loc)
          else
            q.text("(")
          end
        end

        q.format(parameters) if parameters

        if locals.any?
          q.text("; ")
          q.seplist(locals) { |local| q.format(local) }
        end

        q.text(closing || ")") if parameters || locals.any?
      end
    end
  end

  class BreakNode
    # Represents the use of the `break` keyword.
    #
    #     break foo
    #     ^^^^^^^^^
    def format(q)
      q.format_jump("break", arguments)
    end
  end

  class CallNode
    # Represents a method call, in all of the various forms that can take.
    #
    #       foo
    #       ^^^
    #
    #       foo()
    #       ^^^^^
    #
    #       +foo
    #       ^^^^
    #
    #       foo + bar
    #       ^^^^^^^^^
    #
    #       foo.bar
    #       ^^^^^^^
    #
    #       foo&.bar
    #       ^^^^^^^^
    def format(q)
      message = self.message
      name = self.name

      arguments = [*self.arguments&.arguments]
      block = self.block

      if block.is_a?(BlockArgumentNode)
        arguments << block
        block = nil
      end

      unless safe_navigation?
        case name.to_sym
        when :!
          if message == "not"
            if receiver
              q.group do
                q.text("not")
                q.if_break { q.text("(") }.if_flat { q.text(" ") }
                q.indent do
                  q.breakable_empty
                  q.format(receiver)
                end
                q.breakable_empty
                q.if_break { q.text(")") }
              end
            else
              q.text("not()")
            end

            return
          end

          if arguments.empty? && block.nil?
            q.format_prefix(message_loc, receiver)
            return
          end
        when :+@, :-@, :~
          if arguments.empty? && block.nil?
            q.format_prefix(message_loc, receiver)
            return
          end
        when :+, :-, :*, :/, :==, :>, :<, :>=, :<=, :<=>, :<<, :>>
          if arguments.length == 1 && block.nil?
            q.format_binary(receiver, message_loc, arguments.first)
            return
          end
        when :**
          if arguments.length == 1 && block.nil?
            q.group do
              q.format(receiver)
              q.text("**")
              q.indent do
                q.breakable_empty
                q.seplist(arguments) { |argument| q.format(argument) }
              end
            end

            return
          end
        when :[]
          q.group do
            q.format(receiver)
            q.text("[")

            if arguments.any?
              q.indent do
                q.breakable_empty
                q.seplist(arguments) { |argument| q.format(argument) }
              end

              q.breakable_empty
            end

            q.text("]")

            if block
              q.text(" ")
              q.format(block)
            end
          end

          return
        when :[]=
          if arguments.any?
            q.group do
              *before, after = arguments

              q.group do
                q.format(receiver)
                q.text("[")

                if before.any?
                  q.indent do
                    q.breakable_empty
                    q.seplist(before) { |argument| q.format(argument) }
                  end
                  q.breakable_empty
                end

                q.text("]")
              end

              q.text(" ")
              q.group do
                q.text("=")
                q.indent do
                  q.breakable_space
                  q.format(after)
                end
              end

              if block
                q.text(" ")
                q.format(block)
              end
            end
          else
            q.group do
              q.format(receiver)
              q.text("[]")

              if block
                q.text(" ")
                q.format(block)
              end
            end
          end

          return
        when :call
        end
      end

      q.group do
        doc =
          q.nest(0) do
            if receiver
              q.format(receiver)
              q.text(call_operator == "&." ? "&." : ".")
            end

            q.text(message)
          end

        if arguments.length == 1 && name.end_with?("=") && block.nil?
          q.text(" =")
          q.indent do
            q.breakable_space
            q.seplist(arguments) { |argument| q.format(argument) }
          end
        elsif opening_loc && arguments.any? && closing_loc
          q.text("(")
          q.indent do
            q.breakable_empty
            q.seplist(arguments) { |argument| q.format(argument) }
          end
          q.breakable_empty
          q.text(")")
        elsif arguments.any?
          q.text(" ")
          align(q, self, q.last_position(doc))
        elsif opening_loc && closing_loc
          q.text("()")
        end

        if block
          q.text(" ")
          q.format(block)
        end
      end
    end

    private

    def align(q, node, last)
      if node.arguments && node.arguments.arguments.length == 1
        argument = node.arguments.arguments.first

        if argument.is_a?(DefNode)
          q.format(arguments)
        elsif argument.is_a?(CallNode) && !argument.opening_loc
          align(q, argument, last)
        end
      else
        width = last + 1
        q.nest(width > (q.maxwidth / 2) ? 0 : width) { q.format(arguments) }
      end
    end
  end

  class CallAndWriteNode
    # Represents the use of the `&&=` operator on a call.
    #
    #     foo.bar &&= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) do
        q.group do
          if receiver
            q.format(receiver)
            q.text(call_operator == "&." ? "&." : ".")
          end

          q.text(message)
        end
      end
    end
  end

  class CallOrWriteNode
    # Represents the use of the `||=` operator on a call.
    #
    #     foo.bar ||= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) do
        q.group do
          if receiver
            q.format(receiver)
            q.text(call_operator == "&." ? "&." : ".")
          end

          q.text(message)
        end
      end
    end
  end

  class CallOperatorWriteNode
    # Represents the use of an assignment operator on a call.
    #
    #     foo.bar += baz
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) do
        q.group do
          if receiver
            q.format(receiver)
            q.text(call_operator == "&." ? "&." : ".")
          end

          q.text(message)
        end
      end
    end
  end

  class CapturePatternNode
    # Represents assigning to a local variable in pattern matching.
    #
    #     foo => [bar => baz]
    #             ^^^^^^^^^^
    def format(q)
      q.format_binary(value, operator_loc, target)
    end
  end

  class CaseNode
    # Represents the use of the `case` keyword.
    #
    #     case true
    #     when false
    #     end
    #     ^^^^^^^^^^
    def format(q)
      q.group do
        q.group do
          q.loc(case_keyword_loc)

          if predicate
            q.text(" ")
            q.nest(5) { q.format(predicate) }
          end
        end

        q.breakable_force
        q.seplist(conditions, ->(q) { q.breakable_force }) do |condition|
          q.format(condition)
        end

        if consequent
          q.breakable_force
          q.format(consequent)
        end

        q.indent do
          end_keyword_loc.comments.each do |comment|
            q.breakable_force
            q.text(comment.slice)
          end
        end

        q.breakable_force
        q.text("end")
      end
    end
  end

  class ClassNode
    # Represents a class declaration involving the `class` keyword.
    #
    #    class Foo end
    #    ^^^^^^^^^^^^^
    def format(q)
      class_keyword_loc = self.class_keyword_loc
      inheritance_operator_loc = self.inheritance_operator_loc
      superclass = self.superclass

      q.group do
        q.group do
          q.loc(class_keyword_loc)

          if class_keyword_loc.comments.any?
            q.indent do
              q.breakable_space
              q.format(constant_path)
            end
          else
            q.text(" ")
            q.format(constant_path)
          end

          if superclass
            q.text(" ")
            q.loc(inheritance_operator_loc)

            if inheritance_operator_loc.comments.any?
              q.indent do
                q.breakable_space
                q.format(superclass)
              end
            else
              q.text(" ")
              q.format(superclass)
            end
          end
        end

        q.format_body(body, end_keyword_loc.comments)
        q.breakable_force
        q.text("end")
      end
    end
  end

  class ClassVariableReadNode
    # Represents referencing a class variable.
    #
    #     @@foo
    #     ^^^^^
    def format(q)
      q.text(name.name)
    end

    test("@@foo")
    test("@@foo # comment")
  end

  class ClassVariableTargetNode
    # Represents writing to a class variable in a context that doesn't have an
    # explicit value.
    #
    #     @@foo, @@bar = baz
    #     ^^^^^  ^^^^^
    def format(q)
      q.text(name.name)
    end

    test("@@foo, @@bar = baz") { |node| node.targets.first }
  end

  class ClassVariableWriteNode
    # Represents writing to a class variable.
    #
    #     @@foo = 1
    #     ^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("@@foo = 1")
    test("@@foo = # comment\n  1")
    test("@@foo = 1 # comment")
  end

  class ClassVariableAndWriteNode
    # Represents the use of the `&&=` operator for assignment to a class
    # variable.
    #
    #     @@target &&= value
    #     ^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("@@foo &&= 1")
    test("@@foo &&= # comment\n  1")
    test("@@foo &&= 1 # comment")
  end

  class ClassVariableOrWriteNode
    # Represents the use of the `||=` operator for assignment to a class
    # variable.
    #
    #     @@target ||= value
    #     ^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("@@foo ||= 1")
    test("@@foo ||= # comment\n  1")
    test("@@foo ||= 1 # comment")
  end

  class ClassVariableOperatorWriteNode
    # Represents assigning to a class variable using an operator that isn't `=`.
    #
    #     @@target += value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("@@foo += 1")
    test("@@foo += # comment\n  1")
    test("@@foo += 1 # comment")
  end

  class ConstantReadNode
    # Represents referencing a constant.
    #
    #     Foo
    #     ^^^
    def format(q)
      q.text(name.name)
    end

    test("Foo")
    test("Foo # comment")
  end

  class ConstantTargetNode
    # Represents writing to a constant in a context that doesn't have an
    # explicit value.
    #
    #     Foo, Bar = baz
    #     ^^^  ^^^
    def format(q)
      q.text(name.name)
    end

    test("Foo, Bar = baz") { |node| node.targets.first }
  end

  class ConstantWriteNode
    # Represents writing to a constant.
    #
    #     Foo = 1
    #     ^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("Foo = 1")
    test("Foo = # comment\n  1")
    test("Foo = 1 # comment")
  end

  class ConstantAndWriteNode
    # Represents the use of the `&&=` operator for assignment to a constant.
    #
    #     Target &&= value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("Foo &&= 1")
    test("Foo &&= # comment\n  1")
    test("Foo &&= 1 # comment")
  end

  class ConstantOrWriteNode
    # Represents the use of the `||=` operator for assignment to a constant.
    #
    #     Target ||= value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("Foo ||= 1")
    test("Foo ||= # comment\n  1")
    test("Foo ||= 1 # comment")
  end

  class ConstantOperatorWriteNode
    # Represents assigning to a constant using an operator that isn't `=`.
    #
    #     Target += value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end

    test("Foo += 1")
    test("Foo += # comment\n  1")
    test("Foo += 1 # comment")
  end

  class ConstantPathNode
    # Represents accessing a constant through a path of `::` operators.
    #
    #     Foo::Bar
    #     ^^^^^^^^
    def format(q)
      q.group do
        q.format(parent) if parent
        q.loc(delimiter_loc)
        q.indent do
          q.breakable_empty
          q.format(child)
        end
      end
    end
  end

  class ConstantPathTargetNode
    # Represents writing to a constant path in a context that doesn't have an
    # explicit value.
    #
    #     Foo::Foo, Bar::Bar = baz
    #     ^^^^^^^^  ^^^^^^^^
    def format(q)
      q.group do
        q.format(parent) if parent
        q.loc(delimiter_loc)
        q.indent do
          q.breakable_empty
          q.format(child)
        end
      end
    end
  end

  class ConstantPathWriteNode
    # Represents writing to a constant path.
    #
    #     ::Foo = 1
    #     ^^^^^^^^^
    #
    #     Foo::Bar = 1
    #     ^^^^^^^^^^^^
    #
    #     ::Foo::Bar = 1
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.format(target) }
    end
  end

  class ConstantPathAndWriteNode
    # Represents the use of the `&&=` operator for assignment to a constant
    # path.
    #
    #     Parent::Child &&= value
    #     ^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.format(target) }
    end
  end

  class ConstantPathOrWriteNode
    # Represents the use of the `||=` operator for assignment to a constant
    # path.
    #
    #     Parent::Child ||= value
    #     ^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.format(target) }
    end
  end

  class ConstantPathOperatorWriteNode
    # Represents assigning to a constant path using an operator that isn't `=`.
    #
    #     Parent::Child += value
    #     ^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.format(target) }
    end
  end

  class DefinedNode
    # Represents the use of the `defined?` keyword.
    #
    #     defined?(a)
    #     ^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("defined?")
        q.loc(lparen_loc)
        q.format_body(value, rparen_loc.comments, false)
        q.breakable_empty
        q.text(")")
      end
    end
  end

  class DefNode
    # Represents a method definition.
    #
    #     def method
    #     end
    #     ^^^^^^^^^^
    def format(q)
      q.group do
        q.group do
          q.text("def ")

          if receiver
            q.format(receiver)
            q.text(".")
          end

          q.text(name.name)

          if parameters
            lparen_loc ? q.loc(lparen_loc) : q.text("(")
            q.indent do
              q.breakable_empty
              q.format(parameters)
            end
            q.breakable_empty
            rparen_loc ? q.loc(rparen_loc) : q.text(")")
          else
            q.loc(lparen_loc) if lparen_loc
            q.loc(rparen_loc) if rparen_loc
          end
        end

        if equal_loc
          q.text(" = ")
          q.format(body)
        else
          q.format_body(body, end_keyword_loc.comments)
          q.breakable_force
          q.text("end")
        end
      end
    end
  end

  class ElseNode
    # Represents an `else` clause in a `case`, `if`, or `unless` statement.
    #
    #     if a then b else c end
    #                 ^^^^^^^^^^
    def format(q)
      q.group do
        q.loc(else_keyword_loc)
        q.format_body(statements, end_keyword_loc.comments)
      end
    end
  end

  class EmbeddedStatementsNode
    # Represents an interpolated set of statements.
    #
    #     "foo #{bar}"
    #          ^^^^^^
    def format(q)
      q.group do
        q.loc(opening_loc)
        q.indent do
          q.breakable_empty
          q.format(statements)
        end
        q.breakable_empty
        q.text("}")
      end
    end
  end

  class EmbeddedVariableNode
    # Represents an interpolated variable.
    #
    #     "foo #@bar"
    #          ^^^^^
    def format(q)
      q.group do
        q.text("\#{")
        q.indent do
          q.breakable_empty
          q.format(variable)
        end
        q.breakable_empty
        q.text("}")
      end
    end
  end

  class EnsureNode
    # Represents an `ensure` clause in a `begin` statement.
    #
    #     begin
    #       foo
    #     ensure
    #       bar
    #     ^^^^^^
    #     end
    def format(q)
      q.group do
        q.loc(ensure_keyword_loc)
        q.format_body(statements, end_keyword_loc.comments)
      end
    end
  end

  class FalseNode
    # Represents the use of the literal `false` keyword.
    #
    #     false
    #     ^^^^^
    def format(q)
      q.text("false")
    end
  end

  class FindPatternNode
    # Represents a find pattern in pattern matching.
    #
    #     foo in *bar, baz, *qux
    #     ^^^^^^^^^^^^^^^^^^^^^^
    #
    #     foo in [*bar, baz, *qux]
    #     ^^^^^^^^^^^^^^^^^^^^^^^^
    #
    #     foo in Foo(*bar, baz, *qux)
    #     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.format(constant) if constant
        q.text("[")

        q.indent do
          q.breakable_empty
          q.seplist([left, *requireds, right]) { |element| q.format(element) }
        end

        q.breakable_empty
        q.text("]")
      end
    end
  end

  class FlipFlopNode
    # Represents the use of the `..` or `...` operators to create flip flops.
    #
    #     baz if foo .. bar
    #            ^^^^^^^^^^
    def format(q)
      q.group do
        q.format(left) if left
        q.text(" #{operator} ")
        q.format(right) if right
      end
    end
  end

  class FloatNode
    # Represents a floating point number literal.
    #
    #     1.0
    #     ^^^
    def format(q)
      q.text(slice)
    end
  end

  class ForNode
    # Represents the use of the `for` keyword.
    #
    #     for i in a end
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("for ")
        q.group { q.format(index) }
        q.text(" in ")
        q.group { q.format(collection) }
        q.format_body(statements, end_keyword_loc.comments)
        q.breakable_force
        q.text("end")
      end
    end
  end

  class ForwardingArgumentsNode
    # Represents forwarding all arguments to this method to another method.
    #
    #     def foo(...)
    #       bar(...)
    #       ^^^^^^^^
    #     end
    def format(q)
      q.text("...")
    end
  end

  class ForwardingParameterNode
    # Represents the use of the forwarding parameter in a method, block, or
    # lambda declaration.
    #
    #     def foo(...)
    #             ^^^
    #     end
    def format(q)
      q.text("...")
    end
  end

  class ForwardingSuperNode
    # Represents the use of the `super` keyword without parentheses or
    # arguments.
    #
    #     super
    #     ^^^^^
    def format(q)
      if block
        q.group do
          q.text("super ")
          q.format(block)
        end
      else
        q.text("super")
      end
    end
  end

  class GlobalVariableReadNode
    # Represents referencing a global variable.
    #
    #     $foo
    #     ^^^^
    def format(q)
      q.text(name.name)
    end
  end

  class GlobalVariableTargetNode
    # Represents writing to a global variable in a context that doesn't have an
    # explicit value.
    #
    #     $foo, $bar = baz
    #     ^^^^  ^^^^
    def format(q)
      q.text(name.name)
    end
  end

  class GlobalVariableWriteNode
    # Represents writing to a global variable.
    #
    #     $foo = 1
    #     ^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class GlobalVariableAndWriteNode
    # Represents the use of the `&&=` operator for assignment to a global
    # variable.
    # 
    #     $target &&= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class GlobalVariableOrWriteNode
    # Represents the use of the `||=` operator for assignment to a global
    # variable.
    # 
    #     $target ||= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class GlobalVariableOperatorWriteNode
    # Represents assigning to a global variable using an operator that isn't
    # `=`.
    # 
    #     $target += value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class HashNode
    # Represents a hash literal.
    #
    #     { a => b }
    #     ^^^^^^^^^^
    def format(q)
      elements = self.elements

      if elements.any? { |element| element.value.is_a?(ImplicitNode) }
        format_identity(q)
      elsif elements.all? { |element| !element.is_a?(AssocNode) || element.operator_loc.nil? || element.key.is_a?(InterpolatedSymbolNode) || (element.key.is_a?(SymbolNode) && (value = element.key.value) && value.match?(/^[_A-Za-z]/) && !value.end_with?("=")) }
        format_labels(q)
      else
        format_rockets(q)
      end
    end

    private

    def format_layout(q)
      elements = self.elements

      q.group do
        q.loc(opening_loc)
        q.indent do
          if elements.any?
            q.breakable_space
            q.seplist(elements) do |element|
              if element.is_a?(AssocNode)
                yield element
              else
                q.format(element)
              end
            end
          end

          closing_loc.comments.each do |comment|
            q.breakable_force
            q.text(comment.slice)
          end
        end

        elements.any? ? q.breakable_space : q.breakable_empty
        q.text("}")
      end
    end

    def format_identity(q)
      format_layout(q) { |element| q.format(element) }
    end

    def format_labels(q)
      format_layout(q) do |element|
        if element.operator_loc.nil?
          q.format(element)
        else
          key = element.key

          if key.is_a?(InterpolatedSymbolNode)
            opening = key.opening

            if opening.start_with?("%")
              q.text("\"")
              key.parts.each { |part| q.format(part) }
              q.text("\":")
            else
              q.group do
                q.text(key.opening[1..])
                key.parts.each { |part| q.format(part) }
                q.text(key.closing)
                q.text(":")
              end
            end
          elsif key.is_a?(SymbolNode)
            q.text(key.value)
            q.text(":")
          else
            raise "Unexpected key: #{key.inspect}"
          end

          q.indent do
            q.breakable_space
            q.format(element.value)
          end
        end
      end
    end

    def format_rockets(q)
      format_layout(q) do |element|
        key = element.key

        if key.is_a?(InterpolatedSymbolNode)
          opening = key.opening

          if opening.start_with?("%")
            q.format(key)
          else
            q.group do
              q.text(opening)
              key.parts.each { |part| q.format(part) }
              q.text(key.closing)
            end
          end
        elsif key.is_a?(SymbolNode)
          q.text(":")
          q.text(key.value)
        else
          q.format(key)
        end

        q.text(" ")
        operator_loc = element.operator_loc
        operator_loc ? q.loc(operator_loc) : q.text("=>")

        q.indent do
          q.breakable_space
          q.format(element.value)
        end
      end
    end
  end

  class HashPatternNode
    # Represents a hash pattern in pattern matching.
    #
    #     foo => { a: 1, b: 2 }
    #            ^^^^^^^^^^^^^^
    #
    #     foo => { a: 1, b: 2, **c }
    #            ^^^^^^^^^^^^^^^^^^^
    def format(q)
      constant = self.constant
      opening_loc = self.opening_loc
      closing_loc = self.closing_loc

      elements = [*assocs, *kwrest]

      if constant
        q.group do
          q.format(constant)
          q.text("[")
          opening_loc.comments.each { |comment| comment.format(self) } if opening_loc
          q.format_body_list_empty(elements, closing_loc ? closing_loc.comments : [])
          q.breakable_empty
          q.text("]")
        end
      else
        q.group do
          q.text("{")
          opening_loc.comments.each { |comment| comment.format(self) } if opening_loc
          q.format_body_list_space(elements, closing_loc ? closing_loc.comments : [])
          elements.any? ? q.breakable_space : q.breakable_empty
          q.text("}")
        end
      end
    end
  end

  class IfNode
    # Represents the use of the `if` keyword, either in the block form or the
    # modifier form.
    #
    #     bar if foo
    #     ^^^^^^^^^^
    #
    #     if foo then bar end
    #     ^^^^^^^^^^^^^^^^^^^
    def format(q)
      if !statements
        q.group do
          q.text("if ")
          q.nest(3) { q.format(predicate) }
          q.breakable_force
          q.text("end")
        end
      elsif if_keyword_loc
        q.group do
          q
            .if_break do
              q.group do
                q.text("if ")
                q.nest(3) { q.format(predicate) }
              end

              q.indent do
                q.breakable_space
                q.format(statements)
              end

              if consequent
                q.breakable_space
                q.format(consequent)
              end

              q.breakable_space
              q.text("end")
            end
            .if_flat do
              q.format(statements)
              q.text(" if ")
              q.format(predicate)
            end
        end
      else
        q.group do
          q.format(predicate)
          q.text(" ? ")
          q.format(statements.body.first)
          q.text(" : ")
          q.format(consequent.statements.body.first)
        end
      end
    end
  end

  class ImaginaryNode
    # Represents an imaginary number literal.
    #
    #     1i
    #     ^^
    def format(q)
      q.text(slice)
    end

    test("1i")
    test("1i # comment")
  end

  class ImplicitNode
    # Represents a node that is implicitly being added to the tree but doesn't
    # correspond directly to a node in the source.
    #
    #     { foo: }
    #       ^^^^
    #
    #     { Foo: }
    #       ^^^^
    def format(q)
    end
  end

  class InNode
    # Represents the use of the `in` keyword in a case statement.
    #
    #     case a; in b then c end
    #             ^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("in ")
        q.nest(3) { q.format(pattern) }

        if statements
          q.indent do
            q.breakable_force
            q.format(statements)
          end
        end
      end
    end
  end

  class InstanceVariableReadNode
    # Represents referencing an instance variable.
    #
    #     @foo
    #     ^^^^
    def format(q)
      q.text(name.name)
    end
  end

  class InstanceVariableTargetNode
    # Represents writing to an instance variable in a context that doesn't have
    # an explicit value.
    #
    #     @foo, @bar = baz
    #     ^^^^  ^^^^
    def format(q)
      q.text(name.name)
    end
  end

  class InstanceVariableWriteNode
    # Represents writing to an instance variable.
    #
    #     @foo = 1
    #     ^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class InstanceVariableAndWriteNode
    # Represents the use of the `&&=` operator for assignment to an instance
    # variable.
    #
    #     @target &&= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class InstanceVariableOrWriteNode
    # Represents the use of the `||=` operator for assignment to an instance
    # variable.
    #
    #     @target ||= value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class InstanceVariableOperatorWriteNode
    # Represents assigning to an instance variable using an operator that isn't
    # `=`.
    #
    #     @target += value
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class InterpolatedMatchLastLineNode
    # Represents a regular expression literal that contains interpolation that
    # is being used in the predicate of a conditional to implicitly match
    # against the last line read by an IO object.
    #
    #     if /foo #{bar} baz/ then end
    #        ^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text(opening) if opening_loc
        parts.each { |part| q.format(part) }
        q.text(closing) if closing_loc
      end
    end
  end

  class InterpolatedRegularExpressionNode
    # Represents a regular expression literal that contains interpolation.
    #
    #     /foo #{bar} baz/
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text(opening) if opening_loc
        parts.each { |part| q.format(part) }
        q.text(closing) if closing_loc
      end
    end
  end

  class InterpolatedStringNode
    # Represents a string literal that contains interpolation.
    #
    #     "foo #{bar} baz"
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      if opening&.start_with?("<<")
        q.format_heredoc(self, parts)
      else
        q.group do
          q.text(opening) if opening_loc
          parts.each { |part| q.format(part) }
          q.text(closing) if closing_loc
        end
      end
    end
  end

  class InterpolatedSymbolNode
    # Represents a symbol literal that contains interpolation.
    #
    #     :"foo #{bar} baz"
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text(opening) if opening_loc
        parts.each { |part| q.format(part) }
        q.text(closing) if closing_loc
      end
    end
  end

  class InterpolatedXStringNode
    # Represents an xstring literal that contains interpolation.
    #
    #     `foo #{bar} baz`
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      if opening.start_with?("<<")
        q.format_heredoc(self, parts)
      else
        q.group do
          q.text(opening)
          parts.each { |part| q.format(part) }
          q.text(closing)
        end
      end
    end
  end

  class KeywordHashNode
    # Represents a hash literal without opening and closing braces.
    #
    #     foo(a: b)
    #         ^^^^
    def format(q)
      elements = self.elements

      if elements.any? { |element| element.value.is_a?(ImplicitNode) }
        format_identity(q)
      elsif elements.all? { |element| !element.is_a?(AssocNode) || element.operator_loc.nil? || element.key.is_a?(InterpolatedSymbolNode) || (element.key.is_a?(SymbolNode) && (value = element.key.value) && value.match?(/^[_A-Za-z]/) && !value.end_with?("=")) }
        format_labels(q)
      else
        format_rockets(q)
      end
    end

    private

    def format_layout(q)
      q.seplist(elements) do |element|
        if element.is_a?(AssocNode)
          yield element
        else
          q.format(element)
        end
      end
    end

    def format_identity(q)
      format_layout(q) { |element| q.format(element) }
    end

    def format_labels(q)
      format_layout(q) do |element|
        if element.operator_loc.nil?
          q.format(element)
        else
          key = element.key

          if key.is_a?(InterpolatedSymbolNode)
            opening = key.opening

            if opening.start_with?("%")
              q.group do
                q.text("\"")
                key.parts.each { |part| q.format(part) }
                q.text("\":")
              end
            else
              q.group do
                q.text(key.opening[1..])
                key.parts.each { |part| q.format(part) }
                q.text(key.closing)
                q.text(":")
              end
            end
          elsif key.is_a?(SymbolNode)
            q.text(key.value)
            q.text(":")
          else
            raise "Unexpected key: #{key.inspect}"
          end

          q.indent do
            q.breakable_space
            q.format(element.value)
          end
        end
      end
    end

    def format_rockets(q)
      format_layout(q) do |element|
        key = element.key

        if key.is_a?(InterpolatedSymbolNode)
          opening = key.opening

          if opening.start_with?("%")
            q.format(key)
          else
            q.group do
              q.text(opening)
              key.parts.each { |part| q.format(part) }
              q.text(key.closing)
            end
          end
        elsif key.is_a?(SymbolNode)
          q.text(":")
          q.text(key.value)
        else
          q.format(key)
        end

        q.text(" ")
        operator_loc = element.operator_loc
        operator_loc ? q.loc(operator_loc) : q.text("=>")

        q.indent do
          q.breakable_space
          q.format(element.value)
        end
      end
    end
  end

  class KeywordParameterNode
    # Represents a keyword parameter to a method, block, or lambda definition.
    #
    #     def a(b:)
    #           ^^
    #     end
    #
    #     def a(b: 1)
    #           ^^^^
    #     end
    def format(q)
      if value
        q.group do
          q.text("#{name}:")
          q.indent do
            q.breakable_space
            q.format(value)
          end
        end
      else
        q.text("#{name}:")
      end
    end
  end

  class IntegerNode
    # Represents an integer number literal.
    #
    #     1
    #     ^
    def format(q)
      slice = self.slice

      if slice.match?(/^[1-9]\d{4,}$/)
        # If it's a plain integer and it doesn't have any underscores separating
        # the values, then we're going to insert them every 3 characters
        # starting from the right.
        index = (slice.length + 2) % 3
        q.text("  #{slice}"[index..].scan(/.../).join("_").strip)
      else
        q.text(slice)
      end
    end

    test("1")
    test("1 # comment")
    test("1_2_3_4_5")
    test("12_345", "12345")
  end

  class KeywordRestParameterNode
    # Represents a keyword rest parameter to a method, block, or lambda
    # definition.
    #
    #     def a(**b)
    #           ^^^
    #     end
    def format(q)
      q.text("**")
      q.text(name.name) if name
    end
  end

  class LambdaNode
    # Represents using a lambda literal (not the lambda method call).
    #
    #     ->(value) { value * 2 }
    #     ^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("->")
        q.format(parameters) if parameters

        if body
          q.text(" ")
          q
            .if_break do
              q.text("do")
              q.indent do
                q.breakable_space
                q.format(body)
              end
              q.breakable_space
              q.text("end")
            end
            .if_flat do
              q.text("{ ")
              q.format(body)
              q.text(" }")
            end
        else
          q.text(" {}")
        end
      end
    end
  end

  class LocalVariableReadNode
    # Represents reading a local variable. Note that this requires that a local
    # variable of the same name has already been written to in the same scope,
    # otherwise it is parsed as a method call.
    #
    #     foo
    #     ^^^
    def format(q)
      q.text(name.name)
    end
  end

  class LocalVariableTargetNode
    # Represents writing to a local variable in a context that doesn't have an
    # explicit value.
    #
    #     foo, bar = baz
    #     ^^^  ^^^
    def format(q)
      q.text(name.name)
    end
  end

  class LocalVariableWriteNode
    # Represents writing to a local variable.
    #
    #     foo = 1
    #     ^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class LocalVariableAndWriteNode
    # Represents the use of the `&&=` operator for assignment to a local
    # variable.
    #
    #     target &&= value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class LocalVariableOrWriteNode
    # Represents the use of the `||=` operator for assignment to a local
    # variable.
    #
    #     target ||= value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class LocalVariableOperatorWriteNode
    # Represents assigning to a local variable using an operator that isn't `=`.
    #
    #     target += value
    #     ^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) { q.text(name.name) }
    end
  end

  class MatchLastLineNode
    # Represents a regular expression literal used in the predicate of a
    # conditional to implicitly match against the last line read by an IO
    # object.
    #
    #     if /foo/i then end
    #        ^^^^^^
    def format(q)
      q.text(opening)
      q.text(content)
      q.text(closing)
    end
  end

  class MatchPredicateNode
    # Represents the use of the modifier `in` operator.
    #
    #     foo in bar
    #     ^^^^^^^^^^
    def format(q)
      q.group do
        q.format(value)
        q.text(" in")

        case pattern
        when ArrayPatternNode, HashPatternNode, FindPatternNode
          q.text(" ")
          q.format(pattern)
        else
          q.indent do
            q.breakable_space
            q.format(pattern)
          end
        end
      end
    end
  end

  class MatchRequiredNode
    # Represents the use of the `=>` operator.
    #
    #     foo => bar
    #     ^^^^^^^^^^
    def format(q)
      q.group do
        q.format(value)
        q.text(" =>")

        case pattern
        when ArrayPatternNode, HashPatternNode, FindPatternNode
          q.text(" ")
          q.format(pattern)
        else
          q.indent do
            q.breakable_space
            q.format(pattern)
          end
        end
      end
    end
  end

  class MatchWriteNode
    # Represents writing local variables using a regular expression match with
    # named capture groups.
    #
    #     /(?<foo>bar)/ =~ baz
    #     ^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.format(call)
    end
  end

  class MissingNode
    # Represents a node that is missing from the source and results in a syntax
    # error.
    def format(q)
    end
  end

  class ModuleNode
    # Represents a module declaration involving the `module` keyword.
    #
    #    module Foo end
    #    ^^^^^^^^^^^^^^
    def format(q)
      module_keyword_loc = self.module_keyword_loc

      q.group do
        q.group do
          q.loc(module_keyword_loc)

          if module_keyword_loc.comments.any?
            q.indent do
              q.breakable_space
              q.format(constant_path)
            end
          else
            q.text(" ")
            q.format(constant_path)
          end
        end

        q.format_body(body, end_keyword_loc.comments)
        q.breakable_force
        q.text("end")
      end
    end
  end

  class MultiTargetNode
    # Represents a multi-target expression.
    #
    #     a, b, c = 1, 2, 3
    #     ^^^^^^^
    def format(q)
      q.group do
        if lparen_loc && rparen_loc
          q.text("(")
          q.indent do
            q.breakable_empty
            q.seplist(targets) { |target| q.format(target) }
          end
          q.breakable_empty
          q.text(")")
        else
          q.seplist(targets) { |target| q.format(target) }
        end
      end
    end
  end

  class MultiWriteNode
    # Represents a write to a multi-target expression.
    #
    #     a, b, c = 1, 2, 3
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.format_write(operator_loc, value) do
        q.group do
          if lparen_loc && rparen_loc
            q.loc(lparen_loc)
            q.indent do
              q.breakable_empty
              q.seplist(targets) { |target| q.format(target) }
            end
            q.breakable_empty
            q.text(")")
          else
            q.seplist(targets) { |target| q.format(target) }
          end
        end
      end
    end
  end

  class NextNode
    # Represents the use of the `next` keyword.
    #
    #     next foo
    #     ^^^^^^^^
    def format(q)
      q.format_jump("next", arguments)
    end
  end

  class NilNode
    # Represents the use of the literal `nil` keyword.
    #
    #     nil
    #     ^^^
    def format(q)
      q.text("nil")
    end
  end

  class NoKeywordsParameterNode
    # Represents the use of `**nil` inside method arguments.
    #
    #     def a(**nil)
    #           ^^^^^
    #     end
    def format(q)
      q.group do
        q.loc(operator_loc)
        q.nest(2) do
          q.breakable_empty
          q.text("nil")
        end
      end
    end
  end

  class NumberedReferenceReadNode
    # Represents reading a numbered reference to a capture in the previous
    # match.
    #
    #     $1
    #     ^^
    def format(q)
      q.text(slice)
    end
  end

  class OptionalParameterNode
    # Represents an optional parameter to a method, block, or lambda definition.
    #
    #     def a(b = 1)
    #           ^^^^^
    #     end
    def format(q)
      q.group do
        q.text("#{name} ")
        q.loc(operator_loc)
        q.indent do
          q.breakable_space
          q.format(value)
        end
      end
    end
  end

  class OrNode
    # Represents the use of the `||` operator or the `or` keyword.
    #
    #     left or right
    #     ^^^^^^^^^^^^^
    def format(q)
      q.format_binary(left, operator_loc, right)
    end
  end

  class ParametersNode
    # Represents the list of parameters on a method, block, or lambda
    # definition.
    #
    #     def a(b, c, d)
    #           ^^^^^^^
    #     end
    def format(q)
      q.seplist([*requireds, *optionals, *rest, *posts, *keywords, *keyword_rest, *block]) do |parameter|
        q.format(parameter)
      end
    end
  end

  class ParenthesesNode
    # Represents a parenthesized expression.
    #
    #     (10 + 34)
    #     ^^^^^^^^^
    def format(q)
      q.group do
        q.loc(opening_loc)
        q.format_body(body, closing_loc.comments, false)
        q.breakable_empty
        q.text(")")
      end
    end
  end

  class PinnedExpressionNode
    # Represents the use of the `^` operator for pinning an expression in a
    # pattern matching expression.
    #
    #     foo in ^(bar)
    #            ^^^^^^
    def format(q)
      q.group do
        q.text("^")
        q.loc(lparen_loc)
        q.format_body(expression, rparen_loc.comments, false)
        q.breakable_empty
        q.text(")")
      end
    end
  end

  class PinnedVariableNode
    # Represents the use of the `^` operator for pinning a variable in a pattern
    # matching expression.
    #
    #     foo in ^bar
    #            ^^^^
    def format(q)
      q.format_prefix(operator_loc, variable)
    end
  end

  class PostExecutionNode
    # Represents the use of the `END` keyword.
    #
    #     END { foo }
    #     ^^^^^^^^^^^
    def format(q)
      statements = self.statements
      closing_comments = closing_loc.comments

      q.group do
        q.text("END ")
        q.loc(opening_loc)

        if statements || closing_comments.any?
          q.indent do
            if statements
              q.breakable_space
              q.format(statements)
            end

            closing_comments.each do |comment|
              breakable_empty
              text(comment.slice)
            end
          end

          q.breakable_space
        end

        q.text("}")
      end
    end
  end

  class PreExecutionNode
    # Represents the use of the `BEGIN` keyword.
    #
    # BEGIN { foo }
    # ^^^^^^^^^^^^^
    def format(q)
      statements = self.statements
      closing_comments = closing_loc.comments

      q.group do
        q.text("BEGIN ")
        q.loc(opening_loc)

        if statements || closing_comments.any?
          q.indent do
            if statements
              q.breakable_space
              q.format(statements)
            end

            closing_comments.each do |comment|
              breakable_empty
              text(comment.slice)
            end
          end

          q.breakable_space
        end

        q.text("}")
      end
    end
  end

  class ProgramNode
    # The top level node of any parse tree.
    def format(q)
      q.format(statements)
      q.breakable_force if location.comments.empty?
    end
  end

  class RangeNode
    # Represents the use of the `..` or `...` operators.
    #
    #     1..2
    #     ^^^^
    def format(q)
      q.group do
        q.format(left) if left
        q.text(operator)
        q.format(right) if right
      end
    end
  end

  class RationalNode
    # Represents a rational number literal.
    #
    #     1r
    #     ^^
    def format(q)
      q.text(slice)
    end
  end

  class RedoNode
    # Represents the use of the `redo` keyword.
    #
    #     redo
    #     ^^^^
    def format(q)
      q.text("redo")
    end
  end

  class RegularExpressionNode
    # Represents a regular expression literal with no interpolation.
    #
    #     /foo/i
    #     ^^^^^^
    def format(q)
      q.text(opening)
      q.text(content)
      q.text(closing)
    end

    test("/foo/i")
    test("%r{foo}i")
    test("/foo/i # comment")
    test("%r{foo}i # comment")
  end

  class RequiredDestructuredParameterNode
    # Represents a destructured required parameter node.
    #
    #     def foo((bar, baz))
    #             ^^^^^^^^^^
    #     end
    def format(q)
      q.group do
        q.loc(opening_loc)
        q.indent do
          q.breakable_empty
          q.seplist(parameters) { |parameter| q.format(parameter) }
          closing_loc.comments.each do |comment|
            q.breakable_empty
            q.text(comment.slice)
          end
        end
        q.breakable_empty
        q.text(")")
      end
    end
  end

  class RequiredParameterNode
    # Represents a required parameter to a method, block, or lambda definition.
    #
    #     def a(b)
    #           ^
    #     end
    def format(q)
      q.text(name.name)
    end
  end

  class RescueModifierNode
    # Represents an expression modified with a rescue.
    #
    #     foo rescue nil
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("begin")
        q.indent do
          q.breakable_force
          q.format(expression)
        end
        q.breakable_force
        q.loc(keyword_loc)
        q.text(" StandardError")
        q.indent do
          q.breakable_force
          q.format(rescue_expression)
        end
        q.breakable_force
        q.text("end")
      end
    end
  end

  class RescueNode
    # Represents a rescue statement.
    #
    #     begin
    #     rescue Foo, *splat, Bar => ex
    #       foo
    #     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #     end
    def format(q)
      q.group do
        q.group do
          q.loc(keyword_loc)

          if exceptions.any?
            q.text(" ")
            q.nest(7) { q.seplist(exceptions) { |exception| q.format(exception) } }
          elsif reference.nil?
            q.text(" StandardError")
          end

          if reference
            q.text(" ")
            q.loc(operator_loc)

            if operator_loc.comments.any?
              q.indent do
                q.breakable_space
                q.format(reference)
              end
            else
              q.text(" ")
              q.format(reference)
            end
          end
        end

        if statements
          q.indent do
            q.breakable_force
            q.format(statements)
          end
        end

        if consequent
          q.breakable_force
          q.format(consequent)
        end
      end
    end
  end

  class RestParameterNode
    # Represents a rest parameter to a method, block, or lambda definition.
    #
    #     def a(*b)
    #           ^^
    #     end
    def format(q)
      name = self.name

      if name
        q.group do
          q.loc(operator_loc)
          q.nest(1) do
            q.breakable_empty
            q.text(name.name)
          end
        end
      else
        q.loc(operator_loc)
      end
    end
  end

  class RetryNode
    # Represents the use of the `retry` keyword.
    #
    #     retry
    #     ^^^^^
    def format(q)
      q.text("retry")
    end
  end

  class ReturnNode
    # Represents the use of the `return` keyword.
    #
    #     return foo
    #     ^^^^^^^^^^
    def format(q)
      q.format_jump("return", arguments)
    end
  end

  class SelfNode
    # Represents the use of the literal `self` keyword.
    #
    #     self
    #     ^^^^
    def format(q)
      q.text("self")
    end
  end

  class SingletonClassNode
    # Represents a singleton class declaration involving the `class` keyword.
    #
    #     class << self end
    #     ^^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.group do
          q.text("class ")
          q.loc(operator_loc)

          if operator_loc.comments.any?
            q.indent do
              q.breakable_space
              q.format(expression)
            end
          else
            q.text(" ")
            q.format(expression)
          end
        end

        q.format_body(body, end_keyword_loc.comments)
        q.breakable_force
        q.text("end")
      end
    end
  end

  class SourceEncodingNode
    # Represents the use of the `__ENCODING__` keyword.
    #
    #     __ENCODING__
    #     ^^^^^^^^^^^^
    def format(q)
      q.text("__ENCODING__")
    end
  end

  class SourceFileNode
    # Represents the use of the `__FILE__` keyword.
    #
    #     __FILE__
    #     ^^^^^^^^
    def format(q)
      q.text("__FILE__")
    end
  end

  class SourceLineNode
    # Represents the use of the `__LINE__` keyword.
    #
    #     __LINE__
    #     ^^^^^^^^
    def format(q)
      q.text("__LINE__")
    end
  end

  class SplatNode
    # Represents the use of the splat operator.
    #
    #     [*a]
    #      ^^
    def format(q)
      expression = self.expression
      operator_loc = self.operator_loc

      if expression
        q.group do
          q.text("*")
          operator_loc.comments.each { |comment| comment.format(self) }

          q.nest(1) do
            q.breakable_empty
            q.format(expression)
          end
        end
      else
        q.text("*")
        operator_loc.comments.each { |comment| comment.format(self) }
      end
    end
  end

  class StatementsNode
    # Represents a set of statements contained within some scope.
    #
    #     foo; bar; baz
    #     ^^^^^^^^^^^^^
    def format(q)
      line = nil
      previous_access_control = false

      body.each_with_index do |statement, index|
        if line.nil?
          q.format(statement)
        elsif ((statement.location.start_line - line) > 1) || access_control?(statement) || previous_access_control
          q.breakable_force
          q.breakable_force
          q.format(statement)
        elsif (statement.location.start_line != line) || !q.parent.is_a?(EmbeddedStatementsNode)
          q.breakable_force
          q.format(statement)
        else
          q.text("; ")
          q.format(statement)
        end

        line = statement.location.end_line
        previous_access_control = access_control?(statement)
      end
    end

    private

    def access_control?(statement)
      statement.is_a?(CallNode) && statement.variable_call? && %w[private protected public].include?(statement.message)
    end
  end

  class StringConcatNode
    # Represents the use of compile-time string concatenation.
    #
    # "foo" "bar"
    # ^^^^^^^^^^^
    def format(q)
      q.group do
        q.format(left)
        q
          .if_break do
            q.text(" \\")
            q.indent do
              q.breakable_empty
              q.format(right)
            end
          end
          .if_flat do
            q.text(" ")
            q.format(right)
          end
      end
    end
  end

  class StringNode
    # Represents a string literal, a string contained within a `%w` list, or
    # plain string content within an interpolated string.
    #
    #     "foo"
    #     ^^^^^
    #
    #     %w[foo]
    #        ^^^
    #
    #     "foo #{bar} baz"
    #      ^^^^      ^^^^
    def format(q)
      opening = self.opening

      if opening&.start_with?("<<")
        q.format_heredoc(self, [self])
      else
        content = self.content

        if opening == "?"
          if content.length == 1
            q.text("\"#{content == "\"" ? "\\\"" : content}\"")
          else
            q.text("?#{content}")
          end
        else
          q.text(opening) if opening
          q.text(content)
          q.text(closing) if closing_loc
        end
      end
    end
  end

  class SuperNode
    # Represents the use of the `super` keyword with parentheses or arguments.
    #
    #     super()
    #     ^^^^^^^
    #
    #     super foo, bar
    #     ^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("super")

        if lparen_loc && rparen_loc
          q.text("(")
          if arguments
            q.indent do
              q.breakable_empty
              q.format(arguments)
            end
            q.breakable_empty
          end
          q.text(")")
        elsif arguments
          q.text(" ")
          q.nest(6) { q.format(arguments) }
        end

        if block
          q.text(" ")
          q.format(block)
        end
      end
    end
  end

  class SymbolNode
    # Represents a symbol literal or a symbol contained within a `%i` list.
    #
    #     :foo
    #     ^^^^
    def format(q)
      q.text(slice) 
    end
  end

  class TrueNode
    # Represents the use of the literal `true` keyword.
    #
    #     true
    #     ^^^^
    def format(q)
      q.text("true")
    end
  end

  class UndefNode
    # Represents the use of the `undef` keyword.
    #
    #     undef :foo, :bar, :baz
    #     ^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      q.group do
        q.text("undef ")
        q.nest(6) do
          q.seplist(names) do |name|
            if name.is_a?(SymbolNode)
              q.text("#{name.value}")

              if (comment = name.location.comments.first)
                comment.format(q)
              end
            else
              q.format(name)
            end
          end
        end
      end
    end
  end

  class UnlessNode
    # Represents the use of the `unless` keyword, either in the block form or
    # the modifier form.
    #
    #     bar unless foo
    #     ^^^^^^^^^^
    #
    #     unless foo then bar end
    #     ^^^^^^^^^^^^^^^^^^^^^^^
    def format(q)
      consequent = self.consequent

      if !statements
        q.group do
          q.text("unless ")
          q.nest(3) { q.format(predicate) }

          if consequent
            q.breakable_force
            q.format(consequent)
          end

          q.breakable_force
          q.text("end")
        end
      elsif consequent
        q.group do
          q.group do
            q.text("unless ")
            q.nest(3) { q.format(predicate) }
          end

          q.breakable_force
          q.format(statements)

          q.breakable_force
          q.format(consequent)

          q.breakable_force
          q.text("end")
        end
      else
        q.group do
          q
            .if_break do
              q.group do
                q.text("unless ")
                q.nest(3) { q.format(predicate) }
              end

              q.breakable_space
              q.format(statements)

              if consequent
                q.breakable_space
                q.format(consequent)
              end

              q.breakable_space
              q.text("end")
            end
            .if_flat do
              q.format(statements)
              q.text(" unless ")
              q.format(predicate)
            end
        end
      end
    end
  end

  class UntilNode
    # Represents the use of the `until` keyword, either in the block form or the
    # modifier form.
    #
    #     bar until foo
    #     ^^^^^^^^^^^^^
    #
    #     until foo do bar end
    #     ^^^^^^^^^^^^^^^^^^^^
    def format(q)
      if begin_modifier?
        q.group do
          q.format(statements)
          q.text(" until ")
          q.format(predicate)
        end
      elsif !statements
        q.group do
          q.text("until ")
          q.nest(6) { q.format(predicate) }
          q.breakable_force
          q.text("end")
        end
      else
        q.group do
          q
            .if_break do
              q.text("until ")
              q.nest(6) { q.format(predicate) }
              q.indent do
                q.breakable_space
                q.format(statements)
              end
              q.breakable_space
              q.text("end")
            end
            .if_flat do
              q.format(statements)
              q.text(" until ")
              q.format(predicate)
            end
        end
      end
    end
  end

  class WhenNode
    # Represents the use of the `when` keyword within a case statement.
    #
    #     case true
    #     when true
    #     ^^^^^^^^^
    #     end
    def format(q)
      q.group do
        q.group do
          q.text("when ")
          q.nest(5) do
            q.seplist(conditions, ->(q) { q.group { q.comma_breakable } }) do |condition|
              q.format(condition)
            end
          end
        end

        if statements
          q.indent do
            q.breakable_force
            q.format(statements)
          end
        end
      end
    end
  end

  class WhileNode
    # Represents the use of the `while` keyword, either in the block form or the
    # modifier form.
    #
    #     bar while foo
    #     ^^^^^^^^^^^^^
    #
    #     while foo do bar end
    #     ^^^^^^^^^^^^^^^^^^^^
    def format(q)
      if begin_modifier?
        q.group do
          q.format(statements)
          q.text(" while ")
          q.format(predicate)
        end
      elsif !statements
        q.group do
          q.text("while ")
          q.nest(6) { q.format(predicate) }
          q.breakable_force
          q.text("end")
        end
      else
        q.group do
          q
            .if_break do
              q.text("while ")
              q.nest(6) { q.format(predicate) }
              q.indent do
                q.breakable_space
                q.format(statements)
              end
              q.breakable_space
              q.text("end")
            end
            .if_flat do
              q.format(statements)
              q.text(" while ")
              q.format(predicate)
            end
        end
      end
    end
  end

  class XStringNode
    # Represents an xstring literal with no interpolation.
    #
    #     `foo`
    #     ^^^^^
    def format(q)
      if opening.start_with?("<<")
        q.format_heredoc(self, [self])
      else
        q.text("`#{content}`")
      end
    end
  end

  class YieldNode
    # Represents the use of the `yield` keyword.
    #
    #     yield foo
    #     ^^^^^^^^^
    def format(q)
      q.format_jump("yield", arguments)
    end
  end
end
