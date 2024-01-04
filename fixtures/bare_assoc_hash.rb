%
foo(bar: bar)
%
foo(:bar => bar)
-
foo(bar: bar)
%
foo(:"bar" => bar)
-
foo(bar: bar)
%
foo(bar => bar, baz: baz)
-
foo(bar => bar, :baz => baz)
%
foo(bar => bar, "baz #{1}": baz)
-
foo(bar => bar, :"baz #{1}" => baz)
%
foo(bar: barrrrrrrrrrrrrrrrrrrrrrrrrrrrrr, baz: bazzzzzzzzzzzzzzzzzzzzzzzzzzzzzz)
-
foo(
  bar: barrrrrrrrrrrrrrrrrrrrrrrrrrrrrr,
  baz: bazzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
)
