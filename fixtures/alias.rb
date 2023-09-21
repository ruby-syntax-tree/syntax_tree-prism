%
alias foo bar
%
alias << push
%
alias in within
%
alias in IN
%
alias :foo :bar
-
alias foo bar
%
alias :"foo #{1}" :bar
-
alias :"foo #{1}" bar
%
alias :foo :"bar #{1}"
-
alias foo :"bar #{1}"
%
alias foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo bar
-
alias foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      bar
%
alias foo bar # comment
%
alias foo # comment
      bar
%
alias foo # comment1
      bar # comment2
