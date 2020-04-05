type ('a, 'b) either = Left of 'a | Right of 'b
type non_terminal = NonTerminal of string
type terminal = Terminal of string 
type lhs = non_terminal
type rhs = (non_terminal, terminal) either list
type assignment = 
    Assignment of lhs * rhs
  | Nothing