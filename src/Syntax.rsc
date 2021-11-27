module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id ":" Type //Normal question
  | Str Id ":" Type "=" Expr //Computed question
  | "{" Question* "}" //Block of questions
  | "if" "(" Expr ")" "{" Question* "}" //If-then question
  | "if" "(" Expr ")" "{" Question* "}" "else" "{" Question* "}" //If-then-else question
  ; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Bool
  | Int
  | Str
  | "(" Expr ")" //Highest priority to brackets expressions.
  > "!" Expr //Always negate first.
  > left ( Expr "*" Expr | Expr "/" Expr ) //Multiplication and division have highest priority.
  > left ( Expr "+" Expr | Expr "-" Expr ) //Sums and substractions after
  > non-assoc ( Expr "\>" Expr | Expr "\<" Expr | Expr "\>=" Expr | Expr "\<=" Expr )
  > left ( Expr "==" Expr | Expr "!=" Expr ) 
  > left ( Expr "&&" Expr | Expr "||" Expr )
  ;
  
syntax Type
  = "string"
  | "integer"
  | "boolean"
  ;  
  
lexical Str = "\"" ![\"]* "\""; //character class that defines anything except quotes.

lexical Int 
  = [\-]?[1-9][0-9]* //possibly a negative number or any other number (non zero)
  | [0] //zero
  ;

lexical Bool 
  = "true"
  | "false"
  ;



