module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  
  switch (f) {
    case (Form) `form <Id x> { <Question* qs> }`:
      return form("<x>", [ cst2ast(q) | Question q <- qs ], src=f@\loc); 
    
    default: throw "Invalid form: <f>";
  } 
}

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question) `<Str label> <Id x> : <Type t>`:
      return normal("<label>", cst2ast(x), cst2ast(t), src=q@\loc);
    
    case (Question) `<Str label> <Id x> : <Type t> = <Expr e>`:
      return computed("<label>", cst2ast(x), cst2ast(t), cst2ast(e), src=q@\loc);
      
    case (Question) `{ <Question* qs> }`:
      return block([ cst2ast(q) | Question q <- qs ], src=q@\loc);
      
    case (Question) `if ( <Expr cond> ) { <Question* qs> }`:
      return if_then(cst2ast(cond), [ cst2ast(q) | Question q <- qs ], src=q@\loc);
      
    case (Question) `if ( <Expr cond> ) { <Question* ifqs> } else { <Question* elseqs> }`:
      return if_then_else(cst2ast(cond), [ cst2ast(q) | Question q <- ifqs ], 
        [ cst2ast(q) | Question q <- elseqs ],  src=q@\loc);
      
    case (Question) `// _`:
      return empty("");
      
    default: throw "Invalid question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=x@\loc);
    
    // etc.
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  throw "Not yet implemented";
}

AId cst2ast(Id x) {
  switch (x) {
  	case (Id)`<Str name>`: 
  	  return id("<name>", src=x@\loc);
    
    default: throw "Invalid id: <x>";
  }
}
