module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

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
      
    case (Question) `if ( <Expr cond> ) { <Question* ifQs> }`:
      return if_then(cst2ast(cond), [ cst2ast(q) | Question q <- ifQs ], src=q@\loc);
      
    case (Question) `if ( <Expr cond> ) { <Question* ifQs> } else { <Question* elseQs> }`:
      return if_then_else(cst2ast(cond), [ cst2ast(q) | Question q <- ifQs ], 
        [ cst2ast(q) | Question q <- elseQs ],  src=q@\loc);
      
    default: throw "Invalid question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {   
    case (Expr) `<Id x>`: 
      return ref(cst2ast(x), src=x@\loc);
      
    case (Expr) `<Bool b>`:
      return boolean(fromString("<b>"), src=b@\loc);
      
    case (Expr) `<Int i>`:
      return integer(toInt("<i>"), src=i@\loc);
      
    case (Expr) `<Str s>`:
      return string("<s>", src=s@\loc);
      
    case (Expr) `( <Expr l> )`:
      return cst2ast(l);
      
    case (Expr) `! <Expr l>`:
      return not(cst2ast(l), src=l@\loc);
      
    case (Expr) `<Expr l> * <Expr r>`:
      return mul(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> / <Expr r>`:
      return div(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> + <Expr r>`:
      return add(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> - <Expr r>`:
      return sub(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \> <Expr r>`:
      return greater(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \< <Expr r>`:
      return less(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \<= <Expr r>`:
      return leq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> \>= <Expr r>`:
      return geq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> == <Expr r>`:
      return eq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> != <Expr r>`:
      return neq(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> && <Expr r>`:
      return and(cst2ast(l), cst2ast(r), src=e@\loc);
      
    case (Expr) `<Expr l> || <Expr r>`:
      return or(cst2ast(l), cst2ast(r), src=e@\loc);
        
    default: throw "Invalid expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
  	case (Type) `boolean`:
  	  return boolean();
  	  
  	case (Type) `integer`:
  	  return integer();
  	  
  	case (Type) `string`:
  	  return string();
  	  
  	default: throw "Unknown type: <t>";
  }
}

AId cst2ast(Id x) {
  switch (x) {
  	case (Id)`<Str name>`: 
  	  return id("<name>", src=x@\loc);
    
    default: throw "Invalid id: <x>";
  }
}
