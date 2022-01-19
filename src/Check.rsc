module Check

import AST;
import Resolve;
import Message; // see standard library

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
//deep match

TEnv collect(AForm f) {
	TEnv tenv = {};	
	for(/AQuestion q <- f.questions) {
		tenv += {<q.src, q.id, q.label, toType(q.questionType)> | q has name};
	}
	return tenv;
}
  
Type toType(\boolean()) = tbool();
Type toType(\integer()) = tint();
Type toType(\string()) = tstr();
default Type toType(AType _) = tunknown();

  
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};
  
  	if ( (<loc1, x, _, tint()> <- tenv && <loc2, x, _, tbool()> <- tenv )
        || (<loc1, x, _, tint()> <- tenv && <loc2, x, _, tstr()> <- tenv )
        || (<loc1, x, _, tbool()> <- tenv && <loc2, x, _, tstr()> <- tenv )) {
        msgs += error("Duplicate question with different types", loc1);
        msgs += error("Duplicate question with different types", loc2);
  	}
  
 	for (AQuestion q <- f.questions) {
   	 msgs += check(q, tenv, useDef);
 	}
  	
 	 return msgs;
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (q) {
    case normal(str label, AId id, AType questionType, src = loc u):{
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Same label for different questions", u) };
      	
      if( <_, id, label1, _> <- tenv && <_, id, label2, _> <- tenv && label1 != label2)
      	msgs += { warning("Different label for occurrencies of the same question", u) }; 	
     }
    
    case computed(str label, AId id, AType questionType, AExpr expr, src = loc u): {
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Same label for different questions", u) };
      	
      if( <_, id, label1, _> <- tenv && <_, id, label2, _> <- tenv && label1 != label2)
      	msgs += { warning("Different label for occurrencies of the same question", u) };
      
      msgs += check(expr, tenv, useDef);
      msgs += { error("Declared type does not match expression type", u) | 
                typeOf(expr, tenv, useDef) != toType(questionType) };
    }
    
    case block(list[AQuestion] questions, src = loc u):
      for (AQuestion q <- questions) msgs += check(q, tenv, useDef);
    
    case if_then_else(AExpr cond, AQuestion ifqs, AQuestion elseqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifqs)   msgs += check(q, tenv, useDef);
      for (AQuestion q <- elseqs) msgs += check(q, tenv, useDef);
    }
    
    case if_then(AExpr cond, AQuestion ifqs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifqs) msgs += check(q, tenv, useDef);
    }
  }
  
  return msgs;
}













// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(AId x):
      msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };

  case not(AExpr child, src = loc u):
      msgs += { error("Mismatched type used, not operator (!) expects a boolean.", u) | typeOf(child, tenv, useDef) != tbool() };
    case mul(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, multiplication operator (*) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };
    case div(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, division operator (\\) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };
    case add(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, addition operator (+) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };
    case sub(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, subtraction operator (-) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };
    case gt(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, greater than operator (\>) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };    
    case lt(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, less than operator (\<) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };    
    case leq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, less or equal operator (\<=) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };    
    case geq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, greater or equal operator (\>=) expects integers.", u) | operatorHasInts(lhs, rhs, tenv, useDef) };    
    case eq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Mismatched types used, equals operator (==) expects operands of the same type.", u) | operatorHasSameType(lhs, rhs, tenv, useDef) };
    case neq(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Mismatched types used, not equals operator (!=) expects operands of the same type.", u) | operatorHasSameType(lhs, rhs, tenv, useDef) };
    case and(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, and operator (&&) expects booleans.", u) | operatorHasBooleans(lhs, rhs, tenv, useDef) };
    case or(AExpr lhs, AExpr rhs, src = loc u):
      msgs += { error("Wrong types used, or operator (||) expects booleans.", u) | operatorHasBooleans(lhs, rhs, tenv, useDef) };    
  }
  return msgs; 
}
bool operatorHasInts(lhs, rhs, tenv, useDef) { 
  return !(typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) && typeOf(rhs, tenv, useDef) == tint());
} 
bool operatorHasBooleans(lhs, rhs, tenv, useDef) { 
  return !(typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef) && typeOf(rhs, tenv, useDef) == tbool());
}
bool operatorHasSameType(lhs, rhs, tenv, useDef) {
  return typeOf(lhs, tenv, useDef) != typeOf(rhs, tenv, useDef);
}







Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
  case integer(_): return tint();
  case boolean(_): return tbool();
  case string(_): return tstr();

  case not(_): return tbool();
               
  case mul(_,_): return tint();
  case div(_,_): return tint();
               
  case add(_,_): return tint();
  case sub(_,_): return tint();

  case greater(_,_): return tbool();
  case less(_,_): return tbool();
  case leq(_,_): return tbool();
  case geq(_,_): return tbool();

  case eq(_,_): return tbool();
  case neq(_,_): return tbool();
           
  case and(_,_): return tbool();

  case or(_,_): return tbool();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

