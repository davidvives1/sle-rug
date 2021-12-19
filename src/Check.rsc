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

TEnv collect(AForm f)
  = { <q.id.src, q.id.name, q.label, toType(q.\questionType)> | /AQuestion q := f, q has id };
  
  
  
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};

	for (/AQuestion q := f)
		msgs += check(q, tenv, useDef);
	for (/AExpr e := f)
		msgs += check(e, tenv, useDef);

	return msgs;
}



// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
	set[Message] msg = {};
	
	str n = q.id.name;
	if((<_, n, _, Type t> <- tenv) && t != toType(q.questionType)){
		msg += {error("Previously declared question with same name, but different type", q.src)};
		
	}
	
	if((<_, _, str label, _> <- tenv) && label == q.label){
		msg += {warning("The label of this question is identical to that of another question", q.src)};
		
	}
  return msg; 
}

// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, AExpr expr, TEnv tenv, UseDef useDef) {
	set[Message] msg = {};
		
	t = typeOf(expr, tenv, useDef);
	
	if(t != toType(q.questionType)){
		msg += {error("the declared type computed questions should match the type of the expression.", q.src)};
	}
	return msg; 
}





Type toType(\boolean()) = tbool();
Type toType(\integer()) = tint();
Type toType(\string()) = tstr();
default Type toType(AType _) = tunknown();









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
 
 

