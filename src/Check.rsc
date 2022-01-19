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

// Checks:
// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
// - produce an error if the condition is not boolean.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (q) {
    case normal(str label, AId id, AType questionType, src = loc u):{
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Same label for different questions", u) };
      	
      if( <src, _, label1, _> <- tenv && <src, _, label2, _> <- tenv && label1 != label2)
      	msgs += { warning("Different label for occurrencies of the same question", u) }; 	
     }
    
    case computed(str label, AId id, AType questionType, AExpr expr, src = loc u): {
      if( <src1, _, label, _> <- tenv && <src2, _, label, _> <- tenv && src1 != src2)
      	msgs += { warning("Same label for different questions", u) };
      	
      if( <src, _, label1, _> <- tenv && <src, _, label2, _> <- tenv && label1 != label2)
      	msgs += { warning("Different label for occurrencies of the same question", u) };
      
      msgs += check(expr, tenv, useDef);
      msgs += { error("Declared type does not match expression type", u) | 
                typeOf(expr, tenv, useDef) != toType(questionType) };
    }
    
    case block(list[AQuestion] questions, src = loc u):
      for (AQuestion q <- questions) msgs += check(q, tenv, useDef);
    
    case if_then_else(AExpr cond, list[AQuestion] ifQs, list[AQuestion] elseQs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifQs)   msgs += check(q, tenv, useDef);
      for (AQuestion q <- elseQs) msgs += check(q, tenv, useDef); 
    }
    
    case if_then(AExpr cond, list[AQuestion] ifQs, src = loc u): {
      msgs += { error("Condition is not boolean", u) | typeOf(cond, tenv, useDef) != tbool() };
      msgs += check(cond, tenv, useDef);
      for (AQuestion q <- ifQs) msgs += check(q, tenv, useDef);
    }
  }
  
  return msgs;
}


// Checks:
// - Reference to undefined questions
// - Operands of invalid type to operators
set[Message] check(ref(AId x), TEnv tenv, UseDef useDef)
  = { error("Reference to undefined question", x.src) | useDef[x.src] == {} };
default set[Message] check(AExpr e, TEnv tenv, UseDef useDef)
  = { error("Operands of invalid type to operators", e.src) | typeOf(e, tenv, useDef) == tunknown() };


// Check operand compatibility with operators.
Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
  when <u, loc d> <- useDef, <d, _, _, Type t> <- tenv;
  
Type typeOf(boolean(bool _), TEnv _, UseDef _) = tbool();
Type typeOf(integer(int _), TEnv _, UseDef _) = tint();
Type typeOf(string(str _), TEnv _, UseDef _) = tstr();

Type typeOf(not(AExpr expr), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(expr, tenv, useDef) == tbool();
  
Type typeOf(mul(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tint()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(div(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tint()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(add(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tint()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(sub(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tint()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
  
Type typeOf(less(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(leq(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(greater(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(geq(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint();
Type typeOf(eq(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = typeOf(lhs, tenv, useDef)
  when typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef);
Type typeOf(neq(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = typeOf(lhs, tenv, useDef)
  when typeOf(lhs, tenv, useDef) == typeOf(rhs, tenv, useDef);
Type typeOf(and(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool();
Type typeOf(or(AExpr lhs, AExpr rhs), TEnv tenv, UseDef useDef) = tbool()
  when typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool();
  
default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 

