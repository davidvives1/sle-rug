module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);

// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv venv = ();
  
  visit(f) {
    case normal(str label, AId id, AType questionType, src = loc def):
      switch (questionType) {
        case integer(): venv += (id : vint(0)); 
        case boolean(): venv += (id : vbool(false));    
        case string(): venv += (id : vstr(""));
      }
  
    case computed(str label, AId id, AType questionType, AExpr expr, src = loc def):
      switch (questionType) {
        case integer(): venv += (id : vint(0)); 
        case boolean(): venv += (id : vbool(false));
        case string(): venv += (id : vstr(""));
      }
  }
  return venv;
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv)
    venv = evalOnce(f, inp, venv);
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for (q <- f.questions)
    venv = eval(q, inp, venv);
    
  return venv;
}
  

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  if (computed(str _, AId id, AType _, AExpr expr) := q) {
    venv += (id.name: eval(expr, venv));
  } else if (normal(str _, AId id, AType _) := q) {
    if (inp.question == id.name) {
      venv += (id.name: inp.\value);
    }
  } else if (block(list[AQuestion] questions) := q) {
    for (AQuestion x <- questions) {
      venv = eval(x, inp, venv);
    }
  } else if (if_then_else(AExpr condition, list[AQuestion] ifQs, list[AQuestion] elseQs) := q) {
    if (eval(condition, venv).b) {
      for (AQuestion x <- ifQs) {
        venv = eval(x, inp, venv);
      }
    } else {
      for (AQuestion x <- elseQs) {
        venv = eval(x, inp, venv);
      }
    }
  } else if (if_then(AExpr condition, list[AQuestion] ifQs) := q) {
    if (eval(condition, venv).b) {
      for (AQuestion x <- ifQs) {
        venv = eval(x, inp, venv);
      } 
    }
  }  
  return venv;
}


Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(AId id): 
    	return venv[id.name];
    
    case integer(int i):
      return vint(i);
      
    case boolean(bool b):
      return vbool(b);
    
    case string(str s):
      return vstr(s);
    
    case mul(AExpr l, AExpr r):
      return vint(eval(l, venv).n * eval(r, venv).n);
    
    case div(AExpr l, AExpr r): 
      return vint(eval(l, venv).n / eval(r, venv).n);
    
    case add(AExpr l, AExpr r): 
      return vint(eval(l, venv).n + eval(r, venv).n);
    
    case sub(AExpr l, AExpr r): 
      return vint(eval(l, venv).n - eval(r, venv).n);
    
    case greater(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b > eval(r, venv).b);
    
    case less(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b < eval(r, venv).b);
    
    case geq(AExpr l, AExpr r):
      return vbool(eval(l, venv).b >= eval(r, venv).b);
    
    case leq(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b <= eval(r, venv).b);
    
    case not(AExpr l):
      return vbool(!eval(l, venv).b);
    
    case eq(AExpr l, AExpr r):
      return vbool(eval(l, venv).b == eval(r, venv).b); 
    
    case neq(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b != eval(r, venv).b);
    
    case and(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b && eval(r, venv).b);
    
    case or(AExpr l, AExpr r): 
      return vbool(eval(l, venv).b || eval(r, venv).b);
    
    default: throw "Unsupported expression <e>";
  }
}