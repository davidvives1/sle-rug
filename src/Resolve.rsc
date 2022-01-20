module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);
  
 UseDef resolve(AForm f) = uses(f) o defs(f);

Use uses(AForm f) { 
  return { <e.src, e.id.name>  | /AExpr e <- f.questions, e has id}; //Will only get expressions that have a name.
}

Def defs(AForm f) {
  return { <q.id.name, q.src> | /AQuestion q <- f.questions, q has id}; //Will only get questions that have an id.
}