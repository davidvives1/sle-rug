module Transform

import Syntax;
import Resolve;
import AST;

import ParseTree;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
   f.questions=flatten(f.questions,boolean(true));
   return f;
}

list[AQuestion] flatten(list[AQuestion] aQuestions, AExpr expr){
	return ([] | it + flatten(aQuestion, expr) | /AQuestion aQuestion := aQuestions);
}

list[AQuestion] flatten(AQuestion q, AExpr expr) {
	switch (q) {
		case normal(str _, AId _, AType _):
			return  [if_then(expr, [q])];
		case computed(str _, AId _, AType _, AExpr _):
			return  [if_then(expr, [q])];
		case block(list[AQuestion] questions):
			return flatten(questions, expr);
		case if_then(AExpr e, list[AQuestion] if_qs):
			return flatten(if_qs, and(e, expr));
		case if_then_else(AExpr condition, list[AQuestion] trueQuestions, list[AQuestion] falseQuestions):
			return   flatten(trueQuestions, and(condition, expr)) + flatten(falseQuestions, and(not(condition), expr));
	}
	return [];
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
   Id newId = [Id] newName;
	set[loc] instances = {useOrDef};
	
	if (<useOrDef, def> <- useDef) {
		instances += {def} + {use | <use, def> <- useDef};
	}
	
	if (<_, useOrDef> <- useDef) {
		instances += {use | <use, useOrDef> <- useDef};
	}
	
	return visit(f) {
		case (Question)`<Str l> <Id i> : <Type t>` => 
			(Question)`<Str l> <Id newId> : <Type t>` when i@\loc in instances
		case (Question)`<Str l> <Id i> : <Type t> = <Expr e>` => 
			(Question)`<Str l> <Id newId> : <Type t> = <Expr e>` when i@\loc in instances
		case (Expr)`<Id i>` =>
			(Expr)`<Id newId>` when i@\loc in instances
	}; 
 }
 

