import IO;
import ParseTree;
import Syntax;
syn = parse(#start[Form], |project://QL/examples/binary.myql|);

import AST;
import CST2AST;
ast = cst2ast(syn);

import Resolve;
res = resolve(ast);

import Check;
col = collect(ast);
che = check(ast, col, res.useDef);

import Eval;
ienv = initialEnv(ast);