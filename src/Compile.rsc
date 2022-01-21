module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
    head(
      title("QL")
    ),
    body(
      div(
        id("app"), 
        h1(f.name),
        questionList2html(f.questions)
      ),
      script(src("https://cdn.jsdelivr.net/npm/vue")),
      script(src(f.src[extension="js"].file))
    )
  );
}

HTML5Node questionList2html(list[AQuestion] questionList) {
  return div(
    [question2html(question) | question <- questionList]
  );
}

HTML5Node question2html(AQuestion question) {
  switch (question) {
    case normal(str label, AId id, AType questionType): 
      return div(
        p(label),
        input(html5attr("v-model", id), \type(type2htmlStr(questionType)))
      );
    case computed(str label, AId id, AType questionType, AExpr expr): 
      return div(
        p(label),
        input(html5attr("v-model", id), \type(type2htmlStr(questionType)), readonly([]))
      );
    case block(list[AQuestion] block): 
      return questionList2html(block);
    case if_then(AExpr expression, list[AQuestion] ifQs):
      return template(
        html5attr("v-if", "if_<expression.src.begin.line>_<expression.src.begin.column>"),
        questionList2html(ifQs)
      );
    case if_then_else(AExpr expression, list[AQuestion] ifQs, list[AQuestion] elseQs):
      return div(
        template(
          html5attr("v-if", "if_<expression.src.begin.line>_<expression.src.begin.column>"),
          questionList2html(ifQs)
        ),
        template(
          html5attr("v-else", ""),
          questionList2html(elseQs)
        )
      );
    default: return div();
  }
}

str type2htmlStr(AType questionType) {
  switch (questionType) {
    case boolean(): return "checkbox";
    case integer(): return "number";
    case string(): return "text";
  }
}

// Javascript string templates

str form2js(AForm f) {
  return "var app = new Vue({
  	     '  el: \'#app\',
  	     '  data: {
  	     '    <for (/AQuestion questionList := f.questions) {>
  	     '      <form2js(questionList)>
  	     '    <}>
  	     '  }
  	     '});";
}

str form2js(AQuestion question) {
  switch (question) {
    case normal(str label, AId id, AType questionType): 
      return "<id.name>: <defaultValue(questionType)>,";
  	  
    case computed(str label, AId id, AType questionType, AExpr expr): 
      return "<id.name>: function() {
  	     '          return <expr2js(expr)>;
  	     '        },";
  	     
    case block(list[AQuestion] questionList): 
      return "block: {
  	     '    <for (/AQuestion question := questionList) {>
  	     '      <form2js(question)>
  	     '    <}>
  	     '  },";
  	     
    case if_then(AExpr expr, list[AQuestion] ifQs):
      return "if (<expr2js(expr)>) : {
  	     '    <for (/AQuestion question := ifQs) {>
  	     '      <form2js(question)>
  	     '    <}>
  	     '  },";
  	     
    case if_then_else(AExpr expression, list[AQuestion] ifQs, list[AQuestion] elseQs):
      return "if (<expr2js(expr)>) : {
  	     '    <for (/AQuestion question := ifQs) {>
  	     '      <form2js(question)>
  	     '    <}>
  	     '  } else : {
  	     '    <for (/AQuestion question := elseQs) {>
  	     '      <form2js(question)>
  	     '    <}>
  	     '  },";
  	     
    default: return "";
  }
}

str defaultValue(AType typeName) {
  switch (typeName) {
    case boolean(): return "false";
    case integer(): return "0";
    case string(): return "\"\"";
  }
}

str expr2js(AExpr expression) {
  switch (expression) {
    case ref(AId id):
      return "this.<id.name>";
    case boolean(bool b):
      return "<b>";
    case integer(int i):
      return "<i>";
    case string(str s):
      return "<s>";
    case not(AExpr expr):
      return "!<expr2js(expr)>";
    case mul(AExpr a, AExpr b):
      return "(<expr2js(a)> * <expr2js(b)>)";
    case div(AExpr a, AExpr b):
      return "(<expr2js(a)> / <expr2js(b)>)";
    case add(AExpr a, AExpr b):
      return "(<expr2js(a)> + <expr2js(b)>)";
    case sub(AExpr a, AExpr b):
      return "(<expr2js(a)> - <expr2js(b)>)";
    case greater(AExpr a, AExpr b):
      return "(<expr2js(a)> \> <expr2js(b)>)";
    case less(AExpr a, AExpr b):
      return "(<expr2js(a)> \< <expr2js(b)>)";
    case leq(AExpr a, AExpr b):
      return "(<expr2js(a)> \<= <expr2js(b)>)";
    case geq(AExpr a, AExpr b):
      return "(<expr2js(a)> \>= <expr2js(b)>)";
    case eq(AExpr a, AExpr b):
      return "(<expr2js(a)> = <expr2js(b)>)";
    case neq(AExpr a, AExpr b):
      return "(<expr2js(a)> != <expr2js(b)>)";
    case and(AExpr a, AExpr b):
      return "(<expr2js(a)> && <expr2js(b)>)";
    case or(AExpr a, AExpr b):
      return "(<expr2js(a)> || <expr2js(b)>)";
    default: return "";
  }
}
