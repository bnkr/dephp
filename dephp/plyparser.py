import os, sys, logging
import ply.yacc as yacc

from dephp import phpast as ast
from dephp.scanner import tokens

precedence = (
    ('left', 'INCLUDE', 'INCLUDE_ONCE', 'EVAL', 'REQUIRE', 'REQUIRE_ONCE'),
    ('left', 'COMMA'),
    ('left', 'LOGICAL_OR'),
    ('left', 'LOGICAL_XOR'),
    ('left', 'LOGICAL_AND'),
    ('right', 'PRINT'),
    ('right', 'YIELD'),
    ('left', 'EQUALS', 'PLUS_EQUAL', 'MINUS_EQUAL', 'MUL_EQUAL', 'DIV_EQUAL',
        'CONCAT_EQUAL', 'MOD_EQUAL', 'AND_EQUAL', 'OR_EQUAL', 'XOR_EQUAL',
        'SL_EQUAL', 'SR_EQUAL', 'POW_EQUAL'),
    ('left', 'QUESTION', 'COLON'),
    ('left', 'BOOLEAN_OR'),
    ('left', 'BOOLEAN_AND'),
    ('left', 'OR'),
    ('left', 'XOR'),
    ('left', 'AND'),
    ('nonassoc', 'IS_EQUAL', 'IS_NOT_EQUAL', 'IS_IDENTICAL', 'IS_NOT_IDENTICAL'),
    ('nonassoc', 'IS_SMALLER', 'IS_SMALLER_OR_EQUAL', 'IS_GREATER', 'IS_GREATER_OR_EQUAL'),
    ('left', 'SL', 'SR'),
    ('left', 'PLUS', 'MINUS', 'CONCAT'),
    ('left', 'MUL', 'DIV', 'MOD'),
    ('right', 'BOOLEAN_NOT'),
    ('nonassoc', 'INSTANCEOF'),
    ('right', 'NOT', 'INC', 'DEC', 'INT_CAST', 'DOUBLE_CAST', 'STRING_CAST',
        'ARRAY_CAST', 'OBJECT_CAST', 'BOOL_CAST', 'UNSET_CAST', 'AT'),
    ('right', 'POW'),
    ('right', 'LBRACKET'),
    ('nonassoc', 'NEW', 'CLONE'),
    # ('left', 'ELSEIF'),
    # ('left', 'ELSE'),
    ('left', 'ENDIF'),
    ('right', 'STATIC', 'ABSTRACT', 'FINAL', 'PRIVATE', 'PROTECTED', 'PUBLIC'),
)

def p_start(p):
    '''start : top_statement_list'''
    p[0] = p[1]

def p_top_statement_list(p):
    '''top_statement_list : top_statement_list top_statement
                          | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

# namespace_name:
#     T_STRING { $$ = $1; }
#   |  namespace_name T_NS_SEPARATOR T_STRING { zend_do_build_namespace_name(&$$, &$1, &$3 TSRMLS_CC); }
# ;
def p_namespace_name(p):
    '''namespace_name : namespace_name NS_SEPARATOR STRING
                      | STRING'''
    if len(p) == 4:
        p[0] = p[1] + p[2] + p[3]
    else:
        p[0] = p[1]

# top_statement:
#     statement                        { zend_verify_namespace(TSRMLS_C); }
#     | function_declaration_statement { zend_verify_namespace(TSRMLS_C); zend_do_early_binding(TSRMLS_C); }
#     | class_declaration_statement    { zend_verify_namespace(TSRMLS_C); zend_do_early_binding(TSRMLS_C); }
#     | T_HALT_COMPILER '(' ')' ';'    { zend_do_halt_compiler_register(TSRMLS_C); YYACCEPT; }
#     | T_NAMESPACE namespace_name ';' { zend_do_begin_namespace(&$2, 0 TSRMLS_CC); }
#     | T_NAMESPACE namespace_name '{' { zend_do_begin_namespace(&$2, 1 TSRMLS_CC); }
#         top_statement_list '}'       { zend_do_end_namespace(TSRMLS_C); }
#     | T_NAMESPACE '{'                { zend_do_begin_namespace(NULL, 1 TSRMLS_CC); }
#         top_statement_list '}'       { zend_do_end_namespace(TSRMLS_C); }
#     | T_USE use_declarations ';'     { zend_verify_namespace(TSRMLS_C); }
#     | T_USE T_FUNCTION use_function_declarations ';' { zend_verify_namespace(TSRMLS_C); }
#     | T_USE T_CONST use_const_declarations ';'       { zend_verify_namespace(TSRMLS_C); }
#     | constant_declaration ';'       { zend_verify_namespace(TSRMLS_C); }
# ;
def p_top_statement(p):
    '''top_statement : statement
                     | function_declaration_statement
                     | class_declaration_statement
                     | constant_declaration SEMI'''
    p[0] = p[1]

def p_top_statement_halt(p):
    '''top_statement : HALT_COMPILER LPAREN RPAREN SEMI'''
    p[0] = ast.HaltCompiler(lineno=p.lineno(1))

def p_top_statement_namespace(p):
    '''top_statement : NAMESPACE namespace_name SEMI
                     | NAMESPACE LBRACE top_statement_list RBRACE
                     | NAMESPACE namespace_name LBRACE top_statement_list RBRACE'''
    if len(p) == 4:
        p[0] = ast.Namespace(p[2], [], lineno=p.lineno(1))
    elif len(p) == 5:
        p[0] = ast.Namespace(None, p[3], lineno=p.lineno(1))
    else:
        p[0] = ast.Namespace(p[2], p[4], lineno=p.lineno(1))

def p_top_statement_use(p):
    '''top_statement : USE use_declarations SEMI
                     | USE FUNCTION use_declarations SEMI
                     | USE CONST use_declarations SEMI'''
    # The zend parser does extra validation for each of these types of use
    # statement.  Since it's the same syntax we pretend it's the same thing for
    # now.  Phply leaves these out (presumably because it's a bit out of date).
    use = len(p) == 4 and p[2] or p[3]
    p[0] = ast.UseDeclarations(use, lineno=p.lineno(1))

def p_use_declarations(p):
    '''use_declarations : use_declarations COMMA use_declaration
                        | use_declaration'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_use_declaration(p):
    '''use_declaration : namespace_name
                       | namespace_name AS STRING
                       | NS_SEPARATOR namespace_name
                       | NS_SEPARATOR namespace_name AS STRING'''
    if len(p) == 2:
        p[0] = ast.UseDeclaration(p[1], None, lineno=p.lineno(1))
    elif len(p) == 3:
        p[0] = ast.UseDeclaration(p[1] + p[2], None, lineno=p.lineno(1))
    elif len(p) == 4:
        p[0] = ast.UseDeclaration(p[1], p[3], lineno=p.lineno(2))
    else:
        p[0] = ast.UseDeclaration(p[1] + p[2], p[4], lineno=p.lineno(1))

# constant_declaration:
#     constant_declaration ',' T_STRING '=' static_scalar  { zend_do_declare_constant(&$3, &$5 TSRMLS_CC); }
#   |  T_CONST T_STRING '=' static_scalar { zend_do_declare_constant(&$2, &$4 TSRMLS_CC); }
# ;
def p_constant_declaration(p):
    '''constant_declaration : STRING EQUALS static_scalar
                            | CONST STRING EQUALS static_scalar'''
    # Second option is an addition to phply.
    p[0] = ast.ConstantDeclaration(p[1], p[3], lineno=p.lineno(1))

# use_const_declarations:
#     use_const_declarations ',' use_const_declaration
#   |  use_const_declaration
# ;
#
# use_const_declaration:
#     namespace_name       { zend_do_use_const(&$1, NULL, 0 TSRMLS_CC); }
#   |  namespace_name T_AS T_STRING  { zend_do_use_const(&$1, &$3, 0 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name { zend_do_use_const(&$2, NULL, 1 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name T_AS T_STRING { zend_do_use_const(&$2, &$4, 1 TSRMLS_CC); }
# ;
#
# use_function_declarations:
#     use_function_declarations ',' use_function_declaration
#   |  use_function_declaration
# ;
#
# use_function_declaration:
#     namespace_name       { zend_do_use_function(&$1, NULL, 0 TSRMLS_CC); }
#   |  namespace_name T_AS T_STRING  { zend_do_use_function(&$1, &$3, 0 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name { zend_do_use_function(&$2, NULL, 1 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name T_AS T_STRING { zend_do_use_function(&$2, &$4, 1 TSRMLS_CC); }
# ;
#
# Omitting all of the above since they're identical to use_declaration.


# inner_statement_list:
#     inner_statement_list  { zend_do_extended_info(TSRMLS_C); } inner_statement { HANDLE_INTERACTIVE(); }
#   |  /* empty */
# ;
def p_inner_statement_list(p):
    '''inner_statement_list : inner_statement_list inner_statement
                            | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

# inner_statement:
#     statement
#   |  function_declaration_statement
#   |  class_declaration_statement
#   |  T_HALT_COMPILER '(' ')' ';'   { zend_error_noreturn(E_COMPILE_ERROR, "__HALT_COMPILER() can only be used from the outermost scope"); }
# ;
def p_inner_statement(p):
    '''inner_statement : statement
                       | function_declaration_statement
                       | class_declaration_statement
                       | HALT_COMPILER LPAREN RPAREN SEMI'''
    # assert len(p) == 2, "__HALT_COMPILER() can only be used from the outermost scope"
    p[0] = p[1]

# statement:
#     unticked_statement { DO_TICKS(); }
#   |  T_STRING ':' { zend_do_label(&$1 TSRMLS_CC); }
# ;
#
# This is an addition to phply, which doesn't support gotos at all.
def p_statement(p):
    '''statement : unticked_statement
                 | STRING COLON'''
    # TODO: needs verifying
    p[0] = p[1]

# unticked_statement:
#     '{' inner_statement_list '}'
def p_unticked_statement_block(p):
    'unticked_statement : LBRACE inner_statement_list RBRACE'
    p[0] = ast.Block(p[2], lineno=p.lineno(1))

#   |  T_IF parenthesis_expr
#      { zend_do_if_cond(&$2, &$1 TSRMLS_CC); }
#      statement
#      { zend_do_if_after_statement(&$1, 1 TSRMLS_CC); }
#      elseif_list else_single
#      { zend_do_if_end(TSRMLS_C); }
#   |  T_IF parenthesis_expr ':'
#      { zend_do_if_cond(&$2, &$1 TSRMLS_CC); }
#      inner_statement_list
#      { zend_do_if_after_statement(&$1, 1 TSRMLS_CC); }
#      new_elseif_list new_else_single T_ENDIF ';'
#      { zend_do_if_end(TSRMLS_C); }
def p_unticked_statement_if(p):
    '''unticked_statement : IF LPAREN expr RPAREN statement elseif_list else_single
                          | IF LPAREN expr RPAREN COLON inner_statement_list new_elseif_list new_else_single ENDIF SEMI'''
    if len(p) == 8:
        p[0] = ast.If(p[3], p[5], p[6], p[7], lineno=p.lineno(1))
    else:
        p[0] = ast.If(p[3], ast.Block(p[6], lineno=p.lineno(5)),
                      p[7], p[8], lineno=p.lineno(1))

#   |  T_WHILE
#      { $1.u.op.opline_num = get_next_op_number(CG(active_op_array)); }
#      parenthesis_expr
#      { zend_do_while_cond(&$3, &$$ TSRMLS_CC); }
#      while_statement { zend_do_while_end(&$1, &$4 TSRMLS_CC); }
def p_unticked_statement_while(p):
    'unticked_statement : WHILE LPAREN expr RPAREN while_statement'
    p[0] = ast.While(p[3], p[5], lineno=p.lineno(1))

#   |  T_DO
#      { $1.u.op.opline_num = get_next_op_number(CG(active_op_array));  zend_do_do_while_begin(TSRMLS_C); }
#      statement T_WHILE
#      { $4.u.op.opline_num = get_next_op_number(CG(active_op_array)); }
#      parenthesis_expr ';' { zend_do_do_while_end(&$1, &$4, &$6 TSRMLS_CC); }
def p_unticked_statement_do_while(p):
    'unticked_statement : DO statement WHILE LPAREN expr RPAREN SEMI'
    p[0] = ast.DoWhile(p[2], p[5], lineno=p.lineno(1))

#   |  T_FOR
#       '('
#         for_expr
#       ';' { zend_do_free(&$3 TSRMLS_CC); $4.u.op.opline_num = get_next_op_number(CG(active_op_array)); }
#         for_expr
#       ';' { zend_do_extended_info(TSRMLS_C); zend_do_for_cond(&$6, &$7 TSRMLS_CC); }
#         for_expr
#       ')' { zend_do_free(&$9 TSRMLS_CC); zend_do_for_before_statement(&$4, &$7 TSRMLS_CC); }
#       for_statement { zend_do_for_end(&$7 TSRMLS_CC); }
def p_unticked_statement_for(p):
    'unticked_statement : FOR LPAREN for_expr SEMI for_expr SEMI for_expr RPAREN for_statement'
    p[0] = ast.For(p[3], p[5], p[7], p[9], lineno=p.lineno(1))

#   |  T_SWITCH parenthesis_expr
#       { zend_do_switch_cond(&$2 TSRMLS_CC); }
#       switch_case_list { zend_do_switch_end(&$4 TSRMLS_CC); }
def p_unticked_statement_switch(p):
    'unticked_statement : SWITCH LPAREN expr RPAREN switch_case_list'
    p[0] = ast.Switch(p[3], p[5], lineno=p.lineno(1))

#   |  T_BREAK ';'        { zend_do_brk_cont(ZEND_BRK, NULL TSRMLS_CC); }
#   |  T_BREAK expr ';'    { zend_do_brk_cont(ZEND_BRK, &$2 TSRMLS_CC); }
def p_unticked_statement_break(p):
    '''unticked_statement : BREAK SEMI
                          | BREAK expr SEMI'''
    if len(p) == 3:
        p[0] = ast.Break(None, lineno=p.lineno(1))
    else:
        p[0] = ast.Break(p[2], lineno=p.lineno(1))

#   |  T_CONTINUE ';'      { zend_do_brk_cont(ZEND_CONT, NULL TSRMLS_CC); }
#   |  T_CONTINUE expr ';'    { zend_do_brk_cont(ZEND_CONT, &$2 TSRMLS_CC); }
def p_unticked_statement_continue(p):
    '''unticked_statement : CONTINUE SEMI
                          | CONTINUE expr SEMI'''
    if len(p) == 3:
        p[0] = ast.Continue(None, lineno=p.lineno(1))
    else:
        p[0] = ast.Continue(p[2], lineno=p.lineno(1))

#   |  T_RETURN ';'            { zend_do_return(NULL, 0 TSRMLS_CC); }
#   |  T_RETURN expr_without_variable ';'  { zend_do_return(&$2, 0 TSRMLS_CC); }
#   |  T_RETURN variable ';'        { zend_do_return(&$2, 1 TSRMLS_CC); }
def p_unticked_statement_return(p):
    '''unticked_statement : RETURN SEMI
                 | RETURN expr SEMI'''
    # TODO:
    #   Doesn't match zend's grammar.
    if len(p) == 3:
        p[0] = ast.Return(None, lineno=p.lineno(1))
    else:
        p[0] = ast.Return(p[2], lineno=p.lineno(1))

#   |  yield_expr ';' { zend_do_free(&$1 TSRMLS_CC); }
def p_unticked_statement_yield(p):
    '''unticked_statement : yield_expr SEMI'''
    # Additional to phply.
    # TODO: check code
    p[0] = ast.Yield(p[1])

#   |  T_GLOBAL global_var_list ';'
def p_unticked_statement_global(p):
    'unticked_statement : GLOBAL global_var_list SEMI'
    p[0] = ast.Global(p[2], lineno=p.lineno(1))

#   |  T_STATIC static_var_list ';'
def p_unticked_statement_static(p):
    'unticked_statement : STATIC static_var_list SEMI'
    p[0] = ast.Static(p[2], lineno=p.lineno(1))

#   |  T_ECHO echo_expr_list ';'
def p_unticked_statement_echo(p):
    'unticked_statement : ECHO echo_expr_list SEMI'
    p[0] = ast.Echo(p[2], lineno=p.lineno(1))

#   |  T_INLINE_HTML      { zend_do_echo(&$1 TSRMLS_CC); }
def p_unticked_statement_inline_html(p):
    'unticked_statement : INLINE_HTML'
    p[0] = ast.InlineHTML(p[1], lineno=p.lineno(1))

#   |  expr ';'        { zend_do_free(&$1 TSRMLS_CC); }
def p_unticked_statement_expr(p):
    'unticked_statement : expr SEMI'
    p[0] = p[1]

#   |  T_UNSET '(' unset_variables ')' ';'
def p_unticked_statement_unset(p):
    'unticked_statement : UNSET LPAREN unset_variables RPAREN SEMI'
    p[0] = ast.Unset(p[3], lineno=p.lineno(1))

#   |  T_FOREACH '(' variable T_AS
#     { zend_do_foreach_begin(&$1, &$2, &$3, &$4, 1 TSRMLS_CC); }
#     foreach_variable foreach_optional_arg ')' { zend_do_foreach_cont(&$1, &$2, &$4, &$6, &$7 TSRMLS_CC); }
#     foreach_statement { zend_do_foreach_end(&$1, &$4 TSRMLS_CC); }
#   |  T_FOREACH '(' expr_without_variable T_AS
#     { zend_do_foreach_begin(&$1, &$2, &$3, &$4, 0 TSRMLS_CC); }
#     foreach_variable foreach_optional_arg ')'
#     { zend_do_foreach_cont(&$1, &$2, &$4, &$6, &$7 TSRMLS_CC); }
#     foreach_statement { zend_do_foreach_end(&$1, &$4 TSRMLS_CC); }
def p_unticked_statement_foreach(p):
    'unticked_statement : FOREACH LPAREN expr AS foreach_variable foreach_optional_arg RPAREN foreach_statement'
    if p[6] is None:
        p[0] = ast.Foreach(p[3], None, p[5], p[8], lineno=p.lineno(1))
    else:
        p[0] = ast.Foreach(p[3], p[5], p[6], p[8], lineno=p.lineno(1))

#   | T_DECLARE
#     { $1.u.op.opline_num = get_next_op_number(CG(active_op_array));
#       zend_do_declare_begin(TSRMLS_C); }
#     '(' declare_list ')' declare_statement
#     { zend_do_declare_end(&$1 TSRMLS_CC); }
#
# This is additional to phply which doesn't cover DECLARE.
def p_unticked_statement_declare(p):
    'unticked_statement : DECLARE LPAREN declare_list RPAREN declare_statement'
    p[0] = ast.Declare(p[3], p[5], lineno=p.lineno(1))

#   |  ';'    /* empty statement */
def p_unticked_statement_empty(p):
    'unticked_statement : SEMI'
    pass

#   |  T_TRY { zend_do_try(&$1 TSRMLS_CC); } '{' inner_statement_list '}'
#     catch_statement { zend_do_bind_catch(&$1, &$6 TSRMLS_CC); }
#     finally_statement { zend_do_end_finally(&$1, &$6, &$8 TSRMLS_CC); }
def p_unticked_statement_try(p):
    'unticked_statement : TRY LBRACE inner_statement_list RBRACE catch_statement finally_statement'
    # Changed from phply.  Original was:
    #
    #   'statement : TRY LBRACE inner_statement_list RBRACE CATCH LPAREN
    #   fully_qualified_class_name VARIABLE RPAREN LBRACE inner_statement_list
    #   RBRACE additional_catches'
    #
    # Probably won't work.
    # p[0] = ast.Try(p[3], [ast.Catch(p[7], ast.Variable(p[8], lineno=p.lineno(8)),
    #                                 p[11], lineno=p.lineno(5))] + p[13],
    #                lineno=p.lineno(1))

#   |  T_THROW expr ';' { zend_do_throw(&$2 TSRMLS_CC); }
def p_unticked_statement_throw(p):
    'statement : THROW expr SEMI'
    p[0] = ast.Throw(p[2], lineno=p.lineno(1))

#   |  T_GOTO T_STRING ';' { zend_do_goto(&$2 TSRMLS_CC); } ;
#
# This is additional to phply which doesn't cover gotos.
def p_unticked_statement_goto(p):
    'statement : GOTO STRING SEMI'
    p[0] = ast.Throw(p[2], lineno=p.lineno(1))

# catch_statement:
#         /* empty */ { $$.op_type = IS_UNUSED; }
#   |  T_CATCH '(' { zend_initialize_try_catch_element(&$1 TSRMLS_CC); }
#     fully_qualified_class_name { zend_do_first_catch(&$2 TSRMLS_CC); }
#     T_VARIABLE ')' { zend_do_begin_catch(&$1, &$4, &$6, &$2 TSRMLS_CC); }
#     '{' inner_statement_list '}' { zend_do_end_catch(&$1 TSRMLS_CC); }
#     additional_catches { zend_do_mark_last_catch(&$2, &$13 TSRMLS_CC); $$ = $1;}
def p_catch_statement(p):
    '''catch_statement : empty
                       | CATCH LPAREN fully_qualified_class_name VARIABLE RPAREN LBRACE inner_statement_list RBRACE additional_catches'''
    pass

# additional_catches:
#     non_empty_additional_catches { $$ = $1; }
#   |  /* empty */ { $$.u.op.opline_num = -1; }
# ;
def p_additional_catches(p):
    '''additional_catches : non_empty_additional_catches
                          | empty'''
    # In phply it used to be:
    #
    #   additional_catches CATCH LPAREN fully_qualified_class_name VARIABLE
    #   RPAREN LBRACE inner_statement_list RBRACE
    #
    # And this rule was direct in the try_statement rule.
    # if len(1) == 1:
    #     p[0] = p[1]

# non_empty_additional_catches:
#     additional_catch { $$ = $1; }
#   |  non_empty_additional_catches additional_catch { $$ = $2; }
# ;
def p_non_empty_additional_catches(p):
    '''non_empty_additional_catches : additional_catch
                                    | non_empty_additional_catches additional_catch'''

# additional_catch:
#   T_CATCH '(' fully_qualified_class_name
#   { $$.u.op.opline_num = get_next_op_number(CG(active_op_array)); }
#   T_VARIABLE ')'
#   { zend_do_begin_catch(&$1, &$3, &$5, NULL TSRMLS_CC); }
#   '{' inner_statement_list '}'
#   { zend_do_end_catch(&$1 TSRMLS_CC); }
# ;
def p_additional_catch(p):
    '''additional_catch : CATCH LPAREN fully_qualified_class_name VARIABLE RPAREN LBRACE inner_statement_list RBRACE'''

# finally_statement:
#           /* empty */ { $$.op_type = IS_UNUSED; }
#   |  T_FINALLY { zend_do_finally(&$1 TSRMLS_CC); } '{' inner_statement_list '}' { $$ = $1; }
# ;
def p_finally_statement(p):
    '''finally_statement : empty
                         | FINALLY LBRACE inner_statement_list RBRACE'''

# unset_variables:
#     unset_variable
#   |  unset_variables ',' unset_variable
# ;
def p_unset_variables(p):
    '''unset_variables : unset_variables COMMA unset_variable
                       | unset_variable'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# unset_variable:
#     variable  { zend_do_end_variable_parse(&$1, BP_VAR_UNSET, 0 TSRMLS_CC); zend_do_unset(&$1 TSRMLS_CC); }
# ;
def p_unset_variable(p):
    'unset_variable : variable'
    p[0] = p[1]

# function_declaration_statement:
#     unticked_function_declaration_statement  { DO_TICKS(); }
# ;
#
# unticked_function_declaration_statement:
#     function is_reference T_STRING { zend_do_begin_function_declaration(&$1, &$3, 0, $2.op_type, NULL TSRMLS_CC); }
#     '(' parameter_list ')'
#     '{' inner_statement_list '}' { zend_do_end_function_declaration(&$1 TSRMLS_CC); }
# ;
def p_function_declaration_statement(p):
    'function_declaration_statement : function is_reference STRING LPAREN parameter_list RPAREN LBRACE inner_statement_list RBRACE'
    p[0] = ast.Function(p[3], p[5], p[8], p[2], lineno=p.lineno(1))

# class_declaration_statement:
#     unticked_class_declaration_statement  { DO_TICKS(); }
# ;
#
# unticked_class_declaration_statement:
#     class_entry_type T_STRING extends_from
#       { zend_do_begin_class_declaration(&$1, &$2, &$3 TSRMLS_CC); }
#       implements_list
#       '{'
#         class_statement_list
#       '}' { zend_do_end_class_declaration(&$1, &$3 TSRMLS_CC); }
#   |  interface_entry T_STRING
#       { zend_do_begin_class_declaration(&$1, &$2, NULL TSRMLS_CC); }
#       interface_extends_list
#       '{'
#         class_statement_list
#       '}' { zend_do_end_class_declaration(&$1, NULL TSRMLS_CC); }
# ;
#
# interface_entry:
#   T_INTERFACE    { $$.u.op.opline_num = CG(zend_lineno); $$.EA = ZEND_ACC_INTERFACE; }
# ;
#
def p_class_declaration_statement(p):
    '''class_declaration_statement : class_entry_type STRING extends_from implements_list LBRACE class_statement_list RBRACE
                                   | INTERFACE STRING interface_extends_list LBRACE class_statement_list RBRACE'''
    if len(p) == 8:
        p[0] = ast.Class(p[2], p[1], p[3], p[4], p[6], lineno=p.lineno(2))
    else:
        p[0] = ast.Interface(p[2], p[3], p[5], lineno=p.lineno(1))

# class_entry_type:
#     T_CLASS      { $$.u.op.opline_num = CG(zend_lineno); $$.EA = 0; }
#   |  T_ABSTRACT T_CLASS { $$.u.op.opline_num = CG(zend_lineno); $$.EA = ZEND_ACC_EXPLICIT_ABSTRACT_CLASS; }
#   |  T_TRAIT { $$.u.op.opline_num = CG(zend_lineno); $$.EA = ZEND_ACC_TRAIT; }
#   |  T_FINAL T_CLASS { $$.u.op.opline_num = CG(zend_lineno); $$.EA = ZEND_ACC_FINAL_CLASS; }
# ;
def p_class_entry_type(p):
    '''class_entry_type : CLASS
                        | ABSTRACT CLASS
                        | TRAIT
                        | FINAL CLASS'''
    if len(p) == 3:
        p[0] = p[1].lower()

# interface_extends_list:
#     /* empty */
#   |  T_EXTENDS interface_list
# ;
def p_interface_extends_list(p):
    '''interface_extends_list : EXTENDS interface_list
                              | empty'''
    if len(p) == 3:
        p[0] = p[2]

# implements_list:
#     /* empty */
#   |  T_IMPLEMENTS interface_list
# ;
def p_implements_list(p):
    '''implements_list : IMPLEMENTS interface_list
                       | empty'''
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = []

# extends_from:
#     /* empty */          { $$.op_type = IS_UNUSED; }
#   |  T_EXTENDS fully_qualified_class_name  { zend_do_fetch_class(&$$, &$2 TSRMLS_CC); }
# ;
def p_extends_from(p):
    '''extends_from : empty
                    | EXTENDS fully_qualified_class_name'''
    if len(p) == 3:
        p[0] = p[2]

# interface_list:
#     fully_qualified_class_name      { zend_do_implements_interface(&$1 TSRMLS_CC); }
#   |  interface_list ',' fully_qualified_class_name { zend_do_implements_interface(&$3 TSRMLS_CC); }
# ;
def p_interface_list(p):
    '''interface_list : interface_list COMMA fully_qualified_class_name
                      | fully_qualified_class_name'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# is_reference:
#     /* empty */  { $$.op_type = 0; }
#   |  '&'      { $$.op_type = 1; }
# ;
def p_is_reference(p):
    '''is_reference : AND
                    | empty'''
    p[0] = p[1] is not None

# is_variadic:
#     /* empty */ { $$.op_type = 0; }
#   |  T_ELLIPSIS  { $$.op_type = 1; }
# ;
#
# Doesn't exist in phply.
def p_is_variadic(p):
    '''is_variadic : ELLIPSIS
                   | empty'''
    pass

# for_statement:
#     statement
#   |  ':' inner_statement_list T_ENDFOR ';'
# ;
def p_for_statement(p):
    '''for_statement : statement
                     | COLON inner_statement_list ENDFOR SEMI'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ast.Block(p[2], lineno=p.lineno(1))

# foreach_variable:
#     variable      { zend_check_writable_variable(&$1); $$ = $1; }
#   |  '&' variable    { zend_check_writable_variable(&$2); $$ = $2;  $$.EA |= ZEND_PARSED_REFERENCE_VARIABLE; }
#   |  T_LIST '(' { zend_do_list_init(TSRMLS_C); } assignment_list ')' { $$ = $1; $$.EA = ZEND_PARSED_LIST_EXPR; }
# ;
def p_foreach_variable(p):
    '''foreach_variable : VARIABLE
                        | AND VARIABLE
                        | LIST LPAREN assignment_list RPAREN'''
    # TODO: 3rd not covered
    if len(p) == 2:
        p[0] = ast.ForeachVariable(p[1], False, lineno=p.lineno(1))
    else:
        p[0] = ast.ForeachVariable(p[2], True, lineno=p.lineno(1))

# foreach_optional_arg:
#     /* empty */            { $$.op_type = IS_UNUSED; }
#   |  T_DOUBLE_ARROW foreach_variable  { $$ = $2; }
# ;
def p_foreach_optional_arg(p):
    '''foreach_optional_arg : empty
                            | DOUBLE_ARROW foreach_variable'''
    if len(p) == 3:
        p[0] = p[2]

# foreach_statement:
#     statement
#   |  ':' inner_statement_list T_ENDFOREACH ';'
# ;
def p_foreach_statement(p):
    '''foreach_statement : statement
                         | COLON inner_statement_list ENDFOREACH SEMI'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ast.Block(p[2], lineno=p.lineno(1))

# declare_statement:
#     statement
#   |  ':' inner_statement_list T_ENDDECLARE ';'
# ;
def p_declare_statement(p):
    '''declare_statement : statement
                         | COLON inner_statement_list ENDDECLARE SEMI'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ast.Block(p[2], lineno=p.lineno(1))

# declare_list:
#     T_STRING '=' static_scalar          { zend_do_declare_stmt(&$1, &$3 TSRMLS_CC); }
#   |  declare_list ',' T_STRING '=' static_scalar  { zend_do_declare_stmt(&$3, &$5 TSRMLS_CC); }
# ;
def p_declare_list(p):
    '''declare_list : STRING EQUALS static_scalar
                    | declare_list COMMA STRING EQUALS static_scalar'''
    if len(p) == 4:
        p[0] = [ast.Directive(p[1], p[3], lineno=p.lineno(1))]
    else:
        p[0] = p[1] + [ast.Directive(p[3], p[5], lineno=p.lineno(2))]


# switch_case_list:
#     '{' case_list '}'          { $$ = $2; }
#   |  '{' ';' case_list '}'        { $$ = $3; }
#   |  ':' case_list T_ENDSWITCH ';'    { $$ = $2; }
#   |  ':' ';' case_list T_ENDSWITCH ';'  { $$ = $3; }
# ;
def p_switch_case_list(p):
    '''switch_case_list : LBRACE case_list RBRACE
                        | LBRACE SEMI case_list RBRACE'''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[3]

def p_switch_case_list_colon(p):
    '''switch_case_list : COLON case_list ENDSWITCH SEMI
                        | COLON SEMI case_list ENDSWITCH SEMI'''
    if len(p) == 5:
        p[0] = p[2]
    else:
        p[0] = p[3]

# case_list:
#     /* empty */  { $$.op_type = IS_UNUSED; }
#   |  case_list T_CASE expr case_separator { zend_do_extended_info(TSRMLS_C);  zend_do_case_before_statement(&$1, &$2, &$3 TSRMLS_CC); } inner_statement_list { zend_do_case_after_statement(&$$, &$2 TSRMLS_CC); $$.op_type = IS_CONST; }
#   |  case_list T_DEFAULT case_separator { zend_do_extended_info(TSRMLS_C);  zend_do_default_before_statement(&$1, &$2 TSRMLS_CC); } inner_statement_list { zend_do_case_after_statement(&$$, &$2 TSRMLS_CC); $$.op_type = IS_CONST; }
# ;
def p_case_list(p):
    '''case_list : empty
                 | case_list CASE expr case_separator inner_statement_list
                 | case_list DEFAULT case_separator inner_statement_list'''
    if len(p) == 6:
        p[0] = p[1] + [ast.Case(p[3], p[5], lineno=p.lineno(2))]
    elif len(p) == 5:
        p[0] = p[1] + [ast.Default(p[4], lineno=p.lineno(2))]
    else:
        p[0] = []

# case_separator:
#     ':'
#   |  ';'
# ;
def p_case_separator(p):
    '''case_separator : COLON
                      | SEMI'''
    pass

# while_statement:
#     statement
#   |  ':' inner_statement_list T_ENDWHILE ';'
# ;
def p_while_statement(p):
    '''while_statement : statement
                       | COLON inner_statement_list ENDWHILE SEMI'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ast.Block(p[2], lineno=p.lineno(1))

# elseif_list:
#     /* empty */
#   |  elseif_list T_ELSEIF parenthesis_expr { zend_do_if_cond(&$3, &$2 TSRMLS_CC); } statement { zend_do_if_after_statement(&$2, 0 TSRMLS_CC); }
# ;
def p_elseif_list(p):
    '''elseif_list : empty
                   | elseif_list ELSEIF LPAREN expr RPAREN statement'''
    # TODO: needs checking
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [ast.ElseIf(p[4], p[6], lineno=p.lineno(2))]

# new_elseif_list:
#     /* empty */
#   |  new_elseif_list T_ELSEIF parenthesis_expr ':' { zend_do_if_cond(&$3, &$2 TSRMLS_CC); } inner_statement_list { zend_do_if_after_statement(&$2, 0 TSRMLS_CC); }
# ;
def p_new_elseif_list(p):
    '''new_elseif_list : empty
                       | new_elseif_list ELSEIF LPAREN expr RPAREN COLON inner_statement_list'''
    # TODO: needs checking
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [ast.ElseIf(p[4], ast.Block(p[7], lineo=p.lineno(6)),
                                  lineno=p.lineno(2))]

# else_single:
#     /* empty */
#   |  T_ELSE statement
# ;
def p_else_single(p):
    '''else_single : empty
                   | ELSE statement'''
    if len(p) == 3:
        p[0] = ast.Else(p[2], lineno=p.lineno(1))

# new_else_single:
#     /* empty */
#   |  T_ELSE ':' inner_statement_list
# ;
def p_new_else_single(p):
    '''new_else_single : empty
                       | ELSE COLON inner_statement_list'''
    if len(p) == 4:
        p[0] = ast.Else(ast.Block(p[3], lineno=p.lineno(2)),
                        lineno=p.lineno(1))

# parameter_list:
#     non_empty_parameter_list
#   |  /* empty */
# ;
def p_parameter_list(p):
    '''parameter_list : non_empty_parameter_list'''
    p[0] = p[1]

def p_parameter_list_empty(p):
    'parameter_list : empty'
    p[0] = []

# non_empty_parameter_list:
#     parameter
#   |  non_empty_parameter_list ',' parameter
# ;
def p_non_empty_parameter_list(p):
    '''non_empty_parameter_list : parameter
                                | non_empty_parameter_list COMMA parameter'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# parameter:
#     optional_class_type is_reference is_variadic T_VARIABLE
#       { zend_do_receive_param(ZEND_RECV, &$4, NULL, &$1, $2.op_type, $3.op_type TSRMLS_CC); }
#   |  optional_class_type is_reference is_variadic T_VARIABLE '=' static_scalar
#       { zend_do_receive_param(ZEND_RECV_INIT, &$4, &$6, &$1, $2.op_type, $3.op_type TSRMLS_CC); }
# ;
def p_parameter(p):
    '''parameter : optional_class_type is_reference is_variadic VARIABLE
                 | optional_class_type is_reference is_variadic VARIABLE EQUALS static_scalar'''
    # '''parameter : VARIABLE
    #              | class_name VARIABLE
    #              | AND VARIABLE
    #              | class_name AND VARIABLE
    #              | VARIABLE EQUALS static_scalar
    #              | class_name VARIABLE EQUALS static_scalar
    #              | AND VARIABLE EQUALS static_scalar
    #              | class_name AND VARIABLE EQUALS static_scalar'''
    # assert False # TODO: very different in phply

    # if len(p) == 2: # VARIABLE
    #     p[0] = ast.FormalParameter(p[1], None, False, None, lineno=p.lineno(1))
    # elif len(p) == 3 and p[1] == '&': # AND VARIABLE
    #     p[0] = ast.FormalParameter(p[2], None, True, None, lineno=p.lineno(1))
    # elif len(p) == 3 and p[1] != '&': # STRING VARIABLE
    #     p[0] = ast.FormalParameter(p[2], None, False, p[1], lineno=p.lineno(1))
    # elif len(p) == 4 and p[2] != '&': # VARIABLE EQUALS static_scalar
    #     p[0] = ast.FormalParameter(p[1], p[3], False, None, lineno=p.lineno(1))
    # elif len(p) == 4 and p[2] == '&': # STRING AND VARIABLE
    #     p[0] = ast.FormalParameter(p[3], None, True, p[1], lineno=p.lineno(1))
    # elif len(p) == 5 and p[1] == '&': # AND VARIABLE EQUALS static_scalar
    #     p[0] = ast.FormalParameter(p[2], p[4], True, None, lineno=p.lineno(1))
    # elif len(p) == 5 and p[1] != '&': # class_name VARIABLE EQUALS static_scalar
    #     p[0] = ast.FormalParameter(p[2], p[4], False, p[1], lineno=p.lineno(1))
    # else: # STRING AND VARIABLE EQUALS static_scalar
    #     p[0] = ast.FormalParameter(p[3], p[5], True, p[1], lineno=p.lineno(1))

# optional_class_type:
#     /* empty */          { $$.op_type = IS_UNUSED; }
#   |  T_ARRAY            { $$.op_type = IS_CONST; Z_TYPE($$.u.constant)=IS_ARRAY; }
#   |  T_CALLABLE          { $$.op_type = IS_CONST; Z_TYPE($$.u.constant)=IS_CALLABLE; }
#   |  fully_qualified_class_name      { $$ = $1; }
# ;
def p_optional_class_type(p):
    '''optional_class_type : empty
                           | ARRAY
                           | CALLABLE
                           | fully_qualified_class_name'''
    # assert False # TODO: missing in phply

# function_call_parameter_list:
#     '(' ')'  { Z_LVAL($$.u.constant) = 0; }
#   |  '(' non_empty_function_call_parameter_list ')'  { $$ = $2; }
#   |  '(' yield_expr ')'  { zend_do_pass_param(&$2, ZEND_SEND_VAL TSRMLS_CC); }
# ;
#
# This is re-written from phply to match php.
def p_function_call_parameter_list(p):
    '''function_call_parameter_list : LPAREN RPAREN
                                    | LPAREN non_empty_function_call_parameter_list RPAREN
                                    | LPAREN yield_expr RPAREN'''
    # Original:
    #
    #   function_call_parameter_list : function_call_parameter_list COMMA function_call_parameter
    #                                | function_call_parameter'''
    #   function_call_parameter_list : empty
    #
    # Hm.
    # if len(p) == 4:
    #     p[0] = p[1] + [p[3]]
    # else:
    #     p[0] = [p[1]]

# non_empty_function_call_parameter_list:
#     function_call_parameter
#   |  non_empty_function_call_parameter_list ',' function_call_parameter
# ;
def p_non_empty_function_call_parameter_list(p):
    '''non_empty_function_call_parameter_list : function_call_parameter
                                              | non_empty_function_call_parameter_list COMMA function_call_parameter'''
    # assert False # TODO: missing in phply

# function_call_parameter:
#     expr_without_variable  { zend_do_pass_param(&$1, ZEND_SEND_VAL TSRMLS_CC); }
#   |  variable        { zend_do_pass_param(&$1, ZEND_SEND_VAR TSRMLS_CC); }
#   |  '&' w_variable       { zend_do_pass_param(&$2, ZEND_SEND_REF TSRMLS_CC); }
#   |  T_ELLIPSIS expr      { zend_do_unpack_params(&$2 TSRMLS_CC); }
# ;
def p_function_call_parameter(p):
    '''function_call_parameter : expr_without_variable
                               | variable
                               | AND w_variable
                               | ELLIPSIS expr'''
    # TODO: added elipsis and the w_variable thing
    #
    # Originally this:
    #
    #   function_call_parameter : expr
    #                           | AND variable
    #
    if len(p) == 2:
        p[0] = ast.Parameter(p[1], False, lineno=p.lineno(1))
    else:
        p[0] = ast.Parameter(p[2], True, lineno=p.lineno(1))

# global_var_list:
#     global_var_list ',' global_var  { zend_do_fetch_global_variable(&$3, NULL, ZEND_FETCH_GLOBAL_LOCK TSRMLS_CC); }
#   |  global_var            { zend_do_fetch_global_variable(&$1, NULL, ZEND_FETCH_GLOBAL_LOCK TSRMLS_CC); }
# ;
def p_global_var_list(p):
    '''global_var_list : global_var_list COMMA global_var
                       | global_var'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# global_var:
#     T_VARIABLE      { $$ = $1; }
#   |  '$' r_variable    { $$ = $2; }
#   |  '$' '{' expr '}'  { $$ = $3; }
# ;
def p_global_var(p):
    '''global_var : VARIABLE
                  | DOLLAR r_variable
                  | DOLLAR LBRACE expr RBRACE'''
    if len(p) == 2:
        p[0] = ast.Variable(p[1], lineno=p.lineno(1))
    elif len(p) == 3:
        p[0] = ast.Variable(p[2], lineno=p.lineno(1))
    else:
        p[0] = ast.Variable(p[3], lineno=p.lineno(1))

# static_var_list:
#     static_var_list ',' T_VARIABLE { zend_do_fetch_static_variable(&$3, NULL, ZEND_FETCH_STATIC TSRMLS_CC); }
#   |  static_var_list ',' T_VARIABLE '=' static_scalar { zend_do_fetch_static_variable(&$3, &$5, ZEND_FETCH_STATIC TSRMLS_CC); }
#   |  T_VARIABLE  { zend_do_fetch_static_variable(&$1, NULL, ZEND_FETCH_STATIC TSRMLS_CC); }
#   |  T_VARIABLE '=' static_scalar { zend_do_fetch_static_variable(&$1, &$3, ZEND_FETCH_STATIC TSRMLS_CC); }
# ;
def p_static_var_list(p):
    '''static_var_list : static_var_list COMMA VARIABLE
                       | static_var_list VARIABLE EQUALS static_scalar
                       | VARIABLE
                       | VARIABLE EQUALS static_scalar
                       '''
    # was:
    #
    # static_var_list : static_var_list COMMA static_var
    #                 | static_var
    #
    # static_var : VARIABLE EQUALS static_scalar
    #            | VARIABLE
    #
    # assert False
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# class_statement_list:
#     class_statement_list class_statement
#   |  /* empty */
# ;
def p_class_statement_list(p):
    '''class_statement_list : class_statement_list class_statement
                            | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

# class_statement:
#     variable_modifiers { CG(access_type) = Z_LVAL($1.u.constant); } class_variable_declaration ';'
#   |  class_constant_declaration ';'
#   |  trait_use_statement
#   |  method_modifiers function is_reference T_STRING { zend_do_begin_function_declaration(&$2, &$4, 1, $3.op_type, &$1 TSRMLS_CC); }
#     '(' parameter_list ')'
#     method_body { zend_do_abstract_method(&$4, &$1, &$9 TSRMLS_CC); zend_do_end_function_declaration(&$2 TSRMLS_CC); }
# ;
def p_class_statement(p):
    '''class_statement : variable_modifiers class_variable_declaration SEMI
                       | class_constant_declaration SEMI
                       | trait_use_statement
                       | method_modifiers function is_reference STRING LPAREN parameter_list RPAREN method_body'''
    # Originally:
    #
    #    '''class_statement : method_modifiers FUNCTION is_reference STRING LPAREN parameter_list RPAREN method_body
    #                  | variable_modifiers class_variable_declaration SEMI
    #                  | class_constant_declaration SEMI'''

    if len(p) == 9:
        p[0] = ast.Method(p[4], p[1], p[6], p[8], p[3], lineno=p.lineno(2))
    elif len(p) == 4:
        p[0] = ast.ClassVariables(p[1], p[2], lineno=p.lineno(3))
    elif len(p) == 1:
        # assert False # missing action
        pass
    else:
        p[0] = ast.ClassConstants(p[1], lineno=p.lineno(2))

# trait_use_statement:
#     T_USE trait_list trait_adaptations
# ;
def p_trait_use_statement(p):
    '''trait_use_statement : USE trait_list trait_adaptations'''
    # assert False # TODO: missing trait_use_statement

# trait_list:
#     fully_qualified_class_name            { zend_do_use_trait(&$1 TSRMLS_CC); }
#   |  trait_list ',' fully_qualified_class_name    { zend_do_use_trait(&$3 TSRMLS_CC); }
# ;
def p_trait_list(p):
    '''trait_list : fully_qualified_class_name
                  |  trait_list COMMA fully_qualified_class_name'''
    # assert False # TODO: missing trait_use_statement

# trait_adaptations:
#     ';'
#   |  '{' trait_adaptation_list '}'
# ;
def p_trait_adaptations(p):
    '''trait_adaptations : SEMI
                         | LBRACE trait_adaptation_list RBRACE'''
    # assert False # TODO: missing trait_use_statement

# trait_adaptation_list:
#     /* empty */
#   |  non_empty_trait_adaptation_list
# ;
def p_trait_adaptation_list(p):
    '''trait_adaptation_list : empty
                             | non_empty_trait_adaptation_list'''
    # assert False

# non_empty_trait_adaptation_list:
#     trait_adaptation_statement
#   |  non_empty_trait_adaptation_list trait_adaptation_statement
# ;
def p_non_empty_trait_adaptation_list(p):
    '''non_empty_trait_adaptation_list : trait_adaptation_statement
                                       |  non_empty_trait_adaptation_list trait_adaptation_statement'''
    # assert False

# trait_adaptation_statement:
#     trait_precedence ';'
#   |  trait_alias ';'
# ;
def p_trait_adaptation_statement(p):
    '''trait_adaptation_statement : trait_precedence SEMI
                                  | trait_alias SEMI'''
    # assert False

# trait_precedence:
#   trait_method_reference_fully_qualified T_INSTEADOF trait_reference_list  { zend_add_trait_precedence(&$1, &$3 TSRMLS_CC); }
# ;
def p_trait_precedence(p):
    '''trait_precedence : trait_method_reference_fully_qualified INSTEADOF trait_reference_list'''
    # assert False

# trait_reference_list:
#     fully_qualified_class_name                  { zend_resolve_class_name(&$1 TSRMLS_CC); zend_init_list(&$$.u.op.ptr, Z_STRVAL($1.u.constant) TSRMLS_CC); }
#   |  trait_reference_list ',' fully_qualified_class_name      { zend_resolve_class_name(&$3 TSRMLS_CC); zend_add_to_list(&$1.u.op.ptr, Z_STRVAL($3.u.constant) TSRMLS_CC); $$ = $1; }
# ;
def p_trait_reference_list(p):
    '''trait_reference_list : fully_qualified_class_name
                            |  trait_reference_list COMMA fully_qualified_class_name'''

# trait_method_reference:
#     T_STRING                          { zend_prepare_reference(&$$, NULL, &$1 TSRMLS_CC); }
#   |  trait_method_reference_fully_qualified            { $$ = $1; }
# ;
def p_trait_method_reference(p):
    '''trait_method_reference : STRING
                              | trait_method_reference_fully_qualified'''

# trait_method_reference_fully_qualified:
#   fully_qualified_class_name T_DOUBLE_COLON T_STRING    { zend_prepare_reference(&$$, &$1, &$3 TSRMLS_CC); }
# ;
def p_trait_method_reference_fully_qualified(p):
    '''trait_method_reference_fully_qualified : fully_qualified_class_name DOUBLE_COLON STRING'''


# trait_alias:
#     trait_method_reference T_AS trait_modifiers T_STRING    { zend_add_trait_alias(&$1, &$3, &$4 TSRMLS_CC); }
#   |  trait_method_reference T_AS member_modifier          { zend_add_trait_alias(&$1, &$3, NULL TSRMLS_CC); }
# ;
def p_trait_alias(p):
    '''trait_alias : trait_method_reference AS trait_modifiers STRING
                   | trait_method_reference AS member_modifier'''

# trait_modifiers:
#     /* empty */          { Z_LVAL($$.u.constant) = 0x0; } /* No change of methods visibility */
#   |  member_modifier  { $$ = $1; } /* REM: Keep in mind, there are not only visibility modifiers */
# ;
def p_trait_modifiers(p):
    '''trait_modifiers : empty
                       | member_modifier'''

# method_body:
#     ';' /* abstract method */    { Z_LVAL($$.u.constant) = ZEND_ACC_ABSTRACT; }
#   |  '{' inner_statement_list '}'  { Z_LVAL($$.u.constant) = 0;  }
# ;
def p_method_body(p):
    '''method_body : LBRACE inner_statement_list RBRACE
                   | SEMI'''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = []


# variable_modifiers:
#     non_empty_member_modifiers    { $$ = $1; }
#   |  T_VAR              { Z_LVAL($$.u.constant) = ZEND_ACC_PUBLIC; }
# ;
def p_variable_modifiers_non_empty(p):
    'variable_modifiers : non_empty_member_modifiers'
    p[0] = p[1]

def p_variable_modifiers_var(p):
    'variable_modifiers : VAR'
    p[0] = []


# method_modifiers:
#     /* empty */              { Z_LVAL($$.u.constant) = ZEND_ACC_PUBLIC; }
#   |  non_empty_member_modifiers      { $$ = $1;  if (!(Z_LVAL($$.u.constant) & ZEND_ACC_PPP_MASK)) { Z_LVAL($$.u.constant) |= ZEND_ACC_PUBLIC; } }
# ;
def p_method_modifiers_non_empty(p):
    'method_modifiers : non_empty_member_modifiers'
    p[0] = p[1]

def p_method_modifiers_empty(p):
    'method_modifiers : empty'
    p[0] = []


# non_empty_member_modifiers:
#     member_modifier            { $$ = $1; }
#   |  non_empty_member_modifiers member_modifier  { Z_LVAL($$.u.constant) = zend_do_verify_access_types(&$1, &$2); }
# ;
def p_non_empty_member_modifiers(p):
    '''non_empty_member_modifiers : non_empty_member_modifiers member_modifier
                                  | member_modifier'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_member_modifier(p):
    '''member_modifier : PUBLIC
                       | PROTECTED
                       | PRIVATE
                       | STATIC
                       | ABSTRACT
                       | FINAL'''
    p[0] = p[1].lower()

# class_variable_declaration:
#     class_variable_declaration ',' T_VARIABLE          { zend_do_declare_property(&$3, NULL, CG(access_type) TSRMLS_CC); }
#   |  class_variable_declaration ',' T_VARIABLE '=' static_scalar  { zend_do_declare_property(&$3, &$5, CG(access_type) TSRMLS_CC); }
#   |  T_VARIABLE            { zend_do_declare_property(&$1, NULL, CG(access_type) TSRMLS_CC); }
#   |  T_VARIABLE '=' static_scalar  { zend_do_declare_property(&$1, &$3, CG(access_type) TSRMLS_CC); }
# ;
def p_class_variable_declaration_initial(p):
    '''class_variable_declaration : class_variable_declaration COMMA VARIABLE EQUALS static_scalar
                                  | VARIABLE EQUALS static_scalar'''
    if len(p) == 6:
        p[0] = p[1] + [ast.ClassVariable(p[3], p[5], lineno=p.lineno(2))]
    else:
        p[0] = [ast.ClassVariable(p[1], p[3], lineno=p.lineno(1))]

def p_class_variable_declaration_no_initial(p):
    '''class_variable_declaration : class_variable_declaration COMMA VARIABLE
                                  | VARIABLE'''
    if len(p) == 4:
        p[0] = p[1] + [ast.ClassVariable(p[3], None, lineno=p.lineno(2))]
    else:
        p[0] = [ast.ClassVariable(p[1], None, lineno=p.lineno(1))]

# class_constant_declaration:
#     class_constant_declaration ',' T_STRING '=' static_scalar  { zend_do_declare_class_constant(&$3, &$5 TSRMLS_CC); }
#   |  T_CONST T_STRING '=' static_scalar  { zend_do_declare_class_constant(&$2, &$4 TSRMLS_CC); }
# ;
def p_class_constant_declaration(p):
    '''class_constant_declaration : class_constant_declaration COMMA STRING EQUALS static_scalar
                                  | CONST STRING EQUALS static_scalar'''
    if len(p) == 6:
        p[0] = p[1] + [ast.ClassConstant(p[3], p[5], lineno=p.lineno(2))]
    else:
        p[0] = [ast.ClassConstant(p[2], p[4], lineno=p.lineno(1))]


# echo_expr_list:
#     echo_expr_list ',' expr { zend_do_echo(&$3 TSRMLS_CC); }
#   |  expr          { zend_do_echo(&$1 TSRMLS_CC); }
# ;
def p_echo_expr_list(p):
    '''echo_expr_list : echo_expr_list COMMA expr
                      | expr'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# for_expr:
#     /* empty */      { $$.op_type = IS_CONST;  Z_TYPE($$.u.constant) = IS_BOOL;  Z_LVAL($$.u.constant) = 1; }
#   |  non_empty_for_expr  { $$ = $1; }
# ;
def p_for_expr(p):
    '''for_expr : empty
                | non_empty_for_expr'''
    p[0] = p[1]

# non_empty_for_expr:
#     non_empty_for_expr ','  { zend_do_free(&$1 TSRMLS_CC); } expr { $$ = $4; }
#   |  expr          { $$ = $1; }
# ;
def p_non_empty_for_expr(p):
    '''non_empty_for_expr : non_empty_for_expr COMMA expr
                          | expr'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# chaining_method_or_property:
#     chaining_method_or_property variable_property   { $$.EA = $2.EA; }
#   |  variable_property                 { $$.EA = $1.EA; }
# ;
def p_chaining_method_or_property(p):
    '''chaining_method_or_property : chaining_method_or_property variable_property
                                   |  variable_property'''

# chaining_dereference:
#     chaining_dereference '[' dim_offset ']'  { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   |  '[' dim_offset ']'    { zend_do_pop_object(&$1 TSRMLS_CC); fetch_array_dim(&$$, &$1, &$2 TSRMLS_CC); }
# ;
def p_chaining_dereference(p):
    '''chaining_dereference : chaining_dereference LBRACKET dim_offset RBRACKET
                            | LBRACKET dim_offset RBRACKET'''
    pass

# chaining_instance_call:
#     chaining_dereference     { zend_do_push_object(&$1 TSRMLS_CC); } chaining_method_or_property { $$ = $3; }
#   |  chaining_dereference     { zend_do_push_object(&$1 TSRMLS_CC); $$ = $1; }
#   |  chaining_method_or_property { $$ = $1; }
# ;
def p_chaining_instance_call(p):
    '''chaining_instance_call : chaining_dereference chaining_method_or_property
                              | chaining_dereference
                              | chaining_method_or_property'''
    pass

# TODO:
#   Seems more sensible for this to be after 'expr'... ?  I think it's a hack so
#   they can have (new Pants)->something;
#
# instance_call:
#     /* empty */     { $$ = $0; }
#   |  { zend_do_push_object(&$0 TSRMLS_CC); zend_do_begin_variable_parse(TSRMLS_C); }
#     chaining_instance_call  { zend_do_pop_object(&$$ TSRMLS_CC); zend_do_end_variable_parse(&$2, BP_VAR_R, 0 TSRMLS_CC); }
# ;
def p_instance_call(p):
    '''instance_call : empty
                     | chaining_instance_call'''

def p_new_expr(p):
    '''new_expr : NEW class_name_reference ctor_arguments'''
    p[0] = ast.New(p[2], p[3], lineno=p.lineno(1))

def p_expr_without_variable(p):
    '''expr_without_variable : LIST LPAREN assignment_list RPAREN EQUALS expr
                             | expr QUESTION expr COLON expr
                             | expr QUESTION COLON expr
                             | BACKTICK backticks_expr BACKTICK
                             | YIELD
                             | function is_reference LPAREN parameter_list RPAREN lexical_vars LBRACE inner_statement_list RBRACE
                             | STATIC function is_reference LPAREN parameter_list RPAREN lexical_vars LBRACE inner_statement_list RBRACE
                             '''
def p_expr_without_variable_conflictey_brackets(p):
    '''expr_without_variable : parenthesis_expr
                             | LPAREN new_expr RPAREN instance_call
                             | new_expr
                             '''
    # Undocumented shift/reduce here is present in php's grammar as well.  We
    # can solve it py putting instance_call after parenthesis_expr but I think
    # that would by very broad.

def p_expr_without_variable_identity(p):
    '''expr_without_variable : scalar
                             | combined_scalar_offset
                             | combined_scalar
                             | internal_functions_in_yacc
                             '''
    p[0] = p[1]

def p_expr_without_variable_assignment(p):
    '''expr_without_variable : variable EQUALS expr
                             | variable PLUS_EQUAL expr
                             | variable MINUS_EQUAL expr
                             | variable MUL_EQUAL expr
                             | variable POW_EQUAL expr
                             | variable DIV_EQUAL expr
                             | variable CONCAT_EQUAL expr
                             | variable MOD_EQUAL expr
                             | variable AND_EQUAL expr
                             | variable OR_EQUAL expr
                             | variable XOR_EQUAL expr
                             | variable SL_EQUAL expr
                             | variable SR_EQUAL expr'''
    p[0] = ast.AssignOp(p[1], p[2], p[3])

def p_expr_without_variable_binary(p):
    '''expr_without_variable : expr IS_IDENTICAL expr
                             | expr IS_NOT_IDENTICAL expr
                             | expr IS_EQUAL expr
                             | expr IS_NOT_EQUAL expr
                             | expr IS_SMALLER expr
                             | expr IS_SMALLER_OR_EQUAL expr
                             | expr IS_GREATER expr
                             | expr IS_GREATER_OR_EQUAL expr
                             | expr INSTANCEOF class_name_reference
                             | expr BOOLEAN_OR expr
                             | expr BOOLEAN_AND expr
                             | expr LOGICAL_OR expr
                             | expr LOGICAL_AND expr
                             | expr LOGICAL_XOR expr
                             | expr OR expr
                             | expr AND expr
                             | expr XOR expr
                             | expr CONCAT expr
                             | expr PLUS expr
                             | expr MINUS expr
                             | expr MUL expr
                             | expr POW expr
                             | expr DIV expr
                             | expr MOD expr
                             | expr SL expr
                             | expr SR expr
                             '''
    p[0] = ast.BinaryOp(p[2].lower(), p[1], p[3], lineno=p.lineno(2))

def p_expr_without_variable_unary(p):
    '''expr_without_variable : CLONE expr
                             | INC rw_variable
                             | DEC rw_variable
                             | PLUS expr %prec INC
                             | MINUS expr %prec INC
                             | BOOLEAN_NOT expr
                             | NOT expr
                             | INT_CAST expr
                             | DOUBLE_CAST expr
                             | STRING_CAST expr
                             | ARRAY_CAST expr
                             | OBJECT_CAST expr
                             | BOOL_CAST expr
                             | UNSET_CAST expr
                             | EXIT exit_expr
                             | AT expr
                             | PRINT expr
                             '''

def p_expr_without_variable_post_unary(p):
    '''expr_without_variable : rw_variable INC
                             | rw_variable DEC'''

def p_expr_without_variable_reference_assignment(p):
    '''expr_without_variable : variable EQUALS AND variable'''
    p[0] = ast.Assignment(p[1], p[4], is_ref=True, lineno=p.lineno(1))

def p_expr_without_variable_reference_new_assignment(p):
    '''expr_without_variable : variable EQUALS AND NEW class_name_reference ctor_arguments'''
    new = ast.New(p[5], p[6], lineno=p.lineno(1))
    p[0] = ast.Assignment(p[1], new, is_ref=True, lineno=p.lineno(1))

# yield_expr:
#     T_YIELD expr_without_variable { zend_do_yield(&$$, &$2, NULL, 0 TSRMLS_CC); }
#   |  T_YIELD variable { zend_do_yield(&$$, &$2, NULL, 1 TSRMLS_CC); }
#   |  T_YIELD expr T_DOUBLE_ARROW expr_without_variable { zend_do_yield(&$$, &$4, &$2, 0 TSRMLS_CC); }
#   |  T_YIELD expr T_DOUBLE_ARROW variable { zend_do_yield(&$$, &$4, &$2, 1 TSRMLS_CC); }
# ;
def p_yield_expr(p):
    '''yield_expr : YIELD expr_without_variable
                  | YIELD variable
                  | YIELD expr DOUBLE_ARROW expr_without_variable
                  | YIELD expr DOUBLE_ARROW variable'''

# combined_scalar_offset:
#     combined_scalar '[' dim_offset ']' { zend_do_begin_variable_parse(TSRMLS_C); fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   | combined_scalar_offset '[' dim_offset ']' { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#     | T_CONSTANT_ENCAPSED_STRING '[' dim_offset ']' { $1.EA = 0; zend_do_begin_variable_parse(TSRMLS_C); fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
def p_combined_scalar_offset(p):
    '''combined_scalar_offset : combined_scalar LBRACKET dim_offset RBRACKET
                              | combined_scalar_offset LBRACKET dim_offset RBRACKET
                              | CONSTANT_ENCAPSED_STRING LBRACKET dim_offset RBRACKET'''

# combined_scalar:
#       T_ARRAY '(' array_pair_list ')' { $$ = $3; }
#     | '[' array_pair_list ']' { $$ = $2; }
def p_combined_scalar(p):
    '''combined_scalar : ARRAY LPAREN array_pair_list RPAREN
                       | LBRACKET array_pair_list RBRACKET'''

# function:
#   T_FUNCTION { $$.u.op.opline_num = CG(zend_lineno); }
# ;
def p_function(p):
    'function : FUNCTION'

# lexical_vars:
#     /* empty */
#   |  T_USE '(' lexical_var_list ')'
# ;
def p_lexical_vars(p):
    '''lexical_vars : USE LPAREN lexical_var_list RPAREN
                    | empty'''
    if len(p) == 5:
        p[0] = p[3]
    else:
        p[0] = []

# lexical_var_list:
#     lexical_var_list ',' T_VARIABLE      { zend_do_fetch_lexical_variable(&$3, 0 TSRMLS_CC); }
#   |  lexical_var_list ',' '&' T_VARIABLE    { zend_do_fetch_lexical_variable(&$4, 1 TSRMLS_CC); }
#   |  T_VARIABLE                { zend_do_fetch_lexical_variable(&$1, 0 TSRMLS_CC); }
#   |  '&' T_VARIABLE              { zend_do_fetch_lexical_variable(&$2, 1 TSRMLS_CC); }
# ;
def p_lexical_var_list(p):
    '''lexical_var_list : lexical_var_list COMMA AND VARIABLE
                        | lexical_var_list COMMA VARIABLE
                        | AND VARIABLE
                        | VARIABLE'''
    if len(p) == 5:
        p[0] = p[1] + [ast.LexicalVariable(p[4], True, lineno=p.lineno(2))]
    elif len(p) == 4:
        p[0] = p[1] + [ast.LexicalVariable(p[3], False, lineno=p.lineno(2))]
    elif len(p) == 3:
        p[0] = [ast.LexicalVariable(p[2], True, lineno=p.lineno(1))]
    else:
        p[0] = [ast.LexicalVariable(p[1], False, lineno=p.lineno(1))]

# function_call:
#     namespace_name function_call_parameter_list
#   |  T_NAMESPACE T_NS_SEPARATOR namespace_name function_call_parameter_list
#   |  T_NS_SEPARATOR namespace_name function_call_parameter_list
#   |  class_name T_DOUBLE_COLON variable_name function_call_parameter_list
#   |  class_name T_DOUBLE_COLON variable_without_objects function_call_parameter_list
#   |  variable_class_name T_DOUBLE_COLON variable_name function_call_parameter_list
#   |  variable_class_name T_DOUBLE_COLON variable_without_objects function_call_parameter_list
#   |  variable_without_objects function_call_parameter_list
# ;
#
# TODO:
#   Pretty sure this is all correct but meh.
def p_function_call(p):
    '''function_call : namespace_name function_call_parameter_list
                     | NS_SEPARATOR namespace_name function_call_parameter_list
                     | NAMESPACE NS_SEPARATOR namespace_name function_call_parameter_list'''
    if len(p) == 5:
        p[0] = ast.FunctionCall(p[1], p[3], lineno=p.lineno(2))
    elif len(p) == 6:
        p[0] = ast.FunctionCall(p[1] + p[2], p[4], lineno=p.lineno(1))
    else:
        # print [p[i] for i in (1, 2, 3,)]
        # p[0] = ast.FunctionCall(p[1] + p[2] + p[3], p[4], lineno=p.lineno(1))
        pass

def p_function_call_static(p):
    '''function_call : class_name DOUBLE_COLON STRING function_call_parameter_list
                     | class_name DOUBLE_COLON variable_without_objects function_call_parameter_list
                     | variable_class_name DOUBLE_COLON STRING function_call_parameter_list
                     | variable_class_name DOUBLE_COLON variable_without_objects function_call_parameter_list'''
    p[0] = ast.StaticMethodCall(p[1], p[3], p[4], lineno=p.lineno(2))

def p_function_call_variable(p):
    'function_call : variable_without_objects function_call_parameter_list'
    p[0] = ast.FunctionCall(p[1], p[3], lineno=p.lineno(2))

# class_name:
#     T_STATIC { $$.op_type = IS_CONST; ZVAL_STRINGL(&$$.u.constant, "static", sizeof("static")-1, 1);}
#   |  namespace_name { $$ = $1; }
#   |  T_NAMESPACE T_NS_SEPARATOR namespace_name { $$.op_type = IS_CONST; ZVAL_EMPTY_STRING(&$$.u.constant);  zend_do_build_namespace_name(&$$, &$$, &$3 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name { char *tmp = estrndup(Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); memcpy(&(tmp[1]), Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); tmp[0] = '\\'; efree(Z_STRVAL($2.u.constant)); Z_STRVAL($2.u.constant) = tmp; ++Z_STRLEN($2.u.constant); $$ = $2; }
# ;
def p_class_name(p):
    '''class_name : namespace_name
                  | NS_SEPARATOR namespace_name
                  | NAMESPACE NS_SEPARATOR namespace_name'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1] + p[2] + p[3]

def p_class_name_static(p):
    'class_name : STATIC'
    p[0] = p[1].lower()

# fully_qualified_class_name:
#     namespace_name { $$ = $1; }
#   |  T_NAMESPACE T_NS_SEPARATOR namespace_name { $$.op_type = IS_CONST; ZVAL_EMPTY_STRING(&$$.u.constant);  zend_do_build_namespace_name(&$$, &$$, &$3 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name { char *tmp = estrndup(Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); memcpy(&(tmp[1]), Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); tmp[0] = '\\'; efree(Z_STRVAL($2.u.constant)); Z_STRVAL($2.u.constant) = tmp; ++Z_STRLEN($2.u.constant); $$ = $2; }
# ;
def p_fully_qualified_class_name(p):
    '''fully_qualified_class_name : namespace_name
                                  | NS_SEPARATOR namespace_name
                                  | NAMESPACE NS_SEPARATOR namespace_name'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1] + p[2] + p[3]

# class_name_reference:
#     class_name            { zend_do_fetch_class(&$$, &$1 TSRMLS_CC); }
#   |  dynamic_class_name_reference  { zend_do_end_variable_parse(&$1, BP_VAR_R, 0 TSRMLS_CC); zend_do_fetch_class(&$$, &$1 TSRMLS_CC); }
# ;
def p_class_name_reference(p):
    '''class_name_reference : class_name
                            | dynamic_class_name_reference'''
    p[0] = p[1]

# dynamic_class_name_reference:
#     base_variable T_OBJECT_OPERATOR { zend_do_push_object(&$1 TSRMLS_CC); }
#       object_property { zend_do_push_object(&$4 TSRMLS_CC); } dynamic_class_name_variable_properties
#       { zend_do_pop_object(&$$ TSRMLS_CC); $$.EA = ZEND_PARSED_MEMBER; }
#   |  base_variable { $$ = $1; }
# ;
def p_dynamic_class_name_reference(p):
    '''dynamic_class_name_reference : base_variable OBJECT_OPERATOR object_property dynamic_class_name_variable_properties
                                    | base_variable'''
    if len(p) == 5:
        name, dims = p[3]
        p[0] = ast.ObjectProperty(p[1], name, lineno=p.lineno(2))
        for class_, dim, lineno in dims:
            p[0] = class_(p[0], dim, lineno=lineno)
        for name, dims in p[4]:
            p[0] = ast.ObjectProperty(p[0], name, lineno=p.lineno(2))
            for class_, dim, lineno in dims:
                p[0] = class_(p[0], dim, lineno=lineno)
    else:
        p[0] = p[1]

# dynamic_class_name_variable_properties:
#     dynamic_class_name_variable_properties dynamic_class_name_variable_property
#   |  /* empty */
# ;
def p_dynamic_class_name_variable_properties(p):
    '''dynamic_class_name_variable_properties : dynamic_class_name_variable_properties dynamic_class_name_variable_property
                                              | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

# dynamic_class_name_variable_property:
#     T_OBJECT_OPERATOR object_property { zend_do_push_object(&$2 TSRMLS_CC); }
# ;
def p_dynamic_class_name_variable_property(p):
    'dynamic_class_name_variable_property : OBJECT_OPERATOR object_property'
    p[0] = p[2]

# exit_expr:
#     /* empty */  { memset(&$$, 0, sizeof(znode)); $$.op_type = IS_UNUSED; }
#   |  '(' ')'    { memset(&$$, 0, sizeof(znode)); $$.op_type = IS_UNUSED; }
#   |  parenthesis_expr  { $$ = $1; }
# ;
def p_exit_expr(p):
    '''exit_expr : empty
                 | LPAREN RPAREN
                 | parenthesis_expr'''

# backticks_expr:
#     /* empty */  { ZVAL_EMPTY_STRING(&$$.u.constant); INIT_PZVAL(&$$.u.constant); $$.op_type = IS_CONST; }
#   |  T_ENCAPSED_AND_WHITESPACE  { $$ = $1; }
#   |  encaps_list  { $$ = $1; }
# ;
def p_backticks_expr(p):
    '''backticks_expr : empty
                      | ENCAPSED_AND_WHITESPACE
                      | encaps_list'''

# ctor_arguments:
#     /* empty */  { Z_LVAL($$.u.constant) = 0; }
#   |  function_call_parameter_list   { $$ = $1; }
# ;
def p_ctor_arguments(p):
    '''ctor_arguments : function_call_parameter_list
                      | empty'''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = []

# common_scalar:
#     T_LNUMBER           { $$ = $1; }
#   |  T_DNUMBER           { $$ = $1; }
#   |  T_CONSTANT_ENCAPSED_STRING  { $$ = $1; }
#   |  T_LINE             { $$ = $1; }
#   |  T_FILE             { $$ = $1; }
#   |  T_DIR             { $$ = $1; }
#   |  T_TRAIT_C          { $$ = $1; }
#   |  T_METHOD_C          { $$ = $1; }
#   |  T_FUNC_C          { $$ = $1; }
#   |  T_NS_C            { $$ = $1; }
#   |  T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC { $$ = $2; }
#   |  T_START_HEREDOC T_END_HEREDOC { ZVAL_EMPTY_STRING(&$$.u.constant); INIT_PZVAL(&$$.u.constant); $$.op_type = IS_CONST; }
# ;
def p_common_scalar_lnumber(p):
    'common_scalar : LNUMBER'
    if p[1].startswith('0x'):
        p[0] = int(p[1], 16)
    elif p[1].startswith('0'):
        p[0] = int(p[1], 8)
    else:
        p[0] = int(p[1])

def p_common_scalar_dnumber(p):
    'common_scalar : DNUMBER'
    p[0] = float(p[1])

def p_common_scalar_string(p):
    'common_scalar : CONSTANT_ENCAPSED_STRING'
    p[0] = p[1][1:-1].replace("\\'", "'").replace('\\\\', '\\')

def p_common_scalar_magic_line(p):
    'common_scalar : LINE'
    p[0] = ast.MagicConstant(p[1].upper(), p.lineno(1), lineno=p.lineno(1))

def p_common_scalar_magic_file(p):
    'common_scalar : FILE'
    value = getattr(p.lexer, 'filename', None)
    p[0] = ast.MagicConstant(p[1].upper(), value, lineno=p.lineno(1))

def p_common_scalar_magic_dir(p):
    'common_scalar : DIR'
    value = getattr(p.lexer, 'filename', None)
    if value is not None:
        value = os.path.dirname(value)
    p[0] = ast.MagicConstant(p[1].upper(), value, lineno=p.lineno(1))

def p_common_scalar_magic_trait(p):
    'common_scalar : TRAIT_C'
    p[0] = ast.MagicConstant(p[1].upper(), None, lineno=p.lineno(1))

def p_common_scalar_magic_method(p):
    'common_scalar : METHOD_C'
    p[0] = ast.MagicConstant(p[1].upper(), None, lineno=p.lineno(1))

def p_common_scalar_magic_func(p):
    'common_scalar : FUNC_C'
    p[0] = ast.MagicConstant(p[1].upper(), None, lineno=p.lineno(1))

def p_common_scalar_magic_ns(p):
    'common_scalar : NS_C'
    p[0] = ast.MagicConstant(p[1].upper(), None, lineno=p.lineno(1))

def p_common_heredoc(p):
    '''common_scalar : START_HEREDOC ENCAPSED_AND_WHITESPACE END_HEREDOC
                     | START_HEREDOC END_HEREDOC'''

# static_class_constant:
#     class_name T_DOUBLE_COLON T_STRING { zend_do_fetch_constant(&$$, &$1, &$3, ZEND_CT, 0 TSRMLS_CC); }
# ;
def p_static_class_constant(p):
    '''static_class_constant : class_name DOUBLE_COLON STRING'''

# static_scalar: /* compile-time evaluated scalars */
#     static_scalar_value { zend_do_constant_expression(&$$, $1.u.ast TSRMLS_CC); }
#   |  T_ARRAY '(' static_array_pair_list ')' { $$ = $3; Z_TYPE($$.u.constant) = IS_CONSTANT_ARRAY; }
#   |  '[' static_array_pair_list ']' { $$ = $2; Z_TYPE($$.u.constant) = IS_CONSTANT_ARRAY; }
# ;
def p_static_scalar(p):
    '''static_scalar : static_scalar_value
                     | ARRAY LPAREN static_array_pair_list RPAREN
                     | LBRACKET static_array_pair_list RBRACKET'''
    # TODO:
    #   Static scalar is done a bit better in phply.  Probably we don't need to
    #   do most of this because we don't actually want to evaluate anything at
    #   compile time...

# static_scalar_value:
#     common_scalar  { $$.u.ast = zend_ast_create_constant(&$1.u.constant); }
#   |  static_class_name_scalar  { $$.u.ast = zend_ast_create_constant(&$1.u.constant); }
#   |  namespace_name     { zend_do_fetch_constant(&$$, NULL, &$1, ZEND_CT, 1 TSRMLS_CC); $$.u.ast = zend_ast_create_constant(&$$.u.constant); }
#   |  T_NAMESPACE T_NS_SEPARATOR namespace_name { $$.op_type = IS_CONST; ZVAL_EMPTY_STRING(&$$.u.constant);  zend_do_build_namespace_name(&$$, &$$, &$3 TSRMLS_CC); $3 = $$; zend_do_fetch_constant(&$$, NULL, &$3, ZEND_CT, 0 TSRMLS_CC); $$.u.ast = zend_ast_create_constant(&$$.u.constant); }
#   |  T_NS_SEPARATOR namespace_name { char *tmp = estrndup(Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); memcpy(&(tmp[1]), Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); tmp[0] = '\\'; efree(Z_STRVAL($2.u.constant)); Z_STRVAL($2.u.constant) = tmp; ++Z_STRLEN($2.u.constant); zend_do_fetch_constant(&$$, NULL, &$2, ZEND_CT, 0 TSRMLS_CC); $$.u.ast = zend_ast_create_constant(&$$.u.constant); }
#   |  static_class_constant { $$.u.ast = zend_ast_create_constant(&$1.u.constant); }
#   |  T_CLASS_C      { $$.u.ast = zend_ast_create_constant(&$1.u.constant); }
#   |  static_operation { $$ = $1; }
# ;
def p_static_scalar_value(p):
    '''static_scalar_value : common_scalar
                           | static_class_name_scalar
                           | namespace_name
                           | NAMESPACE NS_SEPARATOR namespace_name
                           | NS_SEPARATOR namespace_name
                           | static_class_constant
                           | CLASS_C
                           | static_operation'''

# static_operation:
#     static_scalar_value '+' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_ADD, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '-' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_SUB, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '*' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_MUL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '/' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_DIV, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '%' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_MOD, $1.u.ast, $3.u.ast); }
#   |  '!' static_scalar_value { $$.u.ast = zend_ast_create_unary(ZEND_BOOL_NOT, $2.u.ast); }
#   |  '~' static_scalar_value { $$.u.ast = zend_ast_create_unary(ZEND_BW_NOT, $2.u.ast); }
#   |  static_scalar_value '|' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BW_OR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '&' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BW_AND, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '^' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BW_XOR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_SL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_SL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_SR static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_SR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '.' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_CONCAT, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_LOGICAL_XOR static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BOOL_XOR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_LOGICAL_AND static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BOOL_AND, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_LOGICAL_OR static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BOOL_OR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_BOOLEAN_AND static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BOOL_AND, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_BOOLEAN_OR static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_BOOL_OR, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_IS_IDENTICAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_IDENTICAL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_IS_NOT_IDENTICAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_NOT_IDENTICAL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_IS_EQUAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_EQUAL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_IS_NOT_EQUAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_NOT_EQUAL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '<' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_SMALLER, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value '>' static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_SMALLER, $3.u.ast, $1.u.ast); }
#   |  static_scalar_value T_IS_SMALLER_OR_EQUAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_SMALLER_OR_EQUAL, $1.u.ast, $3.u.ast); }
#   |  static_scalar_value T_IS_GREATER_OR_EQUAL static_scalar_value { $$.u.ast = zend_ast_create_binary(ZEND_IS_SMALLER_OR_EQUAL, $3.u.ast, $1.u.ast); }
#   |  static_scalar_value '?' ':' static_scalar_value { $$.u.ast = zend_ast_create_ternary(ZEND_SELECT, $1.u.ast, NULL, $4.u.ast); }
#   |  static_scalar_value '?' static_scalar_value ':' static_scalar_value { $$.u.ast = zend_ast_create_ternary(ZEND_SELECT, $1.u.ast, $3.u.ast, $5.u.ast); }
#   |  '+' static_scalar_value { $$.u.ast = zend_ast_create_unary(ZEND_UNARY_PLUS, $2.u.ast); }
#   |  '-' static_scalar_value { $$.u.ast = zend_ast_create_unary(ZEND_UNARY_MINUS, $2.u.ast); }
#   |  '(' static_scalar_value ')' { $$ = $2; }
# ;
def p_static_operation(p):
    '''static_operation : static_scalar_value PLUS static_scalar_value
                        | static_scalar_value MINUS static_scalar_value
                        | static_scalar_value MUL static_scalar_value
                        | static_scalar_value DIV static_scalar_value
                        | static_scalar_value MOD static_scalar_value
                        | BOOLEAN_NOT static_scalar_value
                        | NOT static_scalar_value
                        | static_scalar_value OR static_scalar_value
                        | static_scalar_value AND static_scalar_value
                        | static_scalar_value XOR static_scalar_value
                        | static_scalar_value SL static_scalar_value
                        | static_scalar_value SR static_scalar_value
                        | static_scalar_value CONCAT static_scalar_value
                        | static_scalar_value LOGICAL_XOR static_scalar_value
                        | static_scalar_value LOGICAL_AND static_scalar_value
                        | static_scalar_value LOGICAL_OR static_scalar_value
                        | static_scalar_value BOOLEAN_AND static_scalar_value
                        | static_scalar_value BOOLEAN_OR static_scalar_value
                        | static_scalar_value IS_IDENTICAL static_scalar_value
                        | static_scalar_value IS_NOT_IDENTICAL static_scalar_value
                        | static_scalar_value IS_EQUAL static_scalar_value
                        | static_scalar_value IS_NOT_EQUAL static_scalar_value
                        | static_scalar_value IS_SMALLER static_scalar_value
                        | static_scalar_value IS_GREATER static_scalar_value
                        | static_scalar_value IS_SMALLER_OR_EQUAL static_scalar_value
                        | static_scalar_value IS_GREATER_OR_EQUAL static_scalar_value
                        | static_scalar_value QUESTION COLON static_scalar_value
                        | static_scalar_value QUESTION static_scalar_value COLON static_scalar_value
                        | PLUS static_scalar_value
                        | MINUS static_scalar_value
                        | LPAREN static_scalar_value RPAREN
                        '''

# scalar:
#     T_STRING_VARNAME    { $$ = $1; }
#   |  class_name_scalar  { $$ = $1; }
#   |  class_constant    { $$ = $1; }
#   |  namespace_name  { zend_do_fetch_constant(&$$, NULL, &$1, ZEND_RT, 1 TSRMLS_CC); }
#   |  T_NAMESPACE T_NS_SEPARATOR namespace_name { $$.op_type = IS_CONST; ZVAL_EMPTY_STRING(&$$.u.constant);  zend_do_build_namespace_name(&$$, &$$, &$3 TSRMLS_CC); $3 = $$; zend_do_fetch_constant(&$$, NULL, &$3, ZEND_RT, 0 TSRMLS_CC); }
#   |  T_NS_SEPARATOR namespace_name { char *tmp = estrndup(Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); memcpy(&(tmp[1]), Z_STRVAL($2.u.constant), Z_STRLEN($2.u.constant)+1); tmp[0] = '\\'; efree(Z_STRVAL($2.u.constant)); Z_STRVAL($2.u.constant) = tmp; ++Z_STRLEN($2.u.constant); zend_do_fetch_constant(&$$, NULL, &$2, ZEND_RT, 0 TSRMLS_CC); }
#   |  common_scalar      { $$ = $1; }
#   |  '"' encaps_list '"'   { $$ = $2; }
#   |  T_START_HEREDOC encaps_list T_END_HEREDOC { $$ = $2; }
#   |  T_CLASS_C        { if (Z_TYPE($1.u.constant) == IS_CONSTANT) {zend_do_fetch_constant(&$$, NULL, &$1, ZEND_RT, 1 TSRMLS_CC);} else {$$ = $1;} }
# ;
def p_scalar_identity(p):
    '''scalar : STRING_VARNAME
              | class_name_scalar
              | class_constant
              | namespace_name
              | common_scalar
              '''
    p[0] = p[1]

def p_scalar_magic(p):
    '''scalar : CLASS_C'''
    # Bit unclear why this isn't in common scalar...
    p[0] = ast.MagicConstant(p[1].upper(), None, lineno=p.lineno(1))

def p_scalar_other(p):
    '''scalar : NAMESPACE NS_SEPARATOR namespace_name
              | NS_SEPARATOR namespace_name
              | QUOTE encaps_list QUOTE
              | START_HEREDOC encaps_list END_HEREDOC
              '''

def p_scalar_double_quote_string(p):
    'scalar : QUOTE ENCAPSED_AND_WHITESPACE QUOTE'
    # This is my own invention.  I suspect the php lexer does some magic such
    # that pure strings (like "a" as opposed to "$blah") return exactly as their
    # single-quote equivilent.  The php grammar therefore doesn't have an option
    # to reduce encaps_list like that.  (If you attempt to do it there you get
    # problems with backtick expressions.  Phply doesn't support backticks so
    # that's probably why it doesn't solve this problem.
    p[0] = p[2]

# static_array_pair_list:
#     /* empty */ { $$.op_type = IS_CONST; INIT_PZVAL(&$$.u.constant); array_init(&$$.u.constant); }
#   |  non_empty_static_array_pair_list possible_comma  { $$ = $1; }
# ;
def p_static_array_pair_list(p):
    '''static_array_pair_list : empty
                              | non_empty_static_array_pair_list possible_comma'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1]


# possible_comma:
#     /* empty */
#   |  ','
# ;
def p_possible_comma(p):
    '''possible_comma : empty
                      | COMMA'''
    pass

# non_empty_static_array_pair_list:
#     non_empty_static_array_pair_list ',' static_scalar T_DOUBLE_ARROW static_scalar  { zend_do_add_static_array_element(&$$, &$3, &$5); }
#   |  non_empty_static_array_pair_list ',' static_scalar { zend_do_add_static_array_element(&$$, NULL, &$3); }
#   |  static_scalar T_DOUBLE_ARROW static_scalar { $$.op_type = IS_CONST; INIT_PZVAL(&$$.u.constant); array_init(&$$.u.constant); zend_do_add_static_array_element(&$$, &$1, &$3); }
#   |  static_scalar { $$.op_type = IS_CONST; INIT_PZVAL(&$$.u.constant); array_init(&$$.u.constant); zend_do_add_static_array_element(&$$, NULL, &$1); }
# ;
def p_non_empty_static_array_pair_list_item(p):
    '''non_empty_static_array_pair_list : non_empty_static_array_pair_list COMMA static_scalar
                                        | static_scalar'''
    if len(p) == 4:
        p[0] = p[1] + [ast.ArrayElement(None, p[3], False, lineno=p.lineno(2))]
    else:
        p[0] = [ast.ArrayElement(None, p[1], False, lineno=p.lineno(1))]

def p_non_empty_static_array_pair_list_pair(p):
    '''non_empty_static_array_pair_list : non_empty_static_array_pair_list COMMA static_scalar DOUBLE_ARROW static_scalar
                                        | static_scalar DOUBLE_ARROW static_scalar'''
    if len(p) == 6:
        p[0] = p[1] + [ast.ArrayElement(p[3], p[5], False, lineno=p.lineno(2))]
    else:
        p[0] = [ast.ArrayElement(p[1], p[3], False, lineno=p.lineno(2))]

# expr:
#     r_variable          { $$ = $1; }
#   |  expr_without_variable    { $$ = $1; }
# ;
def p_expr(p):
    '''expr : r_variable
            | expr_without_variable'''
    p[0] = p[1]

# parenthesis_expr:
#     '(' expr ')'    { $$ = $2; }
#   |  '(' yield_expr ')'  { $$ = $2; }
# ;
def p_parenthesis_expr(p):
    '''parenthesis_expr : LPAREN expr RPAREN
                        | LPAREN yield_expr RPAREN'''

# r_variable:
#   variable { zend_do_end_variable_parse(&$1, BP_VAR_R, 0 TSRMLS_CC); $$ = $1; }
# ;
def p_r_variable(p):
    '''r_variable : variable'''
    p[0] = p[1]

# w_variable:
#   variable  { zend_do_end_variable_parse(&$1, BP_VAR_W, 0 TSRMLS_CC); $$ = $1;
#           zend_check_writable_variable(&$1); }
# ;
def p_w_variable(p):
    '''w_variable : variable'''
    p[0] = p[1]

# rw_variable:
#   variable  { zend_do_end_variable_parse(&$1, BP_VAR_RW, 0 TSRMLS_CC); $$ = $1;
#           zend_check_writable_variable(&$1); }
# ;
def p_rw_variable(p):
    '''rw_variable : variable'''
    p[0] = p[1]

# variable:
#     base_variable_with_function_calls T_OBJECT_OPERATOR { zend_do_push_object(&$1 TSRMLS_CC); }
#       object_property { zend_do_push_object(&$4 TSRMLS_CC); } method_or_not variable_properties
#       { zend_do_pop_object(&$$ TSRMLS_CC); $$.EA = $1.EA | ($7.EA ? $7.EA : $6.EA); }
#   |  base_variable_with_function_calls { $$ = $1; }
# ;
def p_variable(p):
    '''variable : base_variable_with_function_calls OBJECT_OPERATOR object_property method_or_not variable_properties
                | base_variable_with_function_calls'''
    if len(p) == 6:
        return # broken becuase the dependencies don't return the same thing
        name, dims = p[3]
        params = p[4]
        if params is not None:
            p[0] = ast.MethodCall(p[1], name, params, lineno=p.lineno(2))
        else:
            p[0] = ast.ObjectProperty(p[1], name, lineno=p.lineno(2))
        for class_, dim, lineno in dims:
            p[0] = class_(p[0], dim, lineno=lineno)
        for (name, dims), params in p[5]:
            if params is not None:
                p[0] = ast.MethodCall(p[0], name, params, lineno=p.lineno(2))
            else:
                p[0] = ast.ObjectProperty(p[0], name, lineno=p.lineno(2))
            for class_, dim, lineno in dims:
                p[0] = class_(p[0], dim, lineno=lineno)
    else:
        p[0] = p[1]

# variable_properties:
#     variable_properties variable_property { $$.EA = $2.EA; }
#   |  /* empty */ { $$.EA = 0; }
# ;
def p_variable_properties(p):
    '''variable_properties : variable_properties variable_property
                           | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

# variable_property:
#     T_OBJECT_OPERATOR object_property { zend_do_push_object(&$2 TSRMLS_CC); } method_or_not { $$.EA = $4.EA; }
# ;
def p_variable_property(p):
    'variable_property : OBJECT_OPERATOR object_property method_or_not'
    p[0] = (p[2], p[3])

# array_method_dereference:
#     array_method_dereference '[' dim_offset ']' { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   |  method '[' dim_offset ']' { $1.EA = ZEND_PARSED_METHOD_CALL; fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
# ;
def p_array_method_dereference(p):
    '''array_method_dereference : array_method_dereference LBRACKET dim_offset RBRACKET
                                | method LBRACKET dim_offset RBRACKET'''

# method:
#     { zend_do_pop_object(&$$ TSRMLS_CC); zend_do_begin_method_call(&$$ TSRMLS_CC); }
#     function_call_parameter_list { zend_do_end_function_call(&$1, &$$, 1, 1 TSRMLS_CC); zend_do_extended_fcall_end(TSRMLS_C); }
# ;
def p_method(p):
    '''method : function_call_parameter_list'''

# method_or_not:
#     method            { $$ = $1; $$.EA = ZEND_PARSED_METHOD_CALL; zend_do_push_object(&$$ TSRMLS_CC); }
#   |  array_method_dereference  { $$ = $1; zend_do_push_object(&$$ TSRMLS_CC); }
#   |  /* empty */ { $$.EA = ZEND_PARSED_MEMBER; }
# ;
def p_method_or_not(p):
    '''method_or_not : method
                     | array_method_dereference
                     | empty'''


# variable_without_objects:
#     reference_variable { $$ = $1; }
#   |  simple_indirect_reference reference_variable { zend_do_indirect_references(&$$, &$1, &$2 TSRMLS_CC); }
def p_variable_without_objects(p):
    '''variable_without_objects : simple_indirect_reference
                                | reference_variable'''
    p[0] = p[1]

# static_member:
#     class_name T_DOUBLE_COLON variable_without_objects { $$ = $3; zend_do_fetch_static_member(&$$, &$1 TSRMLS_CC); }
#   |  variable_class_name T_DOUBLE_COLON variable_without_objects { $$ = $3; zend_do_fetch_static_member(&$$, &$1 TSRMLS_CC); }
def p_static_member(p):
    '''static_member : class_name DOUBLE_COLON variable_without_objects
                     | variable_class_name DOUBLE_COLON variable_without_objects'''
    p[0] = ast.StaticProperty(p[1], p[3], lineno=p.lineno(2))

# variable_class_name:
#     reference_variable { zend_do_end_variable_parse(&$1, BP_VAR_R, 0 TSRMLS_CC); $$=$1;; }
# ;
def p_variable_class_name(p):
    'variable_class_name : reference_variable'
    p[0] = p[1]

# array_function_dereference:
#     array_function_dereference '[' dim_offset ']' { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   |  function_call { zend_do_begin_variable_parse(TSRMLS_C); $1.EA = ZEND_PARSED_FUNCTION_CALL; }
#     '[' dim_offset ']' { fetch_array_dim(&$$, &$1, &$4 TSRMLS_CC); }
# ;
def p_array_function_dereference(p):
    '''array_function_dereference : array_function_dereference LBRACKET dim_offset RBRACKET
                                  | function_call LBRACKET dim_offset RBRACKET'''

# base_variable_with_function_calls:
#     base_variable        { $$ = $1; }
#   |  array_function_dereference  { $$ = $1; }
#   |  function_call { zend_do_begin_variable_parse(TSRMLS_C); $$ = $1; $$.EA = ZEND_PARSED_FUNCTION_CALL; }
# ;
def p_base_variable_with_function_calls(p):
    '''base_variable_with_function_calls : base_variable
                                         | array_function_dereference
                                         | function_call'''
    p[0] = p[1]

# base_variable:
#     reference_variable { $$ = $1; $$.EA = ZEND_PARSED_VARIABLE; }
#   |  simple_indirect_reference reference_variable { zend_do_indirect_references(&$$, &$1, &$2 TSRMLS_CC); $$.EA = ZEND_PARSED_VARIABLE; }
#   |  static_member { $$ = $1; $$.EA = ZEND_PARSED_STATIC_MEMBER; }
# ;
def p_base_variable(p):
    '''base_variable : reference_variable
                     | simple_indirect_reference reference_variable
                     | static_member'''
    # TODO: wrpng for rule 2
    p[0] = p[1]

# reference_variable:
#     reference_variable '[' dim_offset ']'  { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   |  reference_variable '{' expr '}'    { fetch_string_offset(&$$, &$1, &$3 TSRMLS_CC); }
#   |  compound_variable      { zend_do_begin_variable_parse(TSRMLS_C); fetch_simple_variable(&$$, &$1, 1 TSRMLS_CC); }
# ;
def p_reference_variable_array_offset(p):
    'reference_variable : reference_variable LBRACKET dim_offset RBRACKET'
    p[0] = ast.ArrayOffset(p[1], p[3], lineno=p.lineno(2))

def p_reference_variable_string_offset(p):
    'reference_variable : reference_variable LBRACE expr RBRACE'
    p[0] = ast.StringOffset(p[1], p[3], lineno=p.lineno(2))

def p_reference_variable_compound_variable(p):
    'reference_variable : compound_variable'
    p[0] = p[1]

# compound_variable:
#     T_VARIABLE      { $$ = $1; }
#   |  '$' '{' expr '}'  { $$ = $3; }
# ;
def p_compound_variable(p):
    '''compound_variable : VARIABLE
                         | DOLLAR LBRACE expr RBRACE'''
    if len(p) == 2:
        p[0] = ast.Variable(p[1], lineno=p.lineno(1))
    else:
        p[0] = ast.Variable(p[3], lineno=p.lineno(1))

# dim_offset:
#     /* empty */    { $$.op_type = IS_UNUSED; }
#   |  expr      { $$ = $1; }
# ;
def p_dim_offset(p):
    '''dim_offset : expr
                  | empty'''
    p[0] = p[1]

# object_property:
#     object_dim_list { $$ = $1; }
#   |  variable_without_objects { zend_do_end_variable_parse(&$1, BP_VAR_R, 0 TSRMLS_CC); } { znode tmp_znode;  zend_do_pop_object(&tmp_znode TSRMLS_CC);  zend_do_fetch_property(&$$, &tmp_znode, &$1 TSRMLS_CC);}
# ;
def p_object_property(p):
    '''object_property : object_dim_list
                       | variable_without_objects'''
    # Phply used to have variable_name before object_dum_list but it's moved
    # into object_dim_list instead of using empty.
    p[0] = p[1]

# object_dim_list:
#     object_dim_list '[' dim_offset ']'  { fetch_array_dim(&$$, &$1, &$3 TSRMLS_CC); }
#   |  object_dim_list '{' expr '}'    { fetch_string_offset(&$$, &$1, &$3 TSRMLS_CC); }
#   |  variable_name { znode tmp_znode;  zend_do_pop_object(&tmp_znode TSRMLS_CC);  zend_do_fetch_property(&$$, &tmp_znode, &$1 TSRMLS_CC);}
# ;
#
# something->something[something]
def p_object_dim_list_empty(p):
    'object_dim_list : variable_name'
    p[0] = [p[1]]

def p_object_dim_list_array_offset(p):
    'object_dim_list : object_dim_list LBRACKET dim_offset RBRACKET'
    p[0] = p[1] + [(ast.ArrayOffset, p[3], p.lineno(2))]

def p_object_dim_list_string_offset(p):
    'object_dim_list : object_dim_list LBRACE expr RBRACE'
    p[0] = p[1] + [(ast.StringOffset, p[3], p.lineno(2))]

# variable_name:
#     T_STRING    { $$ = $1; }
#   |  '{' expr '}'  { $$ = $2; }
# ;
def p_variable_name(p):
    '''variable_name : STRING
                     | LBRACE expr RBRACE'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

# simple_indirect_reference:
#     '$' { Z_LVAL($$.u.constant) = 1; }
#   |  simple_indirect_reference '$' { Z_LVAL($$.u.constant)++; }
# ;
def p_simple_indirect_reference(p):
    '''simple_indirect_reference : DOLLAR
                                 | simple_indirect_reference DOLLAR'''

    # Was:
    #
    # '''simple_indirect_reference : DOLLAR simple_indirect_reference
    #                              | reference_variable'''
    #
    # TODO:
    #   Almost certainly broken.
    if len(p) == 3:
        p[0] = ast.Variable(p[2], lineno=p.lineno(1))
    else:
        p[0] = p[1]

# assignment_list:
#     assignment_list ',' assignment_list_element
#   |  assignment_list_element
# ;
def p_assignment_list(p):
    '''assignment_list : assignment_list COMMA assignment_list_element
                       | assignment_list_element'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# assignment_list_element:
#     variable                { zend_do_add_list_element(&$1 TSRMLS_CC); }
#   |  T_LIST '(' { zend_do_new_list_begin(TSRMLS_C); } assignment_list ')'  { zend_do_new_list_end(TSRMLS_C); }
#   |  /* empty */              { zend_do_add_list_element(NULL TSRMLS_CC); }
# ;
def p_assignment_list_element(p):
    '''assignment_list_element : variable
                               | empty
                               | LIST LPAREN assignment_list RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[3]

# array_pair_list:
#     /* empty */ { zend_do_init_array(&$$, NULL, NULL, 0 TSRMLS_CC); }
#   |  non_empty_array_pair_list possible_comma  { $$ = $1; }
# ;
def p_array_pair_list(p):
    '''array_pair_list : empty
                       | non_empty_array_pair_list possible_comma'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1]

# non_empty_array_pair_list:
#     non_empty_array_pair_list ',' expr T_DOUBLE_ARROW expr  { zend_do_add_array_element(&$$, &$5, &$3, 0 TSRMLS_CC); }
#   |  non_empty_array_pair_list ',' expr      { zend_do_add_array_element(&$$, &$3, NULL, 0 TSRMLS_CC); }
#   |  expr T_DOUBLE_ARROW expr  { zend_do_init_array(&$$, &$3, &$1, 0 TSRMLS_CC); }
#   |  expr         { zend_do_init_array(&$$, &$1, NULL, 0 TSRMLS_CC); }
#   |  non_empty_array_pair_list ',' expr T_DOUBLE_ARROW '&' w_variable { zend_do_add_array_element(&$$, &$6, &$3, 1 TSRMLS_CC); }
#   |  non_empty_array_pair_list ',' '&' w_variable { zend_do_add_array_element(&$$, &$4, NULL, 1 TSRMLS_CC); }
#   |  expr T_DOUBLE_ARROW '&' w_variable  { zend_do_init_array(&$$, &$4, &$1, 1 TSRMLS_CC); }
#   |  '&' w_variable       { zend_do_init_array(&$$, &$2, NULL, 1 TSRMLS_CC); }
# ;
def p_non_empty_array_pair_list_item(p):
    '''non_empty_array_pair_list : non_empty_array_pair_list COMMA AND variable
                                 | non_empty_array_pair_list COMMA expr
                                 | AND variable
                                 | expr'''
    if len(p) == 5:
        p[0] = p[1] + [ast.ArrayElement(None, p[4], True, lineno=p.lineno(2))]
    elif len(p) == 4:
        p[0] = p[1] + [ast.ArrayElement(None, p[3], False, lineno=p.lineno(2))]
    elif len(p) == 3:
        p[0] = [ast.ArrayElement(None, p[2], True, lineno=p.lineno(1))]
    else:
        p[0] = [ast.ArrayElement(None, p[1], False, lineno=p.lineno(1))]

def p_non_empty_array_pair_list_pair(p):
    '''non_empty_array_pair_list : non_empty_array_pair_list COMMA expr DOUBLE_ARROW AND variable
                                 | non_empty_array_pair_list COMMA expr DOUBLE_ARROW expr
                                 | expr DOUBLE_ARROW AND variable
                                 | expr DOUBLE_ARROW expr'''
    if len(p) == 7:
        p[0] = p[1] + [ast.ArrayElement(p[3], p[6], True, lineno=p.lineno(2))]
    elif len(p) == 6:
        p[0] = p[1] + [ast.ArrayElement(p[3], p[5], False, lineno=p.lineno(2))]
    elif len(p) == 5:
        p[0] = [ast.ArrayElement(p[1], p[4], True, lineno=p.lineno(2))]
    else:
        p[0] = [ast.ArrayElement(p[1], p[3], False, lineno=p.lineno(2))]

# encaps_list:
#     encaps_list encaps_var { zend_do_end_variable_parse(&$2, BP_VAR_R, 0 TSRMLS_CC);  zend_do_add_variable(&$$, &$1, &$2 TSRMLS_CC); }
#   |  encaps_list T_ENCAPSED_AND_WHITESPACE  { zend_do_add_string(&$$, &$1, &$2 TSRMLS_CC); }
#   |  encaps_var { zend_do_end_variable_parse(&$1, BP_VAR_R, 0 TSRMLS_CC); zend_do_add_variable(&$$, NULL, &$1 TSRMLS_CC); }
#   |  T_ENCAPSED_AND_WHITESPACE encaps_var  { zend_do_add_string(&$$, NULL, &$1 TSRMLS_CC); zend_do_end_variable_parse(&$2, BP_VAR_R, 0 TSRMLS_CC); zend_do_add_variable(&$$, &$$, &$2 TSRMLS_CC); }
# ;
def p_encaps_list(p):
    '''encaps_list : encaps_list encaps_var'''
    # empty is removed
    if len(p) == 3:
        if p[1] == '':
            p[0] = p[2]
        else:
            p[0] = ast.BinaryOp('.', p[1], p[2], lineno=p.lineno(2))
    else:
        p[0] = ''

def p_encaps_list_string(p):
    'encaps_list : encaps_list ENCAPSED_AND_WHITESPACE'
    if p[1] == '':
        p[0] = p[2].decode('string_escape')
    else:
        p[0] = ast.BinaryOp('.', p[1], p[2].decode('string_escape'),
                            lineno=p.lineno(2))

def p_encaps_list_others(p):
    '''encaps_list : encaps_var
                   | ENCAPSED_AND_WHITESPACE encaps_list'''
    # not sure what these are all about

# encaps_var:
#     T_VARIABLE { zend_do_begin_variable_parse(TSRMLS_C); fetch_simple_variable(&$$, &$1, 1 TSRMLS_CC); }
#   |  T_VARIABLE '[' { zend_do_begin_variable_parse(TSRMLS_C); } encaps_var_offset ']'  { fetch_array_begin(&$$, &$1, &$4 TSRMLS_CC); }
#   |  T_VARIABLE T_OBJECT_OPERATOR T_STRING { zend_do_begin_variable_parse(TSRMLS_C); fetch_simple_variable(&$2, &$1, 1 TSRMLS_CC); zend_do_fetch_property(&$$, &$2, &$3 TSRMLS_CC); }
#   |  T_DOLLAR_OPEN_CURLY_BRACES expr '}' { zend_do_begin_variable_parse(TSRMLS_C);  fetch_simple_variable(&$$, &$2, 1 TSRMLS_CC); }
#   |  T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME '[' expr ']' '}' { zend_do_begin_variable_parse(TSRMLS_C);  fetch_array_begin(&$$, &$2, &$4 TSRMLS_CC); }
#   |  T_CURLY_OPEN variable '}' { $$ = $2; }
# ;
def p_encaps_var(p):
    'encaps_var : VARIABLE'
    p[0] = ast.Variable(p[1], lineno=p.lineno(1))

def p_encaps_var_array_offset(p):
    'encaps_var : VARIABLE LBRACKET encaps_var_offset RBRACKET'
    p[0] = ast.ArrayOffset(ast.Variable(p[1], lineno=p.lineno(1)), p[3],
                           lineno=p.lineno(2))

def p_encaps_var_object_property(p):
    'encaps_var : VARIABLE OBJECT_OPERATOR STRING'
    p[0] = ast.ObjectProperty(ast.Variable(p[1], lineno=p.lineno(1)), p[3],
                              lineno=p.lineno(2))

def p_encaps_var_dollar_curly_expr(p):
    'encaps_var : DOLLAR_OPEN_CURLY_BRACES expr RBRACE'
    p[0] = p[2]

def p_encaps_var_dollar_curly_array_offset(p):
    'encaps_var : DOLLAR_OPEN_CURLY_BRACES STRING_VARNAME LBRACKET expr RBRACKET RBRACE'
    p[0] = ast.ArrayOffset(ast.Variable('$' + p[2], lineno=p.lineno(2)), p[4],
                           lineno=p.lineno(3))

def p_encaps_var_curly_variable(p):
    'encaps_var : CURLY_OPEN variable RBRACE'
    p[0] = p[2]

# encaps_var_offset:
#     T_STRING    { $$ = $1; }
#   |  T_NUM_STRING  { $$ = $1; }
#   |  T_VARIABLE    { fetch_simple_variable(&$$, &$1, 1 TSRMLS_CC); }
# ;
def p_encaps_var_offset_string(p):
    'encaps_var_offset : STRING'
    p[0] = p[1]

def p_encaps_var_offset_num_string(p):
    'encaps_var_offset : NUM_STRING'
    p[0] = int(p[1])

def p_encaps_var_offset_variable(p):
    'encaps_var_offset : VARIABLE'
    p[0] = ast.Variable(p[1], lineno=p.lineno(1))

# internal_functions_in_yacc:
#     T_ISSET '(' isset_variables ')' { $$ = $3; }
#   |  T_EMPTY '(' variable ')'  { zend_do_isset_or_isempty(ZEND_ISEMPTY, &$$, &$3 TSRMLS_CC); }
#   |  T_EMPTY '(' expr_without_variable ')' { zend_do_unary_op(ZEND_BOOL_NOT, &$$, &$3 TSRMLS_CC); }
#   |  T_INCLUDE expr       { zend_do_include_or_eval(ZEND_INCLUDE, &$$, &$2 TSRMLS_CC); }
#   |  T_INCLUDE_ONCE expr   { zend_do_include_or_eval(ZEND_INCLUDE_ONCE, &$$, &$2 TSRMLS_CC); }
#   |  T_EVAL '(' expr ')'   { zend_do_include_or_eval(ZEND_EVAL, &$$, &$3 TSRMLS_CC); }
#   |  T_REQUIRE expr      { zend_do_include_or_eval(ZEND_REQUIRE, &$$, &$2 TSRMLS_CC); }
#   |  T_REQUIRE_ONCE expr    { zend_do_include_or_eval(ZEND_REQUIRE_ONCE, &$$, &$2 TSRMLS_CC); }
# ;
def p_internal_functions_in_yacc(p):
    '''internal_functions_in_yacc : ISSET LPAREN isset_variables RPAREN
                                  | EMPTY LPAREN variable RPAREN
                                  | EMPTY LPAREN expr_without_variable RPAREN
                                  | INCLUDE expr
                                  | INCLUDE_ONCE expr
                                  | EVAL LPAREN expr RPAREN
                                  | REQUIRE expr
                                  | REQUIRE_ONCE expr
                                  '''
# isset_variables:
#     isset_variable      { $$ = $1; }
#   |  isset_variables ',' { zend_do_boolean_and_begin(&$1, &$2 TSRMLS_CC); } isset_variable { zend_do_boolean_and_end(&$$, &$1, &$4, &$2 TSRMLS_CC); }
# ;
def p_isset_variables(p):
    '''isset_variables : isset_variables COMMA isset_variable
                       | isset_variable'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

# isset_variable:
#     variable        { zend_do_isset_or_isempty(ZEND_ISSET, &$$, &$1 TSRMLS_CC); }
#   |  expr_without_variable  { zend_error_noreturn(E_COMPILE_ERROR, "Cannot use isset() on the result of an expression (you can use \"null !== expression\" instead)"); }
# ;
def p_isset_variable(p):
    '''isset_variable : variable
                      | expr_without_variable'''
    p[0] = p[1]

# class_constant:
#     class_name T_DOUBLE_COLON T_STRING { zend_do_fetch_constant(&$$, &$1, &$3, ZEND_RT, 0 TSRMLS_CC); }
#   |  variable_class_name T_DOUBLE_COLON T_STRING { zend_do_fetch_constant(&$$, &$1, &$3, ZEND_RT, 0 TSRMLS_CC); }
# ;
def p_class_constant(p):
    '''class_constant : class_name DOUBLE_COLON STRING
                      | variable_class_name DOUBLE_COLON STRING'''
    p[0] = ast.StaticProperty(p[1], p[3], lineno=p.lineno(2))

# static_class_name_scalar:
#   class_name T_DOUBLE_COLON T_CLASS { zend_do_resolve_class_name(&$$, &$1, 1 TSRMLS_CC); }
# ;
def p_static_class_name_scalar(p):
    '''static_class_name_scalar : class_name DOUBLE_COLON CLASS'''

# class_name_scalar:
#   class_name T_DOUBLE_COLON T_CLASS { zend_do_resolve_class_name(&$$, &$1, 0 TSRMLS_CC); }
# ;
def p_class_name_scalar(p):
    '''class_name_scalar : class_name DOUBLE_COLON CLASS'''
    p[0] = p[1]

def p_empty(p):
    'empty : '

class ParseError(Exception):
    pass

# Error rule for syntax errors
def p_error(t):
    if t:
        raise ParseError('invalid syntax', (None, t.lineno, None, t.value))
    else:
        raise ParseError('unexpected EOF while parsing', (None, None, None, None))

# A logger has to be manually assigned.
logger = logging.getLogger(__name__)

# Build the grammar
parser = yacc.yacc(debug=True)

def parse_string(string):
    # TODO: probs the wrong place for this function
    from dephp.scanner import lexer
    lexer.lexer.begin('INITIAL')
    lexer.input(string)
    return parser.parse(lexer=lexer, debug=logger)
