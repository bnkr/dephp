"""
Lexer for PHP based on work done in the phply project.

Copyright (c) 2010 by Dave Benjamin and contributors.  See AUTHORS
for more details.

Some rights reserved.

Redistribution and use in source and binary forms of the software as well
as documentation, with or without modification, are permitted provided
that the following conditions are met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.

* The names of the contributors may not be used to endorse or
  promote products derived from this software without specific
  prior written permission.

THIS SOFTWARE AND DOCUMENTATION IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT
NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE AND DOCUMENTATION, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
"""
import ply.lex as lex
import re

states = (
    ('php', 'exclusive'),
    ('quoted', 'exclusive'),
    ('quotedvar', 'exclusive'),
    ('varname', 'exclusive'),
    ('offset', 'exclusive'),
    ('property', 'exclusive'),
    ('heredoc', 'exclusive'),
    ('heredocvar', 'exclusive'),
)

# Reserved words
reserved = (
    'ARRAY', 'AS', 'BREAK', 'CASE', 'CLASS', 'CONST', 'CONTINUE', 'DECLARE',
    'DEFAULT', 'DO', 'ECHO', 'ELSE', 'ELSEIF', 'EMPTY', 'ENDDECLARE',
    'ENDFOR', 'ENDFOREACH', 'ENDIF', 'ENDSWITCH', 'ENDWHILE', 'EVAL', 'EXIT',
    'EXTENDS', 'FOR', 'FOREACH', 'FUNCTION', 'GLOBAL', 'IF', 'INCLUDE',
    'INCLUDE_ONCE', 'INSTANCEOF', 'ISSET', 'LIST', 'NEW', 'PRINT', 'REQUIRE',
    'REQUIRE_ONCE', 'RETURN', 'STATIC', 'SWITCH', 'UNSET', 'USE', 'VAR',
    'WHILE', 'FINAL', 'INTERFACE', 'IMPLEMENTS', 'PUBLIC', 'PRIVATE',
    'PROTECTED', 'ABSTRACT', 'CLONE', 'TRY', 'CATCH', 'THROW', 'NAMESPACE',
    'TRAIT', 'GOTO', 'FINALLY', 'CALLABLE', 'INSTEADOF', 'YIELD',
)

# Not used by parser
unparsed = (
    # Invisible characters
    'WHITESPACE',

    # Open and close tags
    'OPEN_TAG', 'OPEN_TAG_WITH_ECHO', 'CLOSE_TAG',

    # Comments
    'COMMENT', 'DOC_COMMENT',
)

tokens = reserved + unparsed + (
    # Operators
    'PLUS', 'MINUS', 'MUL', 'DIV', 'MOD', 'AND', 'OR', 'NOT', 'XOR', 'SL',
    'SR', 'BOOLEAN_AND', 'BOOLEAN_OR', 'BOOLEAN_NOT', 'IS_SMALLER',
    'IS_GREATER', 'IS_SMALLER_OR_EQUAL', 'IS_GREATER_OR_EQUAL', 'IS_EQUAL',
    'IS_NOT_EQUAL', 'IS_IDENTICAL', 'IS_NOT_IDENTICAL', 'POW',

    # Assignment operators
    'EQUALS', 'MUL_EQUAL', 'DIV_EQUAL', 'MOD_EQUAL', 'PLUS_EQUAL',
    'MINUS_EQUAL', 'SL_EQUAL', 'SR_EQUAL', 'AND_EQUAL', 'OR_EQUAL',
    'XOR_EQUAL', 'CONCAT_EQUAL', 'POW_EQUAL',

    # Increment/decrement
    'INC', 'DEC',

    # Arrows
    'OBJECT_OPERATOR', 'DOUBLE_ARROW', 'DOUBLE_COLON',

    # Delimiters
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LBRACE', 'RBRACE', 'DOLLAR',
    'COMMA', 'CONCAT', 'QUESTION', 'COLON', 'SEMI', 'AT', 'NS_SEPARATOR',
    'ELLIPSIS',

    # Casts
    'ARRAY_CAST', 'BOOL_CAST', 'DOUBLE_CAST', 'INT_CAST', 'OBJECT_CAST',
    'STRING_CAST', 'UNSET_CAST',

    # Escaping from HTML
    'INLINE_HTML',

    # Identifiers and reserved words
    'DIR', 'FILE', 'LINE', 'FUNC_C', 'CLASS_C', 'METHOD_C', 'NS_C', 'TRAIT_C',
    'LOGICAL_AND', 'LOGICAL_OR', 'LOGICAL_XOR',
    'HALT_COMPILER',
    'STRING', 'VARIABLE',
    'LNUMBER', 'DNUMBER', 'NUM_STRING',
    'CONSTANT_ENCAPSED_STRING', 'ENCAPSED_AND_WHITESPACE', 'QUOTE', 'BACKTICK',
    'DOLLAR_OPEN_CURLY_BRACES', 'STRING_VARNAME', 'CURLY_OPEN',

    # Heredocs
    'START_HEREDOC', 'END_HEREDOC',
)

# Newlines
def t_php_WHITESPACE(t):
    r'[ \t\r\n]+'
    t.lexer.lineno += t.value.count("\n")
    return t

# Operators
t_php_PLUS                = r'\+'
t_php_MINUS               = r'-'
t_php_MUL                 = r'\*'
t_php_POW                 = r'\*\*'
t_php_DIV                 = r'/'
t_php_MOD                 = r'%'
t_php_AND                 = r'&'
t_php_OR                  = r'\|'
t_php_NOT                 = r'~'
t_php_XOR                 = r'\^'
t_php_SL                  = r'<<'
t_php_SR                  = r'>>'
t_php_BOOLEAN_AND         = r'&&'
t_php_BOOLEAN_OR          = r'\|\|'
t_php_BOOLEAN_NOT         = r'!'
t_php_IS_SMALLER          = r'<'
t_php_IS_GREATER          = r'>'
t_php_IS_SMALLER_OR_EQUAL = r'<='
t_php_IS_GREATER_OR_EQUAL = r'>='
t_php_IS_EQUAL            = r'=='
t_php_IS_NOT_EQUAL        = r'(!=(?!=))|(<>)'
t_php_IS_IDENTICAL        = r'==='
t_php_IS_NOT_IDENTICAL    = r'!=='

# Assignment operators
t_php_EQUALS               = r'='
t_php_MUL_EQUAL            = r'\*='
t_php_POW_EQUAL            = r'\*\*='
t_php_DIV_EQUAL            = r'/='
t_php_MOD_EQUAL            = r'%='
t_php_PLUS_EQUAL           = r'\+='
t_php_MINUS_EQUAL          = r'-='
t_php_SL_EQUAL             = r'<<='
t_php_SR_EQUAL             = r'>>='
t_php_AND_EQUAL            = r'&='
t_php_OR_EQUAL             = r'\|='
t_php_XOR_EQUAL            = r'\^='
t_php_CONCAT_EQUAL         = r'\.='

# Increment/decrement
t_php_INC                  = r'\+\+'
t_php_DEC                  = r'--'

# Arrows
t_php_DOUBLE_ARROW         = r'=>'
t_php_DOUBLE_COLON         = r'::'

def t_php_OBJECT_OPERATOR(t):
    r'->'
    if re.match(r'[A-Za-z_]', peek(t.lexer)):
        t.lexer.push_state('property')
    return t

# Delimeters
t_php_LPAREN               = r'\('
t_php_RPAREN               = r'\)'
t_php_DOLLAR               = r'\$'
t_php_COMMA                = r','
t_php_CONCAT               = r'\.(?!\d|=)'
t_php_QUESTION             = r'\?'
t_php_COLON                = r':'
t_php_SEMI                 = r';'
t_php_AT                   = r'@'
t_php_NS_SEPARATOR         = r'\\'

def t_php_LBRACKET(t):
    r'\['
    t.lexer.push_state('php')
    return t

def t_php_RBRACKET(t):
    r'\]'
    t.lexer.pop_state()
    return t

def t_php_LBRACE(t):
    r'\{'
    t.lexer.push_state('php')
    return t

def t_php_RBRACE(t):
    r'\}'
    t.lexer.pop_state()
    return t

t_php_ELLIPSIS             = r'\.\.\.'

# Casts
t_php_ARRAY_CAST           = r'\([ \t]*[Aa][Rr][Rr][Aa][Yy][ \t]*\)'
t_php_BOOL_CAST            = r'\([ \t]*[Bb][Oo][Oo][Ll]([Ee][Aa][Nn])?[ \t]*\)'
t_php_DOUBLE_CAST          = r'\([ \t]*([Rr][Ee][Aa][Ll]|[Dd][Oo][Uu][Bb][Ll][Ee]|[Ff][Ll][Oo][Aa][Tt])[ \t]*\)'
t_php_INT_CAST             = r'\([ \t]*[Ii][Nn][Tt]([Ee][Gg][Ee][Rr])?[ \t]*\)'
t_php_OBJECT_CAST          = r'\([ \t]*[Oo][Bb][Jj][Ee][Cc][Tt][ \t]*\)'
t_php_STRING_CAST          = r'\([ \t]*[Ss][Tt][Rr][Ii][Nn][Gg][ \t]*\)'
t_php_UNSET_CAST           = r'\([ \t]*[Uu][Nn][Ss][Ee][Tt][ \t]*\)'

# Comments

def t_php_DOC_COMMENT(t):
    r'/\*\*(.|\n)*?\*/'
    t.lexer.lineno += t.value.count("\n")
    return t

def t_php_COMMENT(t):
    r'/\*(.|\n)*?\*/ | //([^?%\n]|[?%](?!>))*\n? | \#([^?%\n]|[?%](?!>))*\n?'
    t.lexer.lineno += t.value.count("\n")
    return t

# Escaping from HTML

def t_OPEN_TAG(t):
    r'<[?%]((php[ \t\r\n]?)|=)?'
    if '=' in t.value: t.type = 'OPEN_TAG_WITH_ECHO'
    t.lexer.lineno += t.value.count("\n")
    t.lexer.begin('php')
    return t

def t_php_CLOSE_TAG(t):
    r'[?%]>\r?\n?'
    t.lexer.lineno += t.value.count("\n")
    t.lexer.begin('INITIAL')
    return t

def t_INLINE_HTML(t):
    r'([^<]|<(?![?%]))+'
    t.lexer.lineno += t.value.count("\n")
    return t

# Identifiers and reserved words

reserved_map = {
    '__DIR__':         'DIR',
    '__FILE__':        'FILE',
    '__LINE__':        'LINE',
    '__FUNCTION__':    'FUNC_C',
    '__CLASS__':       'CLASS_C',
    '__METHOD__':      'METHOD_C',
    '__NAMESPACE__':   'NS_C',
    '__TRAIT__':       'TRAIT_C',

    'AND':             'LOGICAL_AND',
    'OR':              'LOGICAL_OR',
    'XOR':             'LOGICAL_XOR',

    'DIE':             'EXIT',
    '__HALT_COMPILER': 'HALT_COMPILER',
}

for r in reserved:
    reserved_map[r] = r

# Identifier
def t_php_STRING(t):
    r'[A-Za-z_][\w_]*'
    t.type = reserved_map.get(t.value.upper(), 'STRING')
    return t

# Variable
def t_php_VARIABLE(t):
    r'\$[A-Za-z_][\w_]*'
    return t

# Floating literal
def t_php_DNUMBER(t):
    r'(\d*\.\d+|\d+\.\d*)([Ee][+-]?\d+)? | (\d+[Ee][+-]?\d+)'
    return t

# Integer literal
def t_php_LNUMBER(t):
    r'(0x[0-9A-Fa-f]+)|\d+'
    return t

# String literal
def t_php_CONSTANT_ENCAPSED_STRING(t):
    r"'([^\\']|\\(.|\n))*'"
    t.lexer.lineno += t.value.count("\n")
    return t

def t_php_BACKTICK(t):
    r'`'
    return t

def t_php_QUOTE(t):
    r'"'
    t.lexer.push_state('quoted')
    return t

def t_quoted_QUOTE(t):
    r'"'
    t.lexer.pop_state()
    return t

def t_quoted_ENCAPSED_AND_WHITESPACE(t):
    r'( [^"\\${] | \\(.|\n) | \$(?![A-Za-z_{]) | \{(?!\$) )+'
    t.lexer.lineno += t.value.count("\n")
    return t

def t_quoted_VARIABLE(t):
    r'\$[A-Za-z_][\w_]*'
    t.lexer.push_state('quotedvar')
    return t

def t_quoted_CURLY_OPEN(t):
    r'\{(?=\$)'
    t.lexer.push_state('php')
    return t

def t_quoted_DOLLAR_OPEN_CURLY_BRACES(t):
    r'\$\{'
    if re.match(r'[A-Za-z_]', peek(t.lexer)):
        t.lexer.push_state('varname')
    else:
        t.lexer.push_state('php')
    return t

def t_quotedvar_QUOTE(t):
    r'"'
    t.lexer.pop_state()
    t.lexer.pop_state()
    return t

def t_quotedvar_LBRACKET(t):
    r'\['
    t.lexer.begin('offset')
    return t

def t_quotedvar_OBJECT_OPERATOR(t):
    r'->(?=[A-Za-z])'
    t.lexer.begin('property')
    return t

def t_quotedvar_ENCAPSED_AND_WHITESPACE(t):
    r'( [^"\\${] | \\(.|\n) | \$(?![A-Za-z_{]) | \{(?!\$) )+'
    t.lexer.lineno += t.value.count("\n")
    t.lexer.pop_state()
    return t

t_quotedvar_VARIABLE = t_php_VARIABLE

def t_quotedvar_CURLY_OPEN(t):
    r'\{(?=\$)'
    t.lexer.begin('php')
    return t

def t_quotedvar_DOLLAR_OPEN_CURLY_BRACES(t):
    r'\$\{'
    if re.match(r'[A-Za-z_]', peek(t.lexer)):
        t.lexer.begin('varname')
    else:
        t.lexer.begin('php')
    return t

def t_varname_STRING_VARNAME(t):
    r'[A-Za-z_][\w_]*'
    return t

t_varname_RBRACE = t_php_RBRACE
t_varname_LBRACKET = t_php_LBRACKET

def t_offset_STRING(t):
    r'[A-Za-z_][\w_]*'
    return t

def t_offset_NUM_STRING(t):
    r'\d+'
    return t

t_offset_VARIABLE = t_php_VARIABLE
t_offset_RBRACKET = t_php_RBRACKET

def t_property_STRING(t):
    r'[A-Za-z_][\w_]*'
    t.lexer.pop_state()
    return t

# Heredocs

def t_php_START_HEREDOC(t):
    r'<<<[ \t]*(?P<label>[A-Za-z_][\w_]*)\n'
    t.lexer.lineno += t.value.count("\n")
    t.lexer.push_state('heredoc')
    t.lexer.heredoc_label = t.lexer.lexmatch.group('label')
    return t

def t_heredoc_END_HEREDOC(t):
    r'(?<=\n)[A-Za-z_][\w_]*'
    if t.value == t.lexer.heredoc_label:
        del t.lexer.heredoc_label
        t.lexer.pop_state()
    else:
        t.type = 'ENCAPSED_AND_WHITESPACE'
    return t

def t_heredoc_ENCAPSED_AND_WHITESPACE(t):
    r'( [^\n\\${] | \\. | \$(?![A-Za-z_{]) | \{(?!\$) )+\n? | \\?\n'
    t.lexer.lineno += t.value.count("\n")
    return t

def t_heredoc_VARIABLE(t):
    r'\$[A-Za-z_][\w_]*'
    t.lexer.push_state('heredocvar')
    return t

t_heredoc_CURLY_OPEN = t_quoted_CURLY_OPEN
t_heredoc_DOLLAR_OPEN_CURLY_BRACES = t_quoted_DOLLAR_OPEN_CURLY_BRACES

def t_heredocvar_ENCAPSED_AND_WHITESPACE(t):
    r'( [^\n\\${] | \\. | \$(?![A-Za-z_{]) | \{(?!\$) )+\n? | \\?\n'
    t.lexer.lineno += t.value.count("\n")
    t.lexer.pop_state()
    return t

t_heredocvar_LBRACKET = t_quotedvar_LBRACKET
t_heredocvar_OBJECT_OPERATOR = t_quotedvar_OBJECT_OPERATOR
t_heredocvar_VARIABLE = t_quotedvar_VARIABLE
t_heredocvar_CURLY_OPEN = t_quotedvar_CURLY_OPEN
t_heredocvar_DOLLAR_OPEN_CURLY_BRACES = t_quotedvar_DOLLAR_OPEN_CURLY_BRACES

class LexerSyntaxError(Exception):
    def __init__(self, token):
        self.line = token.lineno
        self.value = token.value
        Exception.__init__(self, "unknown token", token)

def t_ANY_error(t):
    # TODO:
    #   Can we make this optional?
    t.lexer.skip(1)
    t.type = "UNKNOWN"
    t.value = t.value[:1]
    return t

def peek(lexer):
    try:
        return lexer.lexdata[lexer.lexpos]
    except IndexError:
        return ''

class FilteredLexer(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.last_token = None

    @property
    def lineno(self):
        return self.lexer.lineno

    @lineno.setter
    def lineno(self, value):
        self.lexer.lineno = value

    @property
    def lexpos(self):
        return self.lexer.lexpos

    @lexpos.setter
    def lexpos(self, value):
        self.lexer.lexpos = value

    def clone(self):
        return FilteredLexer(self.lexer.clone())

    def current_state(self):
        return self.lexer.current_state()

    def input(self, input):
        self.lexer.input(input)

    def token(self):
        t = self.lexer.token()

        # Filter out tokens that the parser is not expecting.
        while t and t.type in unparsed:

            # Skip over open tags, but keep track of when we see them.
            if t.type == 'OPEN_TAG':
                self.last_token = t
                t = self.lexer.token()
                continue

            # Rewrite <?= to yield an "echo" statement.
            if t.type == 'OPEN_TAG_WITH_ECHO':
                t.type = 'ECHO'
                break

            # Insert semicolons in place of close tags where necessary.
            if t.type == 'CLOSE_TAG':
                if self.last_token and \
                       self.last_token.type in ('OPEN_TAG', 'SEMI', 'COLON',
                                                'LBRACE', 'RBRACE'):
                    # Dont insert semicolons after these tokens.
                    pass
                else:
                    # Rewrite close tag as a semicolon.
                    t.type = 'SEMI'
                    break

            t = self.lexer.token()

        self.last_token = t
        return t

    # Iterator interface
    def __iter__(self):
        return self

    def next(self):
        t = self.token()
        if t is None:
            raise StopIteration
        return t

    __next__ = next

full_lexer = lex.lex(debug=0)
lexer = FilteredLexer(full_lexer)

full_tokens = tokens
tokens = filter(lambda token: token not in unparsed, tokens)

def scan_string(string):
    """Return a list of tokens scanned from a string."""
    def iter_tokens(lexer):
        while True:
            token = lexer.token()
            if token is None:
                break
            else:
                yield token

    lexer.lexer.begin('INITIAL')
    lexer.input(string)

    return list(iter_tokens(lexer))
