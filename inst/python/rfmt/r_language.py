#  Copyright 2015 Google Inc. All Rights Reserved.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

"""R-specific portions (to wit, lexer and parser) of the R formatter.

Much of the code for the R lexer is structured along the lines of the C code
in the R system implementation itself, and the PLY grammar used here is adapted
from the YACC grammar used in the R parser. For details, see:

https://svn.r-project.org/R/trunk/src/main/gram.y
"""

import collections
import cStringIO as StringIO
import os
import ply.yacc as yacc

import base
import rparsetab


class _RStream(object):
  """A character stream with facilities for the R lexer."""

  def __init__(self, stream_input):
    if not (isinstance(stream_input, basestring) or
            isinstance(stream_input, file)):
      raise RuntimeError('Either a file or a string should be provided')
    if isinstance(stream_input, file):
      self.stream = stream_input
    else:
      self.stream = StringIO.StringIO(stream_input)
    self.line = 1
    self.col = 0
    self.last_col = 0
    self.last_getc = None

  def GetChar(self):
    """Read the next character, updating the current line and column."""
    c = self.stream.read(1) or 'R_EOF'
    if c != 'R_EOF':
      if c == '\n':
        self.line += 1
        self.last_col, self.col = self.col, 0
      else:
        self.col += 1
    self.last_getc = c  # Record for use in UnGetChar below.
    return c

  def UnGetChar(self):
    """Push the last fetched character back onto the input stream."""
    # Currently, no provision is made for calling UnGetChar twice, with no
    # intervening GetChar.
    if self.last_getc is None:
      raise AssertionError('Too many UnGetChar\'s')
    # Don't back up if we're past the end of the file, but do back up if we just
    # arrived there.
    if self.last_getc != 'R_EOF':
      self.stream.seek(-1, os.SEEK_CUR)
      if self.last_getc == '\n':
        self.col, self.last_col = self.last_col, 0
        self.line -= 1
        self.last_getc = None
      else:
        self.col -= 1

  def IsNextChar(self, c):
    """Fetch a character from the input only if it has the value specified."""
    if self.GetChar() == c:
      return True
    else:
      self.UnGetChar()
      return False

  def Peek(self):
    """Return the value of the next input character, but do not fetch it."""
    c = self.GetChar()
    self.UnGetChar()
    return c

  def Checkpoint(self):
    """Record state including the current position of the input stream."""
    self.checkpoint = (self.stream.tell(),
                       (self.line, self.col, self.last_col, self.last_getc))

  def Reset(self):
    """Return to the state at the last checkpoint."""
    self.stream.seek(self.checkpoint[0], os.SEEK_SET)
    self.line, self.col, self.last_col, self.last_getc = self.checkpoint[1]

  def Close(self):
    self.stream.close()


class _RComments(object):
  """An object representing comments in R source code (abstract class).
  """

  def __init__(self):
    self.lines = []

  def __str__(self):
    return '#' + str(self.lines)

  def __nonzero__(self):
    """If this object contains at least one line of text."""
    return bool(self.lines)

  def AppendLine(self, line):
    """Append a line of text to the comment."""
    self.lines.append(line)

  def Trim(self):
    self.lines = [ln.lstrip('\n') for ln in self.lines]


class _RLineComments(_RComments):
  """A class representing in-line comments.
  """

  def __str__(self):
    return 'L' + super(_RLineComments, self).__str__()


class _RBlockComments(_RComments):
  """A comment that occupies entire lines and may include blank lines.
  """

  def __str__(self):
    return 'B' + super(_RBlockComments, self).__str__()


class RToken(object):
  """An analog of the PLY token, with positional information and comments."""

  def __init__(self, token_type, value, line, col):
    self.line = line
    self.col = col
    self.type = token_type
    self.value = value
    self.pre_comments = None
    self.post_comments = None

  def __str__(self):
    return '(%s, %s, %s, %s)@(%d,%d)' % (self.type, self.value,
                                         str(self.pre_comments),
                                         str(self.post_comments),
                                         self.line, self.col)


class _RTokenizer(object):
  """The substrate of the R lexer, responsible for basic tokenization."""

  def __init__(self, stream_input):
    self.stream = _RStream(stream_input)

  def Token(self, token_type, value=None):
    """Construct and return a new RToken.

    Args:
      token_type: a string specifying the NextToken's terminal "type", as used
        in PLY's grammar productions.
      value: a string giving the actual text associated with the NextToken. If
        the value supplied is "None", the NextToken's type is its value, too.
    Returns:
      An RToken with the type and value supplied, and positional information
      taken from the input stream.
    """
    return RToken(token_type, value or token_type, self.stream.line,
                  self.stream.col)

  def SkipSpace(self):
    """Skip whitespace in the input."""
    while True:
      c = self.stream.GetChar()
      if not (c == ' ' or c == '\t' or c == '\f'):
        return c

  def ProbeForCommentLine(self, break_on_blank_line=False):
    """Look for a line comprising comments.

    Args:
      break_on_blank_line: whether to cease the search after a blank line
    Returns:
      A string comprising the line, or None if none are found.
    """
    ws_chars = (' ', '\t', '\f')
    le_chars = ('\n', 'R_EOF')
    s = []
    def NextChar(c):
      if c:
        s.append(c)
      return self.stream.GetChar()
    def Line():
      return ''.join(s).lstrip('\n')
    state = 1
    c = ''
    while True:
      c = NextChar(c)
      if state == 1:
        if c == '\n' or c in ws_chars:
          if break_on_blank_line and c == '\n':
            break
          if self.stream.Peek() == 'R_EOF': break
          state = 2
          continue
        elif c == '#':
          state = 3
          continue
        else:
          break
      if state == 2:
        if c in ws_chars:
          continue
        elif c in le_chars:
          return Line()
        elif c == '#':
          state = 3
          continue
        else:
          break
      if state == 3:
        if c in le_chars:
          return Line()
        else:
          continue
    return None

  def ProbeForComments(self, post):
    """Look for comments before or after a token.

    Args:
      post: whether the comments occur after a token.
    Returns:
      An RLineComments (post = True) or RBlockComments (post = False)
      containing the comments, or None if none are found.
    """
    comments = (_RLineComments if post else _RBlockComments)()
    start_col = self.stream.col
    hash_col = 0
    do_reset = True
    while True:
      self.stream.Checkpoint()
      line = self.ProbeForCommentLine(
          break_on_blank_line=post and not hash_col)
      if line is None:
        break
      if post:
        ls_line = line.lstrip()
        if not ls_line:
          # Absorb trailing spaces, but don't make up a line comment
          do_reset = hash_col > 0
          self.stream.UnGetChar()  # Leave last CR in the stream
          break
        if not hash_col:
          hash_col = start_col + len(line) - len(ls_line) + 1
        elif len(line) - len(ls_line) + 1 < hash_col:
          break
      self.stream.UnGetChar()
      comments.AppendLine(line)
    if do_reset:
      self.stream.Reset()
    if comments:
      return comments
    else:
      return None

  def NumericValue(self, c):
    """Read a NextToken comprising a numeric value.

    Args:
      c: the first character of the numeric value.
    Returns:
      An RToken of type 'NUM_CONST' containing the numeric value as a string.
    """
    seen_dot = c == '.'
    seen_exp = False
    s = [c]
    while True:
      c = self.stream.GetChar()
      if not (c.isdigit() or c == '.' or c == 'e' or c == 'E'):
        break
      if c == 'e' or c == 'E':
        if seen_exp: break
        seen_exp = True
        seen_dot = True
        s.append(c)
        c = self.stream.GetChar()
        if not (c.isdigit() or c == '+' or c == '-'): break
      if c == '.':
        if seen_dot: break
        seen_dot = True
      s.append(c)
    if c in ('i', 'L'):
      s.append(c)
    else:
      self.stream.UnGetChar()
    return self.Token('NUM_CONST', ''.join(s))

  def StringValue(self, quote):
    """Read a NextToken comprising a literal string value.

    Args:
      quote: the quote character signifying the beginning of the string.
    Returns:
      An RToken of type 'STR_CONST' containing the string.
    """
    s = [quote]
    while True:
      c = self.stream.GetChar()
      if c == 'R_EOF':
        c = quote
      s.append(c)
      if c == quote:
        return self.Token('STR_CONST', ''.join(s))
      if c == '\\':
        s.append(self.stream.GetChar())

  def QuotedSymbolValue(self, c):
    """Read a NextToken comprising a back-quoted symbol.

    Args:
      c: the back quote character signifying the beginning of the string.
    Returns:
      An RToken of type 'SYMBOL' containing the symbol.
    """
    val = self.StringValue(c)
    val.type = 'SYMBOL'
    return val

  def SpecialValue(self, c):
    """Read a NextToken comprising a special operator, delimited by '%'.

    Args:
      c: the '%' character signifying the beginning of the string.
    Returns:
      An RToken of type 'SPECIAL' containing the operator.
    """
    s = [c]
    while True:
      c = self.stream.GetChar()
      if c == 'R_EOF': c = '%'
      if c == '\n':
        _RaiseRSyntaxError('Syntax error:\n End of line in special',
                           (self.stream.line, self.stream.col))
      s.append(c)
      if c == '%': return self.Token('SPECIAL', ''.join(s))

  # Certain symbols are recognized as keywords if they're in the following
  # dict.
  keywords = {'function': 'FUNCTION',
              'while': 'WHILE',
              'repeat': 'REPEAT',
              'for': 'FOR',
              'if': 'IF',
              'in': 'IN',
              'else': 'ELSE',
              'next': 'NEXT',
              'break': 'BREAK'}

  def SymbolValue(self, c):
    """Read a non-quoted symbol (usually an identifier) or keyword.

    Args:
      c: the character beginning the symbol.
    Returns:
      An RToken containing the symbol, with a particular keyword type if the
        symbol is one of R's reserved keywords, or of type 'SYMBOL' otherwise.
    """
    s = [c]
    while True:
      c = self.stream.GetChar()
      # Special case simple package qualifications "pkg::var" and "pkg:::var";
      # rendering these as a single token, rather than a binary expression
      # avoids considerable unpleasantness in formatting.
      if c == ':':
        self.stream.Checkpoint()
        if self.stream.GetChar() == ':':  # Got '::'
          s += [':'] * 2
          c = self.stream.GetChar()
          if c == ':':  # Got ':::'
            s += [':']
            c = self.stream.GetChar()
        else:
          self.stream.Reset()
      if c == 'R_EOF' or not (c.isalnum() or c == '.' or c == '_'):
        self.stream.UnGetChar()
        txt = ''.join(s)
        return self.Token(_RTokenizer.keywords.get(txt, 'SYMBOL'), txt)
      s.append(c)

  def NextToken(self):
    """Read the next token from the input stream."""
    c = self.SkipSpace()
    # The structure of this method follows that of the C code fairly closely;
    # doubtless it could be factored more elegantly.
    if c == 'R_EOF':
      return self.Token('END_OF_INPUT')
    if c.isdigit() or (c == '.' and self.stream.Peek().isdigit()):
      return self.NumericValue(c)
    if c == '"' or c == "'":
      return self.StringValue(c)
    if c == '%':
      return self.SpecialValue(c)
    if c == '`':
      return self.QuotedSymbolValue(c)
    if c.isalpha() or c == '.':
      return self.SymbolValue(c)
    if c == '{':
      return self.Token('LBRACE', '{')
    if c == '}':
      return self.Token('RBRACE', '}')
    if c == '<':
      if self.stream.IsNextChar('='):
        return self.Token('LE', '<=')
      if self.stream.IsNextChar('-'):
        return self.Token('LEFT_ASSIGN', '<-')
      if self.stream.IsNextChar('<'):
        if self.stream.IsNextChar('-'):
          return self.Token('LEFT_ASSIGN', '<<-')
        else:
          return None
      return self.Token('<')
    if c == '-':
      if self.stream.IsNextChar('>'):
        if self.stream.IsNextChar('>'):
          return self.Token('RIGHT_ASSIGN', '->>')
        else:
          return self.Token('RIGHT_ASSIGN', '->')
      return self.Token('-')
    if c == '>':
      if self.stream.IsNextChar('='):
        return self.Token('GE', '>=')
      else:
        return self.Token('>')
    if c == '!':
      if self.stream.IsNextChar('='):
        return self.Token('NE', '!=')
      else:
        self.Token('!')
    if c == '=':
      if self.stream.IsNextChar('='):
        return self.Token('EQ', '==')
      else:
        return self.Token('EQ_ASSIGN', '=')
    if c == ':':
      if self.stream.IsNextChar(':'):
        if self.stream.IsNextChar(':'):
          return self.Token('NS_GET_INT', ':::')
        else:
          return self.Token('NS_GET', '::')
      if self.stream.IsNextChar('='):
        return self.Token('LEFT_ASSIGN', ':=')
      return self.Token(':')
    if c == '&':
      if self.stream.IsNextChar('&'):
        return self.Token('AND2', '&&')
      else:
        return self.Token('AND', '&')
    if c == '|':
      if self.stream.IsNextChar('|'):
        return self.Token('OR2', '||')
      else:
        return self.Token('OR', '|')
    if c == '[':
      if self.stream.IsNextChar('['):
        return self.Token('LBB', '[[')
      else:
        return self.Token('[')
    if c == '*':
      if self.stream.IsNextChar('*'):
        return self.Token('^')
      else:
        return self.Token(c)
    # Default
    return self.Token(c)


class _RLexer(object):
  """An object that carries out context-sensitive lexing of the R language.

  For explanation of the grisly details, see:
  https://github.com/wch/r-source/blob/trunk/src/main/gram.y#L2975
  """

  def __init__(self):
    # Vacuous; the design of PLY makes it expedient to relegate the setup to
    # the input method below.
    pass

  # Non-Google standard naming is mandated by PLY.
  # pylint: disable=invalid-name
  def input(self, stream_input):
    """Initialize the lexer from a string or a stream."""
    self.tokenizer = _RTokenizer(stream_input)
    self.context = ['LBRACE']
    self.__IgnoreNewLines = False
    self.SavedToken = None
    self.begin_token = self.tokenizer.Token('BEGIN')
    self.saved_pre_comments = None

  @property
  def IgnoreNewLines(self):
    # Strictly speaking, this method and its partner could be easily abolished
    # in favor of direct field access. However, so tortuous is R's lexing
    # process that it's very useful, at least pro tem, to have methods in this
    # connection for (temporary) journaling and debugging purposes.
    return self.__IgnoreNewLines

  @IgnoreNewLines.setter
  def IgnoreNewLines(self, value):
    self.__IgnoreNewLines = value

  def IgnoreNewLinesFromParser(self, value):
    """Switch the behavior of the lexer from the parser."""
    self.IgnoreNewLines = value

  def CurContext(self):
    """A character signifying the construct in which lexing is taking place."""
    return self.context[-1]

  def IfPush(self):
    """Record entry of the lexer into a conditional construct."""
    if self.CurContext() in ('LBRACE', '[', '(', 'i'):
      self.context.append('i')
      self.saved_new_lines = 0

  def IfPop(self):
    """Record exit from a conditional construct."""
    if self.CurContext() == 'i':
      self.context.pop()

  # Non-Google standard naming is mandated by PLY.
  # pylint: disable=invalid-name
  def token(self):
    tok = self.__token()
    return tok

  def __token(self):
    """Return the next token retrieved by the lexer."""
    # Deals with saved state from earlier scans---actual scanning is delegated
    # to ScanForToken().
    if self.begin_token:
      token, self.begin_token = self.begin_token, None
    else:
      token = self.ScanForToken()
    if token.type == 'END_OF_INPUT':
      if token.pre_comments:
        anchor = self.tokenizer.Token('SYMBOL', '__END_COMMENT_ANCHOR__')
        anchor.pre_comments = token.pre_comments
        return anchor
      return None
    token.post_comments = self.tokenizer.ProbeForComments(post=True)
    return token

  def ScanForToken(self):
    """Actually scan for the next token in the input stream."""
    while True:
      if self.SavedToken:
        tok, self.SavedToken = self.SavedToken, None
      else:
        if self.saved_pre_comments is not None:
          pre_comments = self.saved_pre_comments
          self.saved_pre_comments = None
        else:
          pre_comments = self.tokenizer.ProbeForComments(post=False)
        tok = self.tokenizer.NextToken()
        tok.pre_comments = pre_comments
      if not(tok.value == '\n' and
             (self.IgnoreNewLines or self.CurContext() in ('[', '('))):
        break
      if tok.value == '\n' and tok.pre_comments:
        self.saved_pre_comments = tok.pre_comments
    if tok.value == '\n':
      self.saved_pre_comments = tok.pre_comments
      if self.CurContext() == 'i':  # In 'if' context
        while tok.value == '\n':
          tok = self.tokenizer.NextToken()
        tok.pre_comments = self.saved_pre_comments
        self.saved_pre_comments = None
        if tok.type == 'RBRACE' or tok.type == ')' or tok.type == ']':
          while self.CurContext() == 'i':
            self.IfPop()
          self.context.pop()
          return tok
        if tok.value == ',':
          self.IfPop()
          return tok
        if tok.type == 'ELSE':
          self.IgnoreNewLines = True
          self.IfPop()
          return tok
        else:
          self.IfPop()
          self.SavedToken = tok
          return self.tokenizer.Token('CR')
      return self.tokenizer.Token('CR')
    if tok.type in ('+', '-', '*', '/', '^', 'LT', 'LE', 'GE', 'GT', 'EQ',
                    'NE', 'OR', 'AND', 'OR2', 'AND2', 'SPECIAL', 'FUNCTION',
                    'WHILE', 'REPEAT', 'FOR', 'IN', '?', '!', '-', ':',
                    '$', '@', 'LEFT_ASSIGN', 'RIGHT_ASSIGN', 'EQ_ASSIGN'):
      self.IgnoreNewLines = True
    elif tok.type == 'IF':
      self.IfPush()
      self.IgnoreNewLines = True
    elif tok.type == 'ELSE':
      self.IfPop()
      self.IgnoreNewLines = True
    elif tok.type in (';', ','):
      self.IfPop()
    elif tok.type in ('SYMBOL', 'STR_CONST', 'NUM_CONST', 'NULL_CONST', 'NEXT',
                      'BREAK'):
      self.IgnoreNewLines = False
    elif tok.type == 'LBB':
      self.context.append('[')
      self.context.append('[')
    elif tok.type == '[':
      self.context.append(tok.type)
    elif tok.type == 'LBRACE':
      self.context.append(tok.type)
      self.IgnoreNewLines = False
    elif tok.type == '(':
      self.context.append(tok.type)
    elif tok.type == ']':
      while self.CurContext() == 'i':
        self.IfPop()
      self.context.pop()
      self.IgnoreNewLines = False
    elif tok.type == 'RBRACE':
      while self.CurContext() == 'i':
        self.IfPop()
      self.context.pop()
    elif tok.type == ')':
      while self.CurContext() == 'i':
        self.IfPop()
      self.context.pop()
      self.IgnoreNewLines = False
    return tok


class NodeTypeError(base.Error):
  """Signal attempt to check for unknown parse node type."""


_known_parse_node_types = []


def ParseNode(typename, field_names, subnode_indexes=None):
  """Return a constructor for a type of parse tree node.

  Args:
    typename: a string that names the node type.
    field_names: a string consisting of a comma-separated list of fields in the
      new node type.
    subnode_indexes: a list of integers, specifying those fields of the node
      that may themselves contain parse nodes. If this argument is None, or
      no actual parameter is provided, it is assumed that all fields are
      potential parse nodes.
  Returns:
    A function that accepts as arguments values for a new node's field, and
      which returns a newly-created node of the given type.
  """

  # Monkey patched methods to check a node's type.
  def _NodeHasType(node, typename):
    """Check that a parse tree node has a given type.

    Args:
      node: a parse tree node; normally bound, as this function is used as a
        method.
      typename: a string that names a node type.

    Returns:
      Whether the type of the node is that given.

    Raises:
      NodeTypeError: the type name provided was unknown.
    """
    if typename not in _known_parse_node_types:
      raise NodeTypeError('"%s" is not a known node type' % 
                          typename)
    return typename == node.__class__.__name__

  def _NodeHasTypeIn(node, typenames):
    """Check that a parse tree node has one of a given sequence of types.

    Args:
      node: a parse tree node; normally bound, as this function is used as a
        method.
      typenames: a sequence of strings that name node types.

    Returns:
      Whether the type of the node is in the given sequence of types.

    Raises:
      NodeTypeError: one or more of the type names provided was unknown.
    """
    unknown_types = set(typenames).difference(_known_parse_node_types)
    if unknown_types:
      if len(unknown_types) == 1:
        raise NodeTypeError('"%s" is not a known node type' % 
                            unknown_types[0])
      else:
        raise NodeTypeError('%s are not known node types' % 
                            ', '.join('"%s"' % t for t in unknown_types))
    return node.__class__.__name__ in typenames

  _known_parse_node_types.append(typename)
  cls = collections.namedtuple(typename, field_names)
  if subnode_indexes is None:
    # Naughty but necessary
    # pylint: disable=protected-access
    subnode_indexes = range(len(cls._fields))
  cls.subnode_indexes = subnode_indexes
  cls.HasType = _NodeHasType
  cls.HasTypeIn = _NodeHasTypeIn
  return cls


# Parse tree node types used for R.

# The following are really classes, not constants, so we beg dispensation from
# the linter.
# pylint: disable=invalid-name
Prog = ParseNode('Prog', 'begin, exprlist')
ExprOrAssign = ParseNode('ExprOrAssign', 'expr1, eq_assign, expr2')
Comment = ParseNode('Comment', 'comment', [])
FunCall = ParseNode('FunCall', 'expr, lparen, arglist, rparen')
Unary = ParseNode('Unary', 'op, expr')
Binary = ParseNode('Binary', 'lexpr, op, rexpr')
Assign = ParseNode('Assign', 'expr1, assign, expr2')
Paren = ParseNode('Paren', 'lparen, expr_or_assign, rparen')
Brace = ParseNode('Brace', 'lbrace, exprlist, rbrace')
If = ParseNode('If', 'if_, cond, expr_or_assign')
IfElse = ParseNode('IfElse',
                   'if_, cond, expr_or_assign1, else_, expr_or_assign2')
While = ParseNode('While', 'while_, cond, expr_or_assign')
Repeat = ParseNode('Repeat', 'repeat_, expr_or_assign')
For = ParseNode('For', 'for_, forcond, expr_or_assign')
Defun = ParseNode('Defun',
                  'function, lparen, formlist, rparen, expr_or_assign')
Subscript1 = ParseNode('Subscript1', 'expr, lbrac, sublist, rbrac')
Subscript2 = ParseNode('Subscript2', 'expr, lbbrac, sublist, rbrac1, rbrac2')
Cond = ParseNode('Cond', 'lparen, expr, rparen')
ForCond = ParseNode('ForCond', 'lparen, symbol, in_, expr, rparen')
ExprList = ParseNode('ExprList', 'elements')
ExprListElt = ParseNode('ExprListElt', 'expr_or_assign, semicolon')
ArgList = ParseNode('ArgList', 'elements')
ArgListElt = ParseNode('ArgListElt', 'arg, comma')
Arg = ParseNode('Arg', 'lhs, eq_assign, rhs')
Atom = ParseNode('Atom', 'type, text, comments', [])
# pylint: enable=invalid-name


class RSyntaxError(base.Error):
  """Raised when the R formatter encounters an error during parsing."""


def _RaiseRSyntaxError(msg, line_col=None):
  if line_col:
    msg += ' at line: %d, column: %d' % line_col
  raise RSyntaxError(msg)

# PLY grammar for parsing R.
# For an overview of PLY's operation, see:
#   http://www.dabeaz.com/ply/ply.html#ply_nn22.

tokens = ('NUM_CONST', 'STR_CONST', 'SYMBOL', 'SPECIAL', 'FUNCTION', 'WHILE',
          'REPEAT', 'FOR', 'IF', 'IN', 'ELSE', 'NEXT', 'BREAK', 'LE',
          'LEFT_ASSIGN', 'RIGHT_ASSIGN', 'GE', 'NE', 'EQ', 'EQ_ASSIGN',
          'NS_GET_INT', 'NS_GET', 'AND2', 'AND', 'OR2', 'OR', 'LBB', 'BEGIN',
          '?', 'LOW', '~', 'TILDE', '!', 'UNOT', '>', '<', '+', '-', '*',
          '/', ':', 'UMINUS', 'UPLUS', '^', '$', '@', '(', '[', 'CR', 'LBRACE',
          'RBRACE', 'COMMENT')

precedence = (('left', '?'),
              ('left', 'LOW', 'WHILE', 'FOR', 'REPEAT'),
              ('right', 'IF'),
              ('left', 'ELSE'),
              ('right', 'LEFT_ASSIGN'),
              ('right', 'EQ_ASSIGN'),
              ('left', 'RIGHT_ASSIGN'),
              ('left', '~', 'TILDE'),
              ('left', 'OR'),
              ('left', 'AND'),
              ('left', 'UNOT', '!'),
              ('left', '>', 'GE', '<', 'LE', 'EQ', 'NE'),
              ('left', '+', '-'),
              ('left', '*', '/'),
              ('left', 'SPECIAL'),
              ('left', ':'),
              ('left', 'UMINUS', 'UPLUS'),
              ('right', '^'),
              ('left', '$', '@'),
              ('left', 'NS_GET', 'NS_GET_INT'),
              ('nonassoc', '(', '[', 'LBB'))


def Precedence(op):
  """The numeric precedence of a binary operator."""
  # Particularly convenient during layout of binary operators.
  return float(sum(i * (op in grp[1:])
                   for i, grp in enumerate(precedence))) / len(precedence)


def _MakeAtom(p, i):
  """Make an Atom parse tree node from the PLY parser state.

  Args:
    p: a YaccProduction containing the state of the PLY parser.
    i: the index of a terminal token in the production from which the node is
      to be constructed.
  Returns:
    A parse tree node with the token's type, value and attached comments.
  """
  tok = p.slice[i]
  return Atom(tok.type, tok.value, (tok.pre_comments, tok.post_comments))


def ParseTreeFor(stream_input, **parse_args):
  """Run the R parser on the stream provided and return a parse tree."""
  return _r_parser.parse(input=stream_input, lexer=_RLexer(), **parse_args)


# Begin PLY YACC productions.

# Disable lint checks that are incompatible with PLY's use of docstrings to
# express grammar productions.
# pylint: disable=g-docstring-quotes,g-short-docstring-punctuation
# pylint: disable=g-space-before-docstring-summary
# pylint: disable=g-no-space-after-docstring-summary
# pylint: disable=g-missing-docstring,g-doc-args,g-bad-name,g-doc-exception


def p_prog(p):
  """prog : BEGIN exprlist
  """
  p[0] = Prog(_MakeAtom(p, 1), ExprList(p[2]))


def p_expr_or_assign(p):
  """expr_or_assign : expr
                    | expr EQ_ASSIGN expr_or_assign
  """
  if len(p) == 2:
    p[0] = ExprOrAssign(None, None, p[1])
  else:
    p[0] = ExprOrAssign(p[1], _MakeAtom(p, 2), p[3])


def p_exprlist(p):
  """exprlist :
              | expr_or_assign
              | CR exprlist
              | ';' exprlist
              | expr_or_assign CR exprlist
              | expr_or_assign ';' exprlist
  """
  p.lexer.IgnoreNewLinesFromParser(False)
  if len(p) == 1:
    p[0] = []
  elif len(p) == 2:
    p[0] = [ExprListElt(p[1], None)]
  elif len(p) == 3:
    if p[1] == ';':
      p[0] = [ExprListElt(None, _MakeAtom(p, 1))] + p[2]
    elif p[-1] == 'CR':
      # Two consecutive CRs (i.e. an empty statement) result in a blank line
      p[0] = [ExprListElt(None, None)] + p[2]
    else:
      p[0] = p[2]
  else:
    e1 = ExprListElt(p[1], _MakeAtom(p, 2) if p[2] == ';' else None)
    p[0] = [e1] + p[3]


def p_expr_comment(p):
  """expr : COMMENT
  """
  p[0] = Comment(p[1])


def p_expr_atom(p):
  """expr : NEXT
          | BREAK
          | NUM_CONST
          | STR_CONST
          | SYMBOL
  """
  p[0] = _MakeAtom(p, 1)


def p_expr_function_call(p):
  """expr : expr '(' sublist ')'
  """
  p[0] = FunCall(p[1], _MakeAtom(p, 2), ArgList(p[3]), _MakeAtom(p, 4))


def p_expr_unop(p):
  """expr : '-' expr %prec UMINUS
          | '+' expr %prec UMINUS
          | '!' expr %prec UNOT
          | '~' expr %prec TILDE
          | '?' expr
  """
  p[0] = Unary(_MakeAtom(p, 1), p[2])


def p_expr_binop(p):
  """expr : expr ':' expr
          | expr NS_GET expr
          | expr NS_GET_INT expr
          | expr '$' expr
          | expr '@' expr
          | expr '+' expr
          | expr '-' expr
          | expr '*' expr
          | expr '/' expr
          | expr '^' expr
          | expr SPECIAL expr
          | expr '%' expr
          | expr '~' expr
          | expr '?' expr
          | expr '<' expr
          | expr LE expr
          | expr EQ expr
          | expr NE expr
          | expr GE expr
          | expr '>' expr
          | expr AND expr
          | expr OR expr
          | expr AND2 expr
          | expr OR2 expr
  """
  p[0] = Binary(p[1], _MakeAtom(p, 2), p[3])


def p_expr_assign(p):
  """expr : expr LEFT_ASSIGN expr
          | expr RIGHT_ASSIGN expr
  """
  p[0] = Assign(p[1], _MakeAtom(p, 2), p[3])


def p_expr_paren(p):
  """expr : '(' expr_or_assign ')'
  """
  p[0] = Paren(_MakeAtom(p, 1), p[2], _MakeAtom(p, 3))


def p_sublist(p):
  """sublist :
             | sub
             | ',' sublist
             | sub ',' sublist
  """
  if len(p) == 1:
    p[0] = [ArgListElt(None, None)]
  elif len(p) == 2:
    p[0] = [ArgListElt(p[1], None)]
  elif len(p) == 3:
    p[0] = [ArgListElt(None, _MakeAtom(p, 1))] + p[2]
  else:
    p[0] = [ArgListElt(p[1], _MakeAtom(p, 2))] + p[3]


def p_sub(p):
  """sub : expr
         | SYMBOL EQ_ASSIGN
         | SYMBOL EQ_ASSIGN expr
         | STR_CONST EQ_ASSIGN
         | STR_CONST EQ_ASSIGN expr
  """
  if len(p) == 2:
    p[0] = Arg(None, None, p[1])
    return
  p[0] = Arg(_MakeAtom(p, 1), _MakeAtom(p, 2), p[3] if len(p) == 4 else None)


def p_expr_braced_block(p):
  """expr : LBRACE exprlist RBRACE
  """
  p[0] = Brace(_MakeAtom(p, 1), ExprList(p[2]), _MakeAtom(p, 3))


def p_eatlines(p):
  """eatlines :
  """
  p.lexer.IgnoreNewLinesFromParser(True)


def p_expr_if_expr(p):
  """expr : IF '(' expr eatlines ')' expr_or_assign
  """
  p[0] = If(_MakeAtom(p, 1), Cond(_MakeAtom(p, 2), p[3], _MakeAtom(p, 5)), p[6])


def p_expr_if_else(p):
  """expr : IF '(' expr eatlines ')' expr_or_assign ELSE expr_or_assign
  """
  p[0] = IfElse(_MakeAtom(p, 1), Cond(_MakeAtom(p, 2), p[3], _MakeAtom(p, 5)),
                p[6], _MakeAtom(p, 7), p[8])


def p_expr_while(p):
  """expr : WHILE '(' expr eatlines ')' expr_or_assign
  """
  p[0] = While(_MakeAtom(p, 1), Cond(_MakeAtom(p, 2), p[3], _MakeAtom(p, 5)),
               p[6])


def p_expr_repeat(p):
  """expr : REPEAT expr_or_assign
  """
  p[0] = Repeat(_MakeAtom(p, 1), p[2])


def p_expr_for_expr(p):
  """expr : FOR '(' SYMBOL IN expr eatlines ')' expr_or_assign %prec FOR
  """
  p[0] = For(_MakeAtom(p, 1), ForCond(_MakeAtom(p, 2), _MakeAtom(p, 3),
                                      _MakeAtom(p, 4), p[5], _MakeAtom(p, 7)),
             p[8])


def p_expr_function(p):
  """expr : FUNCTION '(' formlist eatlines ')' expr_or_assign %prec LOW
  """
  p[0] = Defun(_MakeAtom(p, 1), _MakeAtom(p, 2), ArgList(p[3]), _MakeAtom(p, 5),
               p[6])


def p_formlist(p):
  """formlist :
              | form
              | form ',' formlist
  """
  if len(p) == 1:
    p[0] = [ArgListElt(None, None)]
  elif len(p) == 2:
    p[0] = [ArgListElt(p[1], None)]
  else:
    p[0] = [ArgListElt(p[1], _MakeAtom(p, 2))] + p[3]


def p_form(p):
  """form : SYMBOL
          | SYMBOL EQ_ASSIGN expr
  """
  if len(p) == 2:
    p[0] = Arg(_MakeAtom(p, 1), None, None)
    return
  p[0] = Arg(_MakeAtom(p, 1), _MakeAtom(p, 2), p[3])


def p_expr_subscript(p):
  """expr : expr '[' sublist ']'
          | expr LBB sublist ']' ']'
  """
  if len(p) == 5:
    p[0] = Subscript1(p[1], _MakeAtom(p, 2), ArgList(p[3]), _MakeAtom(p, 4))
  else:
    p[0] = Subscript2(p[1], _MakeAtom(p, 2), ArgList(p[3]), _MakeAtom(p, 4),
                      _MakeAtom(p, 5))


def p_error(p):
  if p is None:
    _RaiseRSyntaxError('Unexpected end of file')
  else:
    _RaiseRSyntaxError('Unexpected "%s"' % p.value, (p.line, p.col))

# End PLY YACC productions.

# Generate an instance of the R parser, generated from the productions above.
# Options keep PLY from generating extraneous output files.
# TODO(pyelland): Consider making the latter command line options.
_r_parser = yacc.yacc(debug=False, optimize=True, tabmodule=rparsetab,
                      write_tables=False)

