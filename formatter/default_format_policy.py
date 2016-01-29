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

"""Default R format policy based on Google style guidelines."""

from contextlib import contextmanager

from google3.third_party.R.tools.rfmt.formatter import base
from google3.third_party.R.tools.rfmt.formatter import blocks
from google3.third_party.R.tools.rfmt.formatter import format_policy
from google3.third_party.R.tools.rfmt.formatter import r_language

_options = base.Options()  # Shorthand for convenient access

# Shorthand for block constructors makes for far less onerous code.
# TODO(pyelland): Consider how convenience and conciseness in the use of
# formatting blocks might be reconciled with the Google Python style guidelines
# in a rather less ungainly manner.
VB = blocks.VerbBlock
TB = blocks.TextBlock
LB = blocks.LineBlock
IB = blocks.IndentBlock
CB = blocks.ChoiceBlock
SB = blocks.StackBlock
WB = blocks.WrapBlock

# Constant TextBlocks used in formatting.
TBSP = TB(' ')
MTTB = TB('')


class DefaultFormatPolicy(format_policy.FormatPolicy):
  """A policy based on the R formatting style prescribed at Google.

  For details of the Google R Style Guide, see:
  https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml.

  Options are provided to abrogate some aspects of the Google style guide, such
  as mandatory braces in flow constructs.
  """

  # Node types that invoke context-dependent formatting.
  PREFIX_CONSTRUCTS = ('FunCall', 'Subscript1', 'Subscript2', 'Binary',
                       'Paren', 'Brace', 'If', 'IfElse', 'While', 'Repeat',
                       'For', 'Defun')
  BRACED_CONSTRUCTS = ('Brace', 'If', 'IfElse', 'While', 'Repeat', 'For',
                       'Defun')

  # Operators formatted without surrounding spaces
  # rlint seems somewhat ambivalent wrt. spaces around '^'.
  TIGHT_BINARY_OPS = (':', 'NS_GET', 'NS_GET_INT', '$', '@')

  def __init__(self):
    super(DefaultFormatPolicy, self).__init__()
    self.in_statement = False
    self.prefix = None

  @contextmanager
  def TempAttr(self, name, value):
    """Construct a context manager that temporarily sets an attribute value.

    Args:
      name: the name of an attribute of this object to be modified temporarily.
      value: a value to which that attribute is set for the duration of any
        with statement in which the manager is used.

    Yields:
      None
    """
    old_value = getattr(self, name)
    setattr(self, name, value)
    yield
    setattr(self, name, old_value)

  def ReformatWithPrefix(self, node, prefix):
    """Reformat a node, incorporating a piece of leading formatted code.

    Args:
      node: the parse node to be reformatted.
      prefix: a LayoutBlock (generally a LineBlock) containing code (such as
        the left-hand side of an assignment) intended to be grafted onto the
        top left of the node's layout.
    Returns:
      A LayoutBlock encapsulating the revised layout, in which the prefix is
      suitably adjoined.
    """
    with self.TempAttr('prefix', prefix):
      return self.Reformat(node)

  def ReformatAsStatement(self, node):
    """Reformat a node as an R 'statement' (generally between braces).

    Args:
      node: the parse node to be reformatted.
    Returns:
      A LayoutBlock formatting the node provided.

    This method temporarily nulls out any prefix setting for the duration of
    the reformatting.
    """
    with self.TempAttr('in_statement', True), self.TempAttr('prefix', None):
      return self.Reformat(node)

  @property
  def prefix_as_list(self):
    """A singleton list containing any current prefix."""
    return [self.prefix] if self.prefix is not None else []

  def FormatFlowConstruct(self, line_elts, body_block, expr_or_assign,
                          force_brace=None):
    """Format a flow control construct, such as a for loop, etc.

    Args:
      line_elts: LayoutBlocks comprising the leading portion of the contruct.
      body_block: a LayoutBlock containing the formatted body of the construct.
      expr_or_assign: a ParseNode representing the construct body.
      force_brace: whether to ensure the body of the block is enclosed in
        braces.
    Returns:
      A LayoutBlock formatting the flow construct.
    """
    force_brace = force_brace or _options.force_brace
    line_block = LB(self.prefix_as_list + line_elts)
    if force_brace or expr_or_assign.expr2.HasType('Brace'):
      if expr_or_assign.expr2.HasType('Brace'):
        lbrace, exprlist, rbrace = self.FormatFields(expr_or_assign.expr2)
      else:
        lbrace, exprlist, rbrace = (TB('{'), [body_block], TB('}'))
      l_body = IB(SB(exprlist)) if len(exprlist) else MTTB
      # A fixed layout with braces.
      return SB([LB([line_block, TBSP, lbrace]), l_body, rbrace],
                break_mult=_options.adj_flow)
    else:
      # Choice of either a single line layout, or a stacked one.
      return CB([LB([line_block, TBSP, body_block]),
                 SB([line_block, IB(body_block)],
                    break_mult=_options.adj_flow)])

  MAX_WRAPPED_ARGS = 50

  def FormatArgList(self, list_node, block_list):
    """Format an argument list (formal or actual).

    Args:
      list_node: an ArgList node representing the argument list.
      block_list: a list of LayoutBlocks preformatting the arguments.
    Returns:
      A LayoutBlock formatting the argument list itself.
    """
    if block_list == [MTTB]:  # An empty argument list
      return MTTB
    if len(block_list) == 1:  # A singleton list is trivially formatted.
      return block_list[0]
    rhss = [elt.arg.rhs for elt in list_node.elements if elt.arg]
    # In the following, we need to allow for the possibility that the right
    # hand side of an argument list component is empty (i.e. None).
    # TODO(pyelland) Consider returning a ParseNode object of a distinguished
    # class (`EmptyParseNode', for example), rather than None, to signify
    # empty constructs.
    any_braced_arg = _options.force_brace and any(
        r.HasTypeIn(DefaultFormatPolicy.BRACED_CONSTRUCTS) for
        r in rhss if r)
    # Fixed layout for difficult cases, otherwise a WrapBlock. In the latter
    # instance, change the line break penalty to discourage breaking of the
    # parameter list.
    if len(block_list) > DefaultFormatPolicy.MAX_WRAPPED_ARGS or any_braced_arg:
      return SB(block_list)
    return WB(block_list, break_mult=_options.adj_arg)

  def FProg(self, node, begin, exprlist):
    begin_comments = node.begin.comments[1]
    if begin_comments:
      exprlist = [blocks.VerbBlock(begin_comments.lines,
                                   first_nl=False)] + exprlist
    return SB(exprlist)

  def FExprOrAssign(self, node, expr1, eq_assign, expr2):
    if expr1 is None:
      if self.in_statement and node.expr2.HasType('Binary'):
        return self.Reformat(node.expr2)
      return expr2
    # Assignments lead to 4-space hanging indents in the Google policy,
    # requiring reformatting of certain types of parse node with a prefix
    # set.
    if node.expr2.HasTypeIn(DefaultFormatPolicy.PREFIX_CONSTRUCTS):
      return self.ReformatWithPrefix(node.expr2,
                                     LB([expr1, TBSP, eq_assign, TBSP]))
    return CB([LB([expr1, TBSP, eq_assign, TBSP, expr2]),
               SB([LB([expr1, TBSP, eq_assign]),
                   IB(expr2, 2 * _options.ind)])])

  def FFunCall(self, node, expr, lparen, arglist, rparen):
    arg_box = self.FormatArgList(node.arglist, arglist)
    if self.prefix:
      # With a prefix, arguments to the function call are offset from the
      # start of the prefix, rather than the start of the call.
      line = LB([self.prefix, expr, lparen, arg_box, rparen])
      # Don't break parentheses around an empty argument list
      if arglist == [MTTB]: return line
      return CB([line,
                 SB([LB([self.prefix, expr, lparen]),
                     IB(LB([arg_box, rparen]), 2 * _options.ind)],
                    break_mult=_options.adj_arg)])
    line = LB([expr, lparen, arg_box, rparen])
    # Don't break parentheses around an empty argument list
    if arglist == [MTTB]: return line
    return CB([line,
               SB([LB([expr, lparen]),
                   IB(LB([arg_box, rparen]), 2 * _options.ind)],
                  break_mult=_options.adj_arg)])

  def FUnary(self, node, op, expr):
    return LB([op, expr])

  def FBinary(self, node, lexpr, op, rexpr):
    # Certain binary operators are formatted without surrounding spaces.
    if node.op.type in DefaultFormatPolicy.TIGHT_BINARY_OPS:
      return LB(self.prefix_as_list + [lexpr, op, rexpr])
    # Indentation of binary operators is context-sensitive, and so such nodes
    # require reformatting if there's a hanging indent, or if a binary operator
    # occurs at the top level of a statement.
    if not self.prefix and self.in_statement:
      lexpr, rexpr = self.Reformat(node.lexpr), self.Reformat(node.rexpr)
    lexpr_list = [lexpr]
    if self.prefix and node.lexpr.HasType('Binary'):
      self.prefix = self.Reformat(node.lexpr)
      lexpr_list = []
      if node.rexpr.HasType('Binary'):
        self.prefix = LB(self.prefix_as_list + [TBSP, op, TBSP])
        return self.Reformat(node.rexpr)
    lineblock_elts = self.prefix_as_list + lexpr_list + [TBSP, op]
    # The result returned below is a choice block with alternates that format
    # the operator as a single line, or broken into two lines, with the second
    # line indented if there's a hanging indent or if we're in a statement.
    # The penalty for the line break in the latter alternate is adjusted so as
    # to favor breaking after binary operators with lower precedence.
    rexpr_indent = (self.in_statement or
                    self.prefix is not None) * 2 * _options.ind
    op_break_mult = _options.adj_arg * (0.8 + 0.2 *
                                        r_language.Precedence(node.op.type))
    return CB([LB(lineblock_elts + [TBSP, rexpr]),
               SB([LB(lineblock_elts), IB(rexpr, rexpr_indent)],
                  break_mult=op_break_mult)])

  def FAssign(self, node, expr1, assign, expr2):
    # Assignments lead to 4-space hanging indents in the Google policy,
    # requiring reformatting of certain types of parse node with a prefix
    # set.
    if node.expr2.HasTypeIn(DefaultFormatPolicy.PREFIX_CONSTRUCTS):
      return self.ReformatWithPrefix(node.expr2,
                                     LB([expr1, TBSP, assign, TBSP]))
    return CB([LB([expr1, TBSP, assign, TBSP, expr2]),
               SB([LB([expr1, TBSP, assign, TBSP]),
                   IB(expr2, 2 * _options.ind)])])

  def FParen(self, node, lparen, expr_or_assign, rparen):
    ln = LB([self.prefix, lparen]) if self.prefix else lparen
    return LB([ln, expr_or_assign, rparen])

  def FBrace(self, node, lbrace, exprlist, rbrace):
    ln = LB([self.prefix, lbrace]) if self.prefix else lbrace
    return SB([ln, IB(SB(exprlist)), rbrace])

  def FIf(self, node, if_, cond, expr_or_assign):
    return self.FormatFlowConstruct([if_, TBSP, cond], expr_or_assign,
                                    node.expr_or_assign)

  def FIfElse(self, node, if_, cond, expr_or_assign1, else_, expr_or_assign2):
    # The current incarnation of the RLinter seems happiest if both arms of
    # a conditional are enclosed in braces, even if they contain only single
    # statements/expressions. This method accordingly obliges.

    def ArmHasType(expr_or_assign, types):
      return (expr_or_assign.expr1 is None and
              expr_or_assign.expr2.HasTypeIn(types))

    line_block = LB(self.prefix_as_list + [if_, TBSP, cond])
    b1 = ArmHasType(node.expr_or_assign1, ('Brace',))
    b2 = ArmHasType(node.expr_or_assign2, ('Brace',))
    if2 = ArmHasType(node.expr_or_assign2, ('If', 'IfElse'))
    if b1:
      (lbrace_if, exprlist_if,
       rbrace_if) = self.FormatFields(node.expr_or_assign1.expr2)
    elif _options.force_brace:
      b1 = True
      lbrace_if, exprlist_if, rbrace_if = TB('{'), [expr_or_assign1], TB('}')
    if b2:
      (lbrace_else, exprlist_else,
       rbrace_else) = self.FormatFields(node.expr_or_assign2.expr2)
    elif _options.force_brace:
      b2 = True
      (lbrace_else, exprlist_else,
       rbrace_else) = TB('{'), [expr_or_assign2], TB('}')
    if b1:
      if b2:  # b1 and b2
        return SB([LB([line_block, TBSP, lbrace_if]),
                   IB(SB(exprlist_if)),
                   LB([rbrace_if, TBSP, else_, TBSP, lbrace_else]),
                   IB(SB(exprlist_else)),
                   rbrace_else],
                  break_mult=_options.adj_flow)
      elif if2:  # b1 and !b2 and if2
        return SB([LB([line_block, TBSP, lbrace_if]),
                   IB(SB(exprlist_if)),
                   self.ReformatWithPrefix(node.expr_or_assign2.expr2,
                                           LB([rbrace_if, TBSP, else_, TBSP]))],
                  break_mult=_options.adj_flow)
      else:  # b1 and !b2 and !if2
        return SB([LB([line_block, TBSP, lbrace_if]),
                   IB(SB(exprlist_if)),
                   LB([rbrace_if, TBSP, else_,]),
                   IB(expr_or_assign2)],
                  break_mult=_options.adj_flow)
    else:
      if b2:  # !b1 and b2
        return SB([line_block,
                   IB(expr_or_assign1),
                   LB([else_, TBSP, lbrace_else]),
                   IB(SB(exprlist_else)),
                   rbrace_else],
                  break_mult=_options.adj_flow)
      elif if2:  # !b1 and !b2 and if2
        return SB([line_block, IB(expr_or_assign1),
                   self.ReformatWithPrefix(node.expr_or_assign2.expr2,
                                           LB([else_, TBSP]))],
                  break_mult=_options.adj_flow)
      else:  # !b1 and !b2 and !if2
        return CB([LB([line_block, TBSP, expr_or_assign1, TBSP, else_, TBSP,
                       expr_or_assign2]),
                   SB([line_block, IB(expr_or_assign1), else_,
                       IB(expr_or_assign2)])])

  def FWhile(self, node, while_, cond, expr_or_assign):
    return self.FormatFlowConstruct([while_, TBSP, cond], expr_or_assign,
                                    node.expr_or_assign)

  def FRepeat(self, node, repeat_, expr_or_assign):
    return self.FormatFlowConstruct([repeat_, TBSP], expr_or_assign,
                                    node.expr_or_assign)

  def FFor(self, node, for_, forcond, expr_or_assign):
    return self.FormatFlowConstruct([for_, TBSP, forcond], expr_or_assign,
                                    node.expr_or_assign)

  def FDefun(self, node, function, lparen, formlist, rparen, expr_or_assign):
    formlist_block = self.FormatArgList(node.formlist, formlist)
    prefix_elts = [self.prefix] * (self.prefix is not None) + [function, lparen]
    line_block = CB([LB(prefix_elts + [formlist_block, rparen]),
                     SB([LB(prefix_elts), IB(LB([formlist_block, rparen]),
                                             2 * _options.ind)])])
    if _options.force_brace or node.expr_or_assign.expr2.HasType('Brace'):
      if node.expr_or_assign.expr2.HasType('Brace'):
        lbrace, exprlist, rbrace = self.FormatFields(node.expr_or_assign.expr2)
      else:
        lbrace, exprlist, rbrace = (TB('{'), [expr_or_assign], TB('}'))
      body_block = IB(SB(exprlist)) if len(exprlist) else MTTB
      return SB([LB([line_block, TBSP, lbrace]), body_block, rbrace],
                break_mult=_options.adj_flow)
    return CB([LB([line_block, TBSP, expr_or_assign]),
               SB([line_block, IB(expr_or_assign)])])

  def FSubscript1(self, node, expr, lbrac, sublist, rbrac):
    wb = MTTB if sublist == [TBSP] else WB(sublist, break_mult=_options.adj_arg)
    if self.prefix:
      return CB([LB([self.prefix, expr, lbrac, wb, rbrac]),
                 SB([LB([self.prefix, expr, lbrac]),
                     IB(LB([wb, rbrac]), 2 * _options.ind)],
                    break_mult=_options.adj_arg)])
    return CB([LB([expr, lbrac, wb, rbrac]),
               SB([LB([expr, lbrac]),
                   IB(LB([wb, rbrac]), 2 * _options.ind)],
                  break_mult=_options.adj_arg)])

  def FSubscript2(self, node, expr, lbbrac, sublist, rbrac1, rbrac2):
    return self.FSubscript1(node, expr, lbbrac, sublist, LB([rbrac1, rbrac2]))

  def FCond(self, node, lparen, expr, rparen):
    return LB([lparen, expr, rparen])

  def FForCond(self, node, lparen, symbol, in_, expr, rparen):
    return LB([lparen, symbol, TBSP, in_, TBSP, expr, rparen])

  def FExprListElt(self, node, expr_or_assign, semicolon):
    scc = node.semicolon and node.semicolon.comments != (None, None)
    if expr_or_assign is None:
      return semicolon if scc else MTTB
    eora = self.ReformatAsStatement(node.expr_or_assign)
    return LB([eora, semicolon]) if scc else eora

  def FArgListElt(self, node, arg, comma):
    mt_arg = MTTB if arg is None else arg
    return mt_arg if comma is None else LB([mt_arg, comma])

  def FArg(self, node, lhs, eq_assign, rhs):
    eq_spc = TBSP if _options.space_arg_eq else MTTB
    if rhs is None:
      return lhs if eq_assign is None else LB([lhs, eq_spc, eq_assign])
    if lhs is None:
      return rhs
    if node.rhs.HasTypeIn(DefaultFormatPolicy.PREFIX_CONSTRUCTS):
      return self.ReformatWithPrefix(node.rhs,
                                     LB([lhs, eq_spc, eq_assign, eq_spc]))
    return LB([lhs, eq_spc, eq_assign, eq_spc, rhs])

  def FAtom(self, node, type_, text, comments):
    if text == '__END_COMMENT_ANCHOR__':
      return blocks.BlockCommentBlock(comments[0])
    tbt = TB(text)
    if comments[0]:
      bcb = blocks.BlockCommentBlock(comments[0])
      if text == '}':
        bcb = IB(bcb)
      if comments[1]:
        return SB([bcb, LB([tbt, blocks.LineCommentBlock(comments[1])])])
      else:
        return SB([bcb, tbt])
    elif comments[1]:
      return LB([tbt, blocks.LineCommentBlock(comments[1])])
    else:
      return tbt

  def BreakElementLines(self, element_lines):
    """Hook for formatting around comment-induced line breaks."""
    def StrippedLine(ln):
      # Non-destructive character removal.
      while ln and ln[0] == TBSP:
        ln = ln[1:]
      while ln and ln[-1] == TBSP:
        ln = ln[:-1]
      return LB(ln)
    ln0 = element_lines.pop(0)
    return [ln0, [IB(SB(map(StrippedLine, element_lines)), 2 * _options.ind)]]
