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

"""Specification of R formatter protocol."""

import abc


class FormatPolicy(object):
  """Abstract class specifying protocol implemented by an R formatter.

  For each type of parse tree node, <node-type>, formatters---qua subclasses of
  this class---implement a method 'F<node-type>'. Each such method is
  presented with arguments comprising 1) an argument 'node', which is the node
  itself, and 2) formatted versions of the node's fields, with argument names
  corresponding to the names of the fields themselves. The method should return
  a LayoutBlock that realizes the appropriate formatting for the node.

  Generally speaking, a parse tree is formatted from the leaves up, with the
  appropriate formatting methods called on the fields of a node before the
  results are provided (along with the node itself) to the formatting method
  for the node. Some policies, however, may choose to format nodes in a
  context-dependent manner (the right-hand side of an assignment, for example,
  might be formatted differently from the identical expression that appears as
  a statement by itself). In such instances, information about the context may
  be (temporarily) stored in the policy object, before some or all of the
  node's fields are formatted again. By default, the results of formatting a
  parse node are memoised by this superclass, so use the method Reformat() to
  effect the reformatting of a node's field. See the class GoogleFormatPolicy
  (in package google_format) for examples.

  This kind of bottom-up/top-down formatting does result in some redundant
  computation, in as far as the formatted versions of the fields passed into
  a formatting method may be discarded. However, memoisation (which ensures
  that the grandchildren of a reformatted node are by default not themselves
  reformatted) keeps such redundant computation to a minimum. Furthermore,
  considerable convenience is afforded writers of FormatPolicy (sub)classes by
  preformatting a node's fields rather than having policy writer format them
  explicitly.
  """

  __metaclass__ = abc.ABCMeta  # Allow decoration of abstract methods

  def BlocksFor(self, parse_tree):
    """Return LayoutBlocks for a parse tree.

    Args:
      parse_tree: the node at the root of the parse tree in question.
    Returns:
      A LayoutBlock at the root of nested collection of such blocks that
      yields the layout of the code represented in the parse tree.
    """
    return self.Format(parse_tree)

  @abc.abstractmethod
  def __init__(self):
    self._fmt_dict = {}

  @abc.abstractmethod
  def FProg(self, node, begin, exprlist):
    pass

  @abc.abstractmethod
  def FExprOrAssign(self, node, expr1, eq_assign, expr2):
    pass

  @abc.abstractmethod
  def FFunCall(self, node, expr, lparen, sublist, rparen):
    pass

  @abc.abstractmethod
  def FUnary(self, node, op, expr):
    pass

  @abc.abstractmethod
  def FBinary(self, node, lexpr, op, rexpr):
    pass

  @abc.abstractmethod
  def FAssign(self, node, expr1, assign, expr2):
    pass

  @abc.abstractmethod
  def FParen(self, node, lparen, expr_or_assign, rparen):
    pass

  @abc.abstractmethod
  def FBrace(self, node, lbrace, exprlist, rbrace):
    pass

  @abc.abstractmethod
  def FIf(self, node, if_, cond, expr_or_assign):
    pass

  @abc.abstractmethod
  def FIfElse(self, node, if_, cond, expr_or_assign1, else_, expr_or_assign2):
    pass

  @abc.abstractmethod
  def FWhile(self, node, while_, cond, expr_or_assign):
    pass

  @abc.abstractmethod
  def FRepeat(self, node, repeat_, expr_or_assign):
    pass

  @abc.abstractmethod
  def FFor(self, node, for_, forcond, expr_or_assign):
    pass

  @abc.abstractmethod
  def FDefun(self, node, function, lparen, formlist, rparen, expr_or_assign):
    pass

  @abc.abstractmethod
  def FSubscript1(self, node, expr, lbrac, sublist, rbrac):
    pass

  @abc.abstractmethod
  def FSubscript2(self, node, expr, lbbrac, sublist, rbrac1, rbrac2):
    pass

  @abc.abstractmethod
  def FCond(self, node, lparen, expr, rparen):
    pass

  @abc.abstractmethod
  def FForCond(self, node, lparen, symbol, in_, expr, rparen):
    pass

  # Argument 'node' is present in the following so as to conform with the
  # calling protocol applied to all formatting methods; inform the linter.
  def FExprList(self, unused_node, elements):
    return elements  # Already a list of layout blocks

  @abc.abstractmethod
  def FExprListElt(self, node, expr_or_assign, semicolon):
    pass

  # Argument 'node' is present in the following so as to conform with the
  # calling protocol applied to all formatting methods; inform the linter.
  def FArgList(self, unused_node, elements):
    return elements  # Already a list of layout blocks

  @abc.abstractmethod
  def FArgListElt(self, node, arg, comma):
    pass

  @abc.abstractmethod
  def FArg(self, node, lhs, eq_assign, rhs):
    pass

  @abc.abstractmethod
  def FAtom(self, node, type_, text, comments):
    pass

  def BreakElementLines(self, element_lines):
    """Hook for formatting around comment-induced line breaks."""
    return element_lines

  def FormatFields(self, node):
    """Format the fields of a node, returning the results in a list."""
    # As with Format(), the results of this method are cached.
    # The following assumes that id(node) is never 0---a not unreasonable
    # assumption in CPython, at least.
    if -id(node) not in self._fmt_dict:
      fields = list(node)
      for i in node.subnode_indexes:  # Fields that may be parse nodes
        field_i = fields[i]
        if field_i is not None:
          if isinstance(field_i, list):
            fields[i] = map(self.Format, field_i)
          else:
            fields[i] = self.Format(field_i)
      self._fmt_dict[-id(node)] = fields
    return self._fmt_dict[-id(node)]

  def Format(self, node):
    """Format a general parse node.

    Args:
      node: a parse tree node (c.f. ParseNode in module r_language).
    Returns:
      A (n instance of a subclass of) LayoutBox which will yield the format
      layout of the node provided.

    The results of this method are memoised, so use the Reformat() method to
    recalculate formatting.
    """
    if id(node) not in self._fmt_dict:
      fmt_elts = self.FormatFields(node)
      format_method = getattr(self, 'F' + type(node).__name__)
      self._fmt_dict[id(node)] = format_method(node, *fmt_elts)
    return self._fmt_dict[id(node)]

  def Reformat(self, node):
    """Calculate formatting for a parse node, even if it's been memoised."""
    self._fmt_dict.pop(id(node), None)  # Ignore nodes that aren't cached.
    self._fmt_dict.pop(-id(node), None)
    return self.Format(node)
