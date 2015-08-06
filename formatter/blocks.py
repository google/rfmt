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

"""The block language system for the R language formatter."""

import re
import base
import support
_options = base.Options()  # Shorthand for convenient access


class LayoutBlock(object):
  """The abstract class at base of the block hierarchy."""

  def __init__(self, is_breaking=False):
    # If a newline is mandated after this block.
    self.is_breaking = is_breaking
    # See OptLayout method below for use of layout_cache.
    self.layout_cache = {}

  def Parms(self):
    """A dictionary containing the parameters of this block."""
    return {}

  def ReprParms(self):
    """The printed representation of this block's parameters."""
    if not self.Parms(): return ''
    return ('<%s>' % (', '.join('%s=%s' % (key, val.__repr__())
                                for key, val in self.Parms().iteritems())))

  def __repr__(self):
    return re.sub('[a-z]', '', self.__class__.__name__ +
                  '*' * self.is_breaking) + self.ReprParms()

  def OptLayout(self, rest_of_line):
    """Retrieve or compute the least-cost (optimum) layout for this block.

    Args:
      rest_of_line: a Solution object representing the text to the right of
        this block.
    Returns:
      A Solution object representing the optimal layout for this block and
      the rest of the line.
    """
    # Deeply-nested choice block may result in the same continuation supplied
    # repeatedly to the same block. Without memoisation, this may result in an
    # exponential blow-up in the layout algorithm.
    if rest_of_line not in self.layout_cache:
      self.layout_cache[rest_of_line] = self.DoOptLayout(rest_of_line)
    return self.layout_cache[rest_of_line]

  def DoOptLayout(self, rest_of_line):
    """Compute the least-cost (optimum) layout for this block.

    Args:
      rest_of_line: a Solution object representing the text to the right of
        this block.
    Returns:
      A Solution object representing the optimal layout for this block and
      the rest of the line.
    """
    # Abstract method.
    pass

  def PrintOn(self, outp):
    """Print the contents of this block with the optimal layout.

    Args:
      outp: a stream on which output is to be printed.
    """
    soln = self.OptLayout(None)
    support.Console(outp).PrintLayout(soln.layouts[0])


class TextBlock(LayoutBlock):
  """A block containing a single unbroken string."""

  def __init__(self, text, is_breaking=False):
    super(TextBlock, self).__init__(is_breaking)
    self.text = text

  def __repr__(self):
    return '*' * self.is_breaking + self.text

  def DoOptLayout(self, rest_of_line):
    span = len(self.text)
    layout = support.Layout([support.LayoutElement.String(self.text)])
    # The costs associated with the layout of this block may require 1, 2 or 3
    # knots, depending on how the length of the text compares with the two
    # margins (m0 and m1) in _options. Note that we assume
    # _options.m1 >= _options.m0 >= 0, as asserted in base.Options.Check().
    if span >= _options.m1:
      s = support.Solution([0], [span],
                           [(span - _options.m0) * _options.c0 +
                            (span - _options.m1) * _options.m1],
                           [_options.c0 + _options.c1], [layout])
    elif span >= _options.m0:
      s = support.Solution([0, _options.m1 - span], [span] * 2,
                           [(span - _options.m0) * _options.c0,
                            (_options.m1 - _options.m0) * _options.c0],
                           [_options.c0, _options.c0 + _options.c1],
                           [layout] * 2)
    else:
      s = support.Solution([0, _options.m0 - span, _options.m1 - span],
                           [span] * 3,
                           [0, 0, (_options.m1 - _options.m0) * _options.c0],
                           [0, _options.c0, _options.c0 + _options.c1],
                           [layout] * 3)
    return s.WithRestOfLine(rest_of_line)


class CompositeLayoutBlock(LayoutBlock):
  """The abstract superclass of blocks which contain other blocks (elements).

  Note that we assume at least one element.
  """

  def __init__(self, elements):
    super(CompositeLayoutBlock, self).__init__()
    self.elements = elements
    # Break after this block if its last element requires a break.
    self.is_breaking = elements and elements[-1].is_breaking

  def ReprElements(self):
    return '[%s]' % (', '.join(e.__repr__() for e in self.elements))

  def __repr__(self):
    return super(CompositeLayoutBlock, self).__repr__() + self.ReprElements()


class LineBlock(CompositeLayoutBlock):
  """A block that places its elements in a single line."""

  def __init__(self, elements):
    super(LineBlock, self).__init__(elements)

  def DoOptLayout(self, rest_of_line):
    # Proceed right-to-left through the elements of this block, successively
    # adding the layout of each element to the continuation. Break at each
    # element (if any) that mandates a newline.
    broken_lines = []  # Collect the lines formed any breaks.
    for elt in self.elements[::-1]:
      if elt.is_breaking:
        if rest_of_line is not None: broken_lines.append(rest_of_line)
        rest_of_line = None
      rest_of_line = elt.OptLayout(rest_of_line)
    if not broken_lines: return rest_of_line
    broken_lines.append(rest_of_line)
    # Full layout is either a single line, or a stack comprising
    # the collection of lines.
    soln = support.VSumSolution(broken_lines[::-1])
    # Add a cost for any mandated line breaks.
    return soln.PlusConst(_options.cb * (len(broken_lines) - 1))


def IndentBlock(element, indent=None):
  """Return a block that contains another block, indented by a given amount."""
  if indent is None: indent = _options.ind
  return LineBlock([TextBlock(' ' * indent), element])


class ChoiceBlock(CompositeLayoutBlock):
  """A block which contains alternate layouts of the same content."""

  def __init__(self, elements):
    super(ChoiceBlock, self).__init__(elements)

  def DoOptLayout(self, rest_of_line):
    # The optimum layout of this block is simply the piecewise minimum of its
    # elements' layouts.
    return support.MinSolution([e.OptLayout(rest_of_line)
                                for e in self.elements])


class MultBreakBlock(CompositeLayoutBlock):
  """The abstract superclass of blocks that locally modify line break cost."""

  def __init__(self, elements, break_mult=1):
    super(MultBreakBlock, self).__init__(elements)
    self.break_mult = break_mult

  def Parms(self):
    return dict(super(MultBreakBlock, self).Parms().items() +
                [('break_mult', self.break_mult)])


class StackBlock(MultBreakBlock):
  """A block that arranges its elements vertically, separated by line breaks."""

  def __init__(self, elements, break_mult=1):
    super(StackBlock, self).__init__(elements, break_mult)

  def DoOptLayout(self, rest_of_line):
    # The optimum layout for this block arranges the elements vertically. Only
    # the final element is composed with the continuation provided---all the
    # others see an empty continuation ("None"), since they face the end of
    # a line.
    soln = support.VSumSolution([e.OptLayout(None)
                                 for e in self.elements[:-1]] +
                                [self.elements[-1].OptLayout(rest_of_line)])
    # Add the cost of the line breaks between the elements.
    return soln.PlusConst(_options.cb * self.break_mult *
                          max(len(self.elements) - 1, 0))


class WrapBlock(MultBreakBlock):
  """A block that arranges its elements like a justified paragraph."""

  def __init__(self, elements, sep=' ', break_mult=1, prefix=None):
    super(WrapBlock, self).__init__(elements)
    self.break_mult = break_mult
    self.sep = sep
    self.prefix = prefix
    self.elt_is_breaking = [e.is_breaking for e in elements]
    self.n = len(self.elements)

  def Parms(self):
    return dict(super(WrapBlock, self).Parms().items() +
                [('sep', self.sep)] +
                (self.prefix is not None) * [('prefix', self.prefix)])

  def DoOptLayout(self, rest_of_line):
    # Computing the optimum layout for this class of block involves finding the
    # optimal packing of elements into lines, a problem which we address using
    # dynamic programming.
    sep_layout = TextBlock(self.sep).OptLayout(None)
    # TODO(pyelland): Investigate why OptLayout doesn't work here.
    prefix_layout = self.prefix and TextBlock(self.prefix).DoOptLayout(None)
    elt_layouts = [e.OptLayout(None) for e in self.elements]
    # Entry i in the list wrap_solutions contains the optimum layout for the
    # last n - i elements of the block.
    wrap_solutions = [None] * self.n
    # Note that we compute the entries for wrap_solutions in reverse order,
    # at each iteration considering all the elements from i ... n - 1 (the
    # actual number of elements considered increases by one on each iteration).
    # This means that the complete solution, with elements 0 ... n - 1 is
    # computed last.
    for i in range(self.n - 1, -1, -1):
      # To calculate wrap_solutions[i], consider breaking the last n - i
      # elements after element j, for j = i ... n - 1.
      # By induction, wrap_solutions contains the optimum layout of the
      # elements after the break, so the full layout is calculated by composing
      # a line with the elements before the break with the entry from
      # wrap_solutions corresponding to the elements after the break.
      # The optimum layout to be entered into wrap_solutions[i] is then simply
      # the minimum of the full layouts calculated for each j.
      solutions_i = []
      # The layout of the elements before the break is built up incrementally
      # in line_layout.
      if prefix_layout is None:
        line_layout = elt_layouts[i]
      else:
        line_layout = prefix_layout.WithRestOfLine(elt_layouts[i])
      last_breaking = self.elements[i].is_breaking
      for j in range(i, self.n - 1):
        full_soln = support.VSumSolution([line_layout, wrap_solutions[j + 1]])
        # We adjust the cost of the full solution by adding the cost of the
        # line break we've introduced, and a small penalty (_options.cpack) to
        # favor (ceteris paribus) layouts with elements packed into earlier
        # lines.
        solutions_i.append(full_soln.PlusConst(_options.cb * self.break_mult +
                                               _options.cpack * (self.n - j)))
        # If the element at the end of the line mandates a following line break,
        # we're done.
        if last_breaking: break
        # Otherwise, add a separator and the next element to the line layout
        # and continue.
        sep_elt_layout = sep_layout.WithRestOfLine(elt_layouts[j + 1])
        line_layout = line_layout.WithRestOfLine(sep_elt_layout)
        last_breaking = self.elements[j + 1].is_breaking
      else:  # Not executed if last_breaking
        solutions_i.append(line_layout.WithRestOfLine(rest_of_line))
      wrap_solutions[i] = support.MinSolution(solutions_i)
    # Once wrap_solutions is complete, the optimum layout for the entire block
    # is the optimum layout for the last n - 0 elements.
    return wrap_solutions[0]


class VerbBlock(LayoutBlock):
  """A block that prints out several lines of text verbatim."""

  def __init__(self, text, is_breaking=True):
    super(VerbBlock, self).__init__(is_breaking)
    self.lines = text.split('\n')

  def __repr__(self):
    return self.lines[0][:3] + '...' + self.lines[-1][-3:]

  def DoOptLayout(self, rest_of_line):
    span = len(self.lines[-1])
    l_elts = []
    for ln in self.lines:
      l_elts.append(support.LayoutElement.String(ln))
      l_elts.append(support.LayoutElement.NewLine())
    layout = support.Layout(l_elts[:-1])  # Drop the last NewLine()
    # This class of block is essentially exempt from cost calculations, since
    # its formatting is fixed.
    return support.Solution([0], [span],
                            [0], [0], [layout]).WithRestOfLine(rest_of_line)


def CommentBlock(text, comment):
  """A composite block containing a text string and associated line comment."""
  comment = re.sub(r'^#+\s*', ' ', comment,
                   flags=re.MULTILINE).replace('\n', '')
  tbs = map(TextBlock, comment.split())
  if not tbs:
    cb = LineBlock([TextBlock(text), IndentBlock(TextBlock('#'))])
  else:
    cb = LineBlock([TextBlock(text),
                    IndentBlock(WrapBlock(tbs, break_mult=_options.adj_comment,
                                          prefix='# '))])
  cb.is_breaking = True
  return cb
