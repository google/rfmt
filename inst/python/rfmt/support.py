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

"""Supporting infrastructure for the block language."""

import math

import base
options = base.Options()  # Shorthand for convenient access


# Shorthand constant, used to denote the "virtual" knot at infinity after the
# last knot explicitly specified in a Solution object (see class definition
# below).
INFINITY = float('inf')


class Console(object):
  """An object that mediates textual output of a code layout."""

  def __init__(self, outp):
    self._m0 = options.m0
    self._m1 = options.m1
    self._h_pos = 0
    self._margins = []
    self._outp = outp

  @property
  def margin(self):
    """The offset from column 0 at which output is currently printed."""
    return self._margins[-1]

  def PrintGuide(self, initial_newline=True, m0_too=True):
    """Output debugging information delimiting the console."""
    inl = '\n' if initial_newline else ''
    print >>self._outp, inl + '=' * self._m1
    if m0_too:
      print >>self._outp, inl + '-' * self._m0

  def String(self, s):
    """Write a string on the console.

    Args:
      s: the string to be written. It is assumed that s contains no
        newline characters.
    """
    self._outp.write(s)
    self._h_pos += len(s)

  def Space(self, n):
    """Write a string of spaces on the console.

    Args:
      n: the number of spaces to be written (integer).
    """
    self.String(' ' * n)

  def NewLine(self, indent=True):
    """Start a new line, optionally beginning at the current margin.

    Args:
      indent: whether to preserve the current margin after the new line.
    """
    self.String('\n')
    self._h_pos = 0
    if indent:
      self.Space(self.margin)

  def NewLineSpace(self, n):
    """Start a new line, indenting from the current margin.

    Args:
      n: the number of spaces by which to indent.
    """
    self.NewLine()
    self.Space(n)

  def PrintLayout(self, layout):
    """Print a layout on the console, pushing a new margin for the duration.

    Args:
      layout: the layout object to be printed (see below for class
        definition).
    """
    self._margins.append(self._h_pos)
    layout.PrintOn(self)
    self._margins.pop()


class PrintDescriptionConsole(object):
  """A console that produces a description of the output.

  Used primarily in generating printable representations of layout objects.
  See Console class for method descriptions.
  """

  def __init__(self):
    self.out = []

  def String(self, s):
    self.out.append(s)

  def Space(self, n):
    self.String('<spc(%s)>' % n)

  def NewLine(self, indent=True):
    self.String('<NL%s>' % ('i' * indent))

  def NewLineSpace(self, n):
    self.NewLine()
    self.Space(n)

  def PrintLayout(self, layout):
    layout.PrintOn(self)

  def Output(self):
    return ''.join(self.out)


class LayoutElement(object):
  """An element of a layout object---a directive to the console.

  This class sports a collection of static methods, each of which returns
  an anonymous function invoking a method of the console to which it is
  applied.
  Refer to the corresponding methods of the Console class for descriptions of
  the methods involved.
  """

  @staticmethod
  def String(s):
    return lambda console: console.String(s)

  @staticmethod
  def NewLine(indent=True):
    return lambda console: console.NewLine(indent)

  @staticmethod
  def NewLineSpace(n):
    return lambda console: console.NewLineSpace(n)

  @staticmethod
  def PrintLayout(layout):
    return lambda console: console.PrintLayout(layout)


class Layout(object):
  """An object containing a sequence of directives to the console."""

  def __init__(self, elements):
    self.elements = elements

  def __str__(self):
    pr_cons = PrintDescriptionConsole()
    self.PrintOn(pr_cons)
    return pr_cons.Output()

  def PrintOn(self, console):
    """Have the console execute all directives in this object."""
    for e in self.elements:
      e(console)

  def __add__(self, layout):
    """Concatenate the directives in two layouts.

    Args:
      layout: the layout whose directives are to follow this one's.
    Returns:
      A new Layout, which concatenates both.
    """
    return self.__class__(self.elements + layout.elements)

  @staticmethod
  def Stack(layouts):
    """Return the vertical composition of a sequence of layouts.

    Args:
      layouts: a sequence of Layout objects.
    Returns:
      A new Layout, stacking the arguments.
    """
    l_elts = []
    for l in layouts:
      for e in l.elements:
        l_elts.append(e)
      l_elts.append(LayoutElement.NewLine())
    return Layout(l_elts[:-1])  # Drop the last NewLine()


class Solution(object):
  """An interim solution produced during layout optimization.

  A Solution object effectively maps an integer (the left margin at which the
  solution is placed) to a layout notionally optimal for that margin,
  together with cost information used to evaluate the layout. For compactness,
  the map takes the form of a piecewise-linear cost function, with associated
  layouts.

  A Solution comprises five variables:
    knots - a list of ints, specifying the margin settings at which the layout
      changes. Note that the first knot is required to be 0.
    spans - a list of ints, giving for each knot, the width of the corresponding
      layout in characters.
    intercepts - list of floats; constant cost associated with each knot.
    gradients - list of flots; at each knot, the rate with which the layout cost
      increases with an additional margin indent of 1 character.
    layouts - the Layout objects expressing the optimal layout between
      each knot.

  In addition to these items of data, a Solution object also facilitates
  iteration through the knots and the associated spans, intercepts, etc.
  """

  def __init__(self, knots, spans, intercepts, gradients, layouts):
    self.knots = map(int, knots)
    self.spans = map(int, spans)
    self.intercepts = map(float, intercepts)
    self.gradients = map(float, gradients)
    self.layouts = layouts
    self.index = 0

  def __repr__(self):
    def KnotRepr(elts):
      k, s, a, b, l = elts
      return '%d/(%d, %.2f, %.2f, %s)' % (k, s, a, b, l.__str__())
    return '<%s>' % (', '.join(map(KnotRepr,
                                   zip(self.knots, self.spans, self.intercepts,
                                       self.gradients, self.layouts))))

  # Iteration protocol
  def Reset(self):
    """Begin iteration."""
    self.index = 0

  def Advance(self):
    """Advance to the next knot."""
    self.index += 1

  def Retreat(self):
    """Move back a knot."""
    self.index -= 1

  def CurKnot(self):
    """The currently indexed knot."""
    return self.knots[self.index]

  def CurSpan(self):
    return self.spans[self.index]

  def CurIntercept(self):
    return self.intercepts[self.index]

  def CurGradient(self):
    return self.gradients[self.index]

  def CurLayout(self):
    return self.layouts[self.index]

  def CurIndex(self):
    return self.index

  def CurValueAt(self, m):
    """The value (cost) extrapolated for margin m from the current knot."""
    # Since a Solution's cost is represented by a piecewise linear function,
    # the extrapolation in this case is linear, from the current knot.
    return self.CurIntercept() + self.CurGradient() * (m - self.CurKnot())

  def NextKnot(self):
    """The knot after the once currently indexed."""
    try:
      return self.knots[self.index + 1]
    except IndexError:
      return INFINITY

  def MoveToMargin(self, m):
    """Adjust the index so m falls between the current knot and the next."""
    if self.CurKnot() > m:
      while self.CurKnot() > m:
        self.Retreat()
    else:
      while self.NextKnot() <= m:
        self.Advance()

  def PlusConst(self, const):
    """Add a constant to all values of this Solution."""
    return self.__class__(self.knots, self.spans,
                          [a + const for a in self.intercepts],
                          self.gradients, self.layouts)

  def WithRestOfLine(self, rest_of_line):
    """Return a Solution that joins the rest of the line right of this one.

    Args:
      rest_of_line: a Solution object representing the code laid out on the
        remainder of the line, or None, if the rest of the line is empty.
    Returns:
      A new Solution object juxtaposing the layout represented by this
      Solution to the immediate right of the remainder of the line.
    """
    return self if rest_of_line is None else HPlusSolution(self, rest_of_line)


class SolutionFactory(object):
  """A factory object used to construct new Solution objects.

  The factory performs basic consistency checks, and eliminates redundant
  segments that are linear extrapolations of those that precede them.
  """

  def __init__(self):
    self.entries = []

  def Append(self, knot, span, intercept, gradient, layout):
    """Add a segment to a Solution under construction."""
    if self.entries:
      # Don't add a knot if the new segment is a linear extrapolation of
      # the last.
      k_last, s_last, i_last, g_last, _ = self.entries[-1]
      if (span == s_last and gradient == g_last and
          i_last + (knot - k_last) * g_last == intercept):
        return
    if knot < 0 or span < 0 or intercept < 0 or gradient < 0:
      raise AssertionError(('Internal error: bad layout'
                            '(k %d, s %d, i %f, g %f)') % (knot, span,
                                                           intercept, gradient))
    self.entries.append((knot, span, intercept, gradient, layout))

  def MkSolution(self):
    """Construct and return a new Solution with the data in this object."""
    return Solution(*zip(*self.entries))


def HPlusSolution(s1, s2):
  """The Solution that results from joining two Solutions side-by-side.

  Args:
    s1: Solution object
    s2: Solution object
  Returns:
    A new Solution reflecting a layout in which s2 ('s layout) is placed
    immediately to the right of s1.

  The resulting Solution object maps each prospective left margin m to the
  span, cost and layout information that would result from siting Solution s1
  at m, and then placing s2 at margin m + sp1(m), where sp1(m) is the span
  of characters occupied by the layout to which s1 maps m. In general, of
  course, both s1 and s2's layouts may occupy multiple lines, in which case
  s2's layout begins at the end of the last line of s1's layout---the span
  in this case is the span of s1's last line.
  """
  col = SolutionFactory()
  s1.Reset()
  s2.Reset()
  s1_margin = 0
  s2_margin = s1.CurSpan()
  s2.MoveToMargin(s2_margin)
  while True:
    # When forming the composite cost gradient and intercept, we must
    # eliminate the over-counting of the last line of the s1, which is
    # attributable to its projection beyond the margins.
    g1 = s1.CurGradient()
    g2 = s2.CurGradient()
    overhang0 = s2_margin - options.m0  # s2_margin = m1 + span of s1
    overhang1 = s2_margin - options.m1  # s2_margin = m1 + span of s1
    g_cur = (g1 + g2 -
             options.c0 * (overhang0 >= 0) -
             options.c1 * (overhang1 >= 0))
    i_cur = (s1.CurValueAt(s1_margin) + s2.CurValueAt(s2_margin) -
             options.c0 * max(overhang0, 0) -
             options.c1 * max(overhang1, 0))
    # The Layout computed by the following implicitly sets the margin
    # for s2 at the end of the last line printed for s1.
    col.Append(s1_margin, s1.CurSpan() + s2.CurSpan(), i_cur, g_cur,
               Layout([LayoutElement.PrintLayout(s1.CurLayout()),
                       LayoutElement.PrintLayout(s2.CurLayout())]))
    # Move to the knot closest to the margin of the corresponding
    # component.
    kn1 = s1.NextKnot()
    kn2 = s2.NextKnot()
    if kn1 == INFINITY and kn2 == INFINITY: break
    # Note in the following that one of kn1 or kn2 may be infinite.
    if kn1 - s1_margin <= kn2 - s2_margin:
      s1.Advance()
      s1_margin = kn1
      s2_margin = s1_margin + s1.CurSpan()
      # Note that s1.CurSpan() may have changed, and s2_margin may
      # decrease, so we cannot simply increment s2's index.
      s2.MoveToMargin(s2_margin)
    else:
      s2.Advance()
      s2_margin = kn2
      s1_margin = s2_margin - s1.CurSpan()
  return col.MkSolution()


def VSumSolution(solutions):
  """The layout that results from stacking several Solutions vertically.

  Args:
    solutions: a non-empty sequence of Solution objects
  Returns:
    A Solution object that lays out the solutions vertically, separated by
    newlines, with the same left margin.
  """
  if len(solutions) == 1: return solutions[0]
  col = SolutionFactory()
  for s in solutions:
    s.Reset()
  margin = 0  # Margin for all components
  while True:
    col.Append(margin, solutions[-1].CurSpan(),
               sum(s.CurValueAt(margin) for s in solutions),
               sum(s.CurGradient() for s in solutions),
               Layout.Stack(s.CurLayout() for s in solutions))
    # The distance to the closest next knot from the current margin.
    d_star = min(s.NextKnot() - margin for s in solutions
                 if s.NextKnot() > margin)  # TODO(pyelland): Redundant check?
    if d_star == INFINITY:
      break
    margin += d_star
    for s in solutions:
      s.MoveToMargin(margin)
  return col.MkSolution()


def MinSolution(solutions):
  """Form the piecewise minimum of a sequence of Solutions.

  Args:
    solutions: a non-empty sequence of Solution objects
  Returns:
    values Solution object whose cost is the piecewise minimum of the Solutions
    provided, and which associates the minimum-cost layout with each piece.
  """
  if len(solutions) == 1: return solutions[0]
  factory = SolutionFactory()
  for s in solutions:
    s.Reset()
  n = len(solutions)
  k_l = 0
  last_i_min_soln = -1  # Index of the last minimum solution
  last_index = -1  # Index of the current knot in the last minimum solution
  # Move through the intervals [k_l, k_h] defined by the glb of the partitions
  # defined by each of the solutions.
  while k_l < INFINITY:
    k_h = min(s.NextKnot() for s in solutions) - 1
    gradients = [s.CurGradient() for s in solutions]
    while True:
      values = [s.CurValueAt(k_l) for s in solutions]
      # Use the index of the corresponding solution to break ties.
      min_value, min_gradient, i_min_soln = min((values[i], gradients[i], i)
                                                for i in range(n))
      min_soln = solutions[i_min_soln]
      if i_min_soln != last_i_min_soln or min_soln.CurIndex() != last_index:
        # Add another piece to the new Solution
        factory.Append(k_l, min_soln.CurSpan(), min_value, min_gradient,
                       min_soln.CurLayout())
        last_i_min_soln = i_min_soln
        last_index = min_soln.CurIndex()
      # It's possible that within the current interval, the minimum solution
      # may change, should a solution with a lower initial value but greater
      # gradient surpass the value of one with a higher initial value but
      # lesser gradient. In such instances, we need to add an extra piece to the
      # new solution.
      distances_to_cross = [math.ceil((values[i] - min_value) /
                                      (min_gradient - gradients[i]))
                            for i in range(n) if gradients[i] < min_gradient]
      # Compute positions of all crossovers in [k_l, k_h]
      crossovers = [k_l + d for d in distances_to_cross if k_l + d <= k_h]
      if crossovers:  # Proceed to crossover in [k_l, k_h]
        k_l = min(crossovers)
      else:  # Proceed to next piece
        k_l = k_h + 1
        if k_l < INFINITY:
          for s in solutions:
            s.MoveToMargin(k_l)
        break
  return factory.MkSolution()
