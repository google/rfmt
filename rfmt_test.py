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

"""Tests for 'rfmt' R source formatting tool."""

import cStringIO
import re
import unittest

import rfmt_test_data
from formatter import base
from formatter import blocks
from formatter import google_format
from formatter import r_language
from formatter import support


class RfmtTest(unittest.TestCase):

  TEST_OPTIONS = {'m0': 0, 'm1': 80, 'c0': .05, 'c1': 100, 'cb': 2, 'ind': 2,
                  'adj_comment': .25, 'adj_flow': .3, 'adj_call': .25,
                  'adj_arg': 2, 'cpack': 1e-3}

  def setUp(self):
    base.Options().__dict__.update(RfmtTest.TEST_OPTIONS)

  def test_blocks(self):
    tbs = [blocks.TextBlock('B' + str(i)) for i in range(10)]
    lb = blocks.LineBlock(tbs)
    sb = blocks.StackBlock(tbs)
    cb = blocks.ChoiceBlock([lb, blocks.IndentBlock(sb)])
    wb = blocks.WrapBlock(tbs)
    self.assertEqual(str(tbs[0]), 'B0')
    self.assertEqual(str(lb), 'LB[B0, B1, B2, B3, B4, B5, B6, B7, B8, B9]')
    self.assertEqual(str(sb),
                     'SB<break_mult=1>[B0, B1, B2, B3, B4, B5, B6, B7, B8, B9]')
    ib_str = 'LB[  , %s]' % str(sb)
    self.assertEqual(str(cb), 'CB[%s, %s]' % (str(lb), ib_str))
    # Darn collation sequences differ across test machines...
    wb_strs = ('WB<%s>[B0, B1, B2, B3, B4, B5, B6, B7, B8, B9]' % args for
               args in ('sep=\' \', break_mult=1', 'break_mult=1, sep=\' \''))
    self.assertTrue(str(wb) in wb_strs)

  def test_simpleLayout(self):
    tb1 = blocks.TextBlock('A')
    tb2 = blocks.TextBlock('B')
    ly1 = tb1.OptLayout(None)
    ly2 = tb2.OptLayout(None)
    self.assertEqual(str(ly1),
                     '<0/(1, 0.05, 0.05, A), 79/(1, 4.00, 100.05, A)>')
    self.assertEqual(str(ly2),
                     '<0/(1, 0.05, 0.05, B), 79/(1, 4.00, 100.05, B)>')
    self.assertEqual(str(tb1.OptLayout(ly2)), '<0/(2, 0.10, 0.05, AB), '
                     '78/(2, 4.00, 100.05, AB), 79/(2, 104.05, 100.05, AB)>')
    ms = support.MinSolution([ly1, support.VSumSolution([ly1, ly2])])
    self.assertEqual(str(ms),
                     '<0/(1, 0.05, 0.05, A), 79/(1, 4.00, 100.05, A)>')

  def test_complexLayout(self):
    tbs = [blocks.TextBlock(('B' + str(i)) * (i + 1)) for i in range(5)]
    saved_m1 = base.Options().m1
    setattr(base.Options(), 'm1', 20)
    wb_ly = blocks.WrapBlock(tbs).OptLayout(None)
    ly_req = ('<0/(19, 3.65, 0.10, B0 B1B1 B2B2B2<NL>B3'
              'B3B3B3 B4B4B4B4B4), 1/(19, 3.75, 100.10,'
              ' B0 B1B1 B2B2B2<NL>B3B3B3B3 B4B4B4B4B4),'
              ' 2/(10, 5.90, 0.15, B0 B1B1 B2B2B2<NL>B3'
              'B3B3B3<NL>B4B4B4B4B4), 6/(10, 6.50, 100.'
              '15, B0 B1B1 B2B2B2<NL>B3B3B3B3<NL>B4B4B4'
              'B4B4), 7/(10, 8.96, 0.20, B0 B1B1<NL>B2B'
              '2B2<NL>B3B3B3B3<NL>B4B4B4B4B4), 10/(10, '
              '9.56, 100.20, B0 B1B1<NL>B2B2B2<NL>B3B3B'
              '3B3<NL>B4B4B4B4B4), 12/(10, 209.96, 200.'
              '20, B0 B1B1<NL>B2B2B2<NL>B3B3B3B3<NL>B4B'
              '4B4B4B4), 13/(10, 410.16, 300.20, B0 B1B'
              '1<NL>B2B2B2<NL>B3B3B3B3<NL>B4B4B4B4B4), '
              '14/(10, 613.01, 300.25, B0<NL>B1B1<NL>B2'
              'B2B2<NL>B3B3B3B3<NL>B4B4B4B4B4), 16/(10,'
              ' 1213.51, 400.25, B0<NL>B1B1<NL>B2B2B2<N'
              'L>B3B3B3B3<NL>B4B4B4B4B4), 18/(10, 2014.'
              '01, 500.25, B0<NL>B1B1<NL>B2B2B2<NL>B3B3'
              'B3B3<NL>B4B4B4B4B4), 21/(34, 3502.75, 10'
              '0.05, B0 B1B1 B2B2B2 B3B3B3B3 B4B4B4B4B4'
              ')>')
    self.assertEqual(str(wb_ly), ly_req)
    setattr(base.Options(), 'm1', saved_m1)

  def test_lexer(self):
    r_src = 'if (1 > 2) 3 else 4 # Comment'
    lex = r_language._RLexer()
    lex.input(r_src)
    tokens = []
    while True:
      tok = lex.token()
      if not tok: break
      tokens.append(str(tok))
    tok_req = ['(BEGIN, BEGIN)@(1,0)', '(IF, if)@(1,2)', '((, ()@(1,4)',
               '(NUM_CONST, 1)@(1,5)', '(>, >)@(1,7)', '(NUM_CONST, 2)@(1,9)',
               '(), ))@(1,10)', '(NUM_CONST, 3)@(1,12)', '(ELSE, else)@(1,17)',
               '(NUM_CONST, 4, # [\' Comment\'])@(1,19)']
    self.assertEqual(tokens, tok_req)

  def test_parser(self):
    r_src = ('while (TRUE) {'
             '  for (i in 1:10)'
             '    x <- 3 * i'
             '}')
    ptree = r_language.ParseTreeFor(r_src)
    ptree_req = ('Prog(begin=Atom(type=\'BEGIN\', text=\'B'
                 'EGIN\', comments=None), exprlist=ExprLis'
                 't(elements=[ExprListElt(expr_or_assign=E'
                 'xprOrAssign(expr1=None, eq_assign=None, '
                 'expr2=While(while_=Atom(type=\'WHILE\', '
                 'text=\'while\', comments=None), cond=Con'
                 'd(lparen=Atom(type=\'(\', text=\'(\', co'
                 'mments=None), expr=Atom(type=\'SYMBOL\','
                 ' text=\'TRUE\', comments=None), rparen=A'
                 'tom(type=\')\', text=\')\', comments=Non'
                 'e)), expr_or_assign=ExprOrAssign(expr1=N'
                 'one, eq_assign=None, expr2=Brace(lbrace='
                 'Atom(type=\'LBRACE\', text=\'{\', commen'
                 'ts=None), exprlist=ExprList(elements=[Ex'
                 'prListElt(expr_or_assign=ExprOrAssign(ex'
                 'pr1=None, eq_assign=None, expr2=For(for_'
                 '=Atom(type=\'FOR\', text=\'for\', commen'
                 'ts=None), forcond=ForCond(lparen=Atom(ty'
                 'pe=\'(\', text=\'(\', comments=None), sy'
                 'mbol=Atom(type=\'SYMBOL\', text=\'i\', c'
                 'omments=None), in_=Atom(type=\'IN\', tex'
                 't=\'in\', comments=None), expr=Binary(le'
                 'xpr=Atom(type=\'NUM_CONST\', text=\'1\','
                 ' comments=None), op=Atom(type=\':\', tex'
                 't=\':\', comments=None), rexpr=Atom(type'
                 '=\'NUM_CONST\', text=\'10\', comments=No'
                 'ne)), rparen=Atom(type=\')\', text=\')\''
                 ', comments=None)), expr_or_assign=ExprOr'
                 'Assign(expr1=None, eq_assign=None, expr2'
                 '=Assign(expr1=Atom(type=\'SYMBOL\', text'
                 '=\'x\', comments=None), assign=Atom(type'
                 '=\'LEFT_ASSIGN\', text=\'<-\', comments='
                 'None), expr2=Binary(lexpr=Atom(type=\'NU'
                 'M_CONST\', text=\'3\', comments=None), o'
                 'p=Atom(type=\'*\', text=\'*\', comments='
                 'None), rexpr=Atom(type=\'SYMBOL\', text='
                 '\'i\', comments=None)))))), semicolon=No'
                 'ne)]), rbrace=Atom(type=\'RBRACE\', text'
                 '=\'}\', comments=None))))), semicolon=No'
                 'ne)]))')
    self.assertEqual(str(ptree), ptree_req)

  def test_formatter(self):
    ptree = r_language.ParseTreeFor(rfmt_test_data.UNFORMATTED_SRC)
    blks = google_format.GoogleFormatPolicy().BlocksFor(ptree)
    outp = cStringIO.StringIO()
    blks.PrintOn(outp)
    format_result = re.sub(r' *$', '', outp.getvalue(), flags=re.MULTILINE)
    self.assertEqual(format_result, rfmt_test_data.FORMATTED_SRC)


if __name__ == '__main__':
  unittest.main()
