#!/usr/bin/env python
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

"""Main module for R source code formatter rfmt.

Command line help:

"""


import argparse
import cStringIO
import os
import re
import shutil
import sys

from rfmt import base, default_format_policy, r_language


# Command line argument configuration.
def str2bool(v):
  """Bodge around strange handling of boolean values in ArgumentParser."""
  return v.lower() in ('yes', 'true', 't', '1')


ap = argparse.ArgumentParser(description='R source code formatter',
                             formatter_class=argparse
                             .ArgumentDefaultsHelpFormatter)
ap.add_argument('files', help='paths of source file(s) (will be overwritten); '
                'use stdin/out if omitted.', nargs='*')
ap.add_argument('-b', '--backup', default=True, type=bool, dest='bak',
                help='backup source \'FILE\' to \'FILE.bak\'', metavar='B')
ap.add_argument('-m0', '--margin0', default=0, type=int,
                help='position of the first right margin', dest='m0')
ap.add_argument('-m1', '--margin1', default=80, type=int,
                help='position of the second right margin', dest='m1')
ap.add_argument('-c0', '--cost0', default=.05, type=float,
                help='cost (per character) beyond margin 0', dest='c0')
ap.add_argument('-c1', '--cost1', default=100, type=float,
                help='cost (per character) beyond margin 1', dest='c1')
ap.add_argument('-cb', '--costb', default=2, type=float,
                help='cost per line-break', dest='cb')
ap.add_argument('-i', '--indent', default=2, type=int,
                help='spaces per indent', dest='ind')
ap.add_argument('-fb', '--force_brace', default=False, type=str2bool,
                dest='force_brace', help='Mandate braces in flow constructs',
                metavar='FB')
ap.add_argument('-se', '--space_arg_eq', default=True, type=str2bool,
                dest='space_arg_eq', help='Mandate spaces around equals sign '
                'in argument lists', metavar='SE')
ap.add_argument('-q', '--quiet', default=False, type = str2bool,
                dest='quiet', help='Suppress progress reports, confirmations, '
                'etc.')
ap.add_argument('--adj_comment', default=.5, type=float,
                help='break cost adjustment in inline comments', metavar='ADJC')
ap.add_argument('--adj_flow', default=.3, type=float,
                help='break cost adjustment in flow constructs', metavar='ADJF')
ap.add_argument('--adj_call', default=.5, type=float,
                help='break cost adjustment in function calls', metavar='ADJCL')
ap.add_argument('--adj_arg', default=5, type=float,
                help='break cost adjustment in arguments', metavar='ADJA')
ap.add_argument('--cpack', default=1e-3, type=float,
                help='cost (per element) for packing justified layouts')


def FormatInput(f_input, options):
  """Parse and format a string or file, returning the formatted text."""
  # Parse source.
  ptree = r_language.ParseTreeFor(f_input)
  if not ptree.exprlist.elements:
    print >>sys.stderr, 'Empty file: "%s"' % options.file
    sys.exit(0)
  # Generate blocks from parse tree.
  # First, store the format policy to allow hooks to retrieve it.
  options.format_policy = default_format_policy.DefaultFormatPolicy()
  blocks = options.format_policy.BlocksFor(ptree)
  # Have generated blocks calculate optimal layout and print on
  # string buffer. (Use of string buffer facilitates trailing space
  # removal below.)
  outp = cStringIO.StringIO()
  blocks.PrintOn(outp)
  return re.sub(r' *$', '', outp.getvalue(), flags=re.MULTILINE)


def main():
  """Main command routine."""
  try:
    options = base.Options()
    init_path = os.path.expanduser(os.environ.get('RFMTRC', '~/.rfmtrc'))
    if os.path.exists(init_path):
      with open(init_path) as fp:
        ap.parse_args(' '.join(fp).split(), namespace=options)
    ap.parse_args(os.environ.get('RFMTOPTS', '').split(), namespace=options)
    ap.parse_args(namespace=options)
    if options.files:  # Input is from a list of files
      for f_path in options.files:
        bak_path = None
        try:
          if options.bak:  # Make a source backup
            bak_path = f_path + '.bak'
            shutil.copy(f_path, bak_path)
          with open(f_path, 'rU') as f_input:  # Univeral line-end conversion
            if not options.quiet:
              print >>sys.stderr, 'Formatting file "%s"' % f_path
            formatted_output = FormatInput(f_input, options)
          with open(f_path, 'w') as f_output:
            print >>f_output, formatted_output
            if not options.quiet:
              print >>sys.stderr, ('Formatting of file "%s" complete' %
                                   f_path)
        except Exception as e:
          print >>sys.stderr, e
          if f_path != options.files[-1]:
            print 'Skipping to next file...'
    else:
      print FormatInput(sys.stdin.read(), options)
  except Exception as e:  # Format-specific errors.
    print >>sys.stderr, e
    sys.exit(1)


# Conditional script stanza
if __name__ == '__main__':
  main()
