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

"""Base functionality for the R formatter.

- Defines formatter-specific exception class.
- Access to and manipulation of tool options.
"""


class Error(Exception):
  """Base class for R formatter exceptions."""

  def __init__(self, msg):
    super(Error, self).__init__()
    self.msg = msg

  def __str__(self):
    return self.msg


class Options(object):
  """Singleton class holding global options for the formatter."""

  _instance = None

  def __new__(cls):
    if Options._instance is None:
      Options._instance = super(Options, cls).__new__(cls)
    return cls._instance

  def Set(self, flags=None, **opts):
    """Set the options used throughout the rfmt system.

    Args:
      flags: an optional :-) Google-style (pyglib) FLAGS object.
      **opts: other keyword options.
    """
    if flags:
      self.__dict__.update(flags.FlagValuesDict())
    self.__dict__.update(opts)
    self.Check()

  def Check(self):
    """Assertion verification for options."""
    try:
      assert self.m0 >= 0, "margin0"
      assert self.m1 >= self.m0, "margin1"
      assert self.c0 >= 0, "cost0"
      assert self.c1 >= 0, "cost1"
      assert self.cb >= 0, "costb"
      assert self.ind >= 0, "indent"
      assert self.adj_comment >= 0, "adj_comment"
      assert self.adj_flow >= 0, "adj_flow"
      assert self.adj_call >= 0, "adj_call"
      assert self.adj_arg >= 0, "adj_arg"
      assert self.cpack >= 0, "cpack"
    except AssertionError as e:
      raise Error("Illegal option value for '%s'" % e.args[0])

