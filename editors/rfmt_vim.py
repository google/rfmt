"""A very rudimentary integration of rfmt into vim.

 - Add to your .vimrc:
   let rfmt_executable = "<path-to-rfmt-executable>"
   map <C-I> :pyf <path-to-this-file>/rfmt_vim.py<cr>
   imap <C-I> <c-o>:pyf <path-to-this-file>/rfmt_vim.py<cr>
"""

import difflib
import subprocess
import vim

rfmt_cmd = vim.eval('rfmt_executable')
text = '\n'.join(vim.current.buffer)

try:
  p = subprocess.Popen(rfmt_cmd,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       stdin=subprocess.PIPE)
  stdout, stderr = p.communicate(input=text)
  if stderr:
    print stderr
  elif stdout:
    lines = stdout.split('\n')
    sequence = difflib.SequenceMatcher(None, vim.current.buffer, lines)
    for op in reversed(sequence.get_opcodes()):
      if op[0] is not 'equal':
        vim.current.buffer[op[1]:op[2]] = lines[op[3]:op[4]]
except RuntimeError as e:
  print e
