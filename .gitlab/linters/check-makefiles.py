#!/usr/bin/env python3

"""
Warn for use of `--interactive` inside Makefiles (#11468).

Encourage the use of `$(TEST_HC_OPTS_INTERACTIVE)` instead of
`$(TEST_HC_OPTS) --interactive -ignore-dot-ghci -v0`. It's too easy to
forget one of those flags when adding a new test.
"""

import sys
import os
import json
import re
import subprocess
import textwrap

failed = 0
base_commit = sys.argv[1]
head_commit = sys.argv[2]

files = subprocess.check_output(['git', 'diff', '--name-only', base_commit, head_commit, '--', './testsuite'], encoding='UTF-8')
for path in files.split('\n'):
    if os.path.isfile(path):
        with open(path) as f:
            for lineno, line in enumerate(f):
                if '--interactive' in line:
                    failed = 1
                    msg = '''
                    {path}:

                           |
                    {lineno:5d}  |  {line}
                           |

                       Warning: Use `$(TEST_HC_OPTS_INTERACTIVE)` instead of
                               `--interactive -ignore-dot-ghci -v0`.
                    '''.format(path=path, lineno=lineno, line=line[:-1])

                    print(textwrap.dedent(msg))

sys.exit(failed)
