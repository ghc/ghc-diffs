"""
Utilities for linters
"""

import textwrap

def lint_failure(file, line_no, line_content, message):
    body = '\n'.join("  " + line for line in textwrap.wrap(message))
    msg = '''
    {file}:

           |
    {line_no:5d}  |  {line_content}
           |

    {body}
    '''.format(file=file, line_no=line_no,
               line_content=line_content,
               body=body)

    print(textwrap.dedent(msg))
