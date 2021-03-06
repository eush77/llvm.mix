"""Processes a foo.h.cmake file and writes foo.h.

Takes a list of KEY=VALUE pairs (where VALUE can be empty).

Handles these types of lines (note that FOO= sets the value of FOO to the empty
string, which is falsy, but FOO=0 sets it to '0' which is truthy):

1.) #cmakedefine01 FOO
    Checks if key FOO is set to a truthy value, and depending on that prints
    one of the following two lines:

        #define FOO 1
        #define FOO 0

2.) #cmakedefine FOO
    Checks if key FOO is set to a truthy in value, and depending on that prints
    one of the following two lines:

        #define FOO
        /* #undef FOO */

3.) #cmakedefine FOO asdf${BAR}jkl
    Checks if key FOO is set to a truthy values, and if so replaces all
    variable references in `asdf${BAR}jkl` with their value and prints that
    (else it prints the same undef line as the previous form):

        #define FOO asdfBAR_VALjkl
        /* #undef FOO */

4.) #define FOO asdf{BAR}jkl
    Always gets its variable values filled in, independent of FOO's value being
    set:

        #define FOO asdfBAR_VALjkl

Fails if any of the KEY=VALUE arguments aren't needed for processing the
.h.cmake file, or if the .h.cmake file has unreplaces ${VAR} references after
processing all values.
"""

import argparse
import os
import re
import sys


def main():
    parser = argparse.ArgumentParser(
                 epilog=__doc__,
                 formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('input', help='input file')
    parser.add_argument('values', nargs='*', help='several KEY=VALUE pairs')
    parser.add_argument('-o', '--output', required=True,
                        help='output file')
    args = parser.parse_args()

    values = {}
    for value in args.values:
        key, val = value.split('=', 1)
        values[key] = val
    unused_values = set(values.keys())

    # Matches e.g. '${CLANG_FOO}' and captures CLANG_FOO in group 1.
    var_re = re.compile(r'\$\{([^}]*)\}')

    in_lines = open(args.input).readlines()
    out_lines = []
    for in_line in in_lines:
        def repl(m):
            unused_values.discard(m.group(1))
            return values[m.group(1)]
        if in_line.startswith('#cmakedefine01 '):
            _, var = in_line.split()
            out_lines.append('#define %s %d\n' % (var, 1 if values[var] else 0))
            unused_values.discard(var)
        elif in_line.startswith('#cmakedefine '):
            _, var = in_line.split(None, 1)
            try:
                var, val = var.split(None, 1)
            except:
                var, val = var.rstrip(), '\n'
            if values[var]:
                out_lines.append('#define %s %s' % (var,
                                                    var_re.sub(repl, val)))
            else:
                out_lines.append('/* #undef %s */\n' % var)
            unused_values.discard(var)
        else:
            # In particular, handles `#define FOO ${FOO}` lines.
            out_lines.append(var_re.sub(repl, in_line))

    if unused_values:
        print >>sys.stderr, 'Unused --values args:'
        print >>sys.stderr, '    ', '\n    '.join(unused_values)
        return 1

    output = ''.join(out_lines)

    leftovers = var_re.findall(output)
    if leftovers:
        print >>sys.stderr, 'unprocessed values:\n', '\n'.join(leftovers)
        return 1

    if not os.path.exists(args.output) or open(args.output).read() != output:
        open(args.output, 'w').write(output)


if __name__ == '__main__':
    sys.exit(main())
