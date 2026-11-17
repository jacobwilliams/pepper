"""Run SpiceyPy's own test suite (test_wrapper.py of the installed SpiceyPy,
and its tests and benchmarks of cyice) against PepperyPy, and summarize:
the compatibility measure of plans/pepperypy_plan.md.

    pixi run -e python py-test-spiceypy [-- pytest arguments]

The test files are copied to a temporary directory with `spiceypy` replaced
by `pepperypy` (the kernels still come from SpiceyPy's downloader); the
benchmarks run once each, as tests (a stand-in for pytest-benchmark's
fixture). Results are
printed by kind: passed, failed for a function PepperyPy lacks
(COVERAGE.md), and other failures.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import spiceypy.tests


CONFTEST = """import pytest


class _Bench:
    # pytest-benchmark's fixture, reduced to calling the function once
    def __call__(self, f, *args, **kwargs):
        return f(*args, **kwargs)

    def pedantic(self, f, args=(), kwargs=None, setup=None, **_):
        if setup is not None:
            r = setup()
            if r is not None:
                args, kwargs = r
        return f(*args, **(kwargs or {}))


@pytest.fixture
def benchmark():
    return _Bench()
"""


def convert(text):
    text = text.replace('import spiceypy as spice', 'import pepperypy as spice')
    text = text.replace('from spiceypy import cyice', 'import pepperypy as cyice')
    text = text.replace('import spiceypy.utils.callbacks', 'import pepperypy.utils.callbacks')
    text = re.sub(r'\bspiceypy\.utils\.', 'pepperypy.utils.', text)
    return text.replace('from spiceypy.utils.support_types import', 'from pepperypy.utils.support_types import')


def main():
    root = Path(spiceypy.tests.__file__).parent.parent
    files = {'test_spiceypy_wrapper.py': root / 'tests' / 'test_wrapper.py',
             'test_spiceypy_cyice.py': root / 'tests' / 'test_cyice.py',
             'test_spiceypy_cyice_bench.py': root / 'benchmarks' / 'test_cyice.py'}
    with tempfile.TemporaryDirectory() as d:
        (Path(d) / 'conftest.py').write_text(CONFTEST)
        for name, src in files.items():
            if src.exists():
                (Path(d) / name).write_text(convert(src.read_text()))
        # one process per file: test_wrapper.py's teardown turns tracing off
        # for good (trcoff), as CSPICE does
        env = dict(os.environ, COLUMNS='400')
        out = ''
        counts = {}
        for n in files:
            if not (Path(d) / n).exists():
                continue
            cmd = [sys.executable, '-m', 'pytest', '-q', '-p', 'no:cacheprovider', '-rf', '--tb=no',
                   '--rootdir', d, str(Path(d) / n)] + sys.argv[1:]
            o = subprocess.run(cmd, capture_output=True, text=True, cwd=d, env=env).stdout
            out += o
            line = next((l for l in reversed(o.splitlines()) if ' passed' in l or ' failed' in l), '')
            for k, v in re.findall(r'(\d+) (passed|failed|skipped|errors?)', line):
                counts[v] = counts.get(v, 0) + int(k)
    fails = re.findall(r'^FAILED \S+::(\S+) - (.*)$', out, re.M)
    missing = sorted({m.group(1) for t, msg in fails for m in [re.search(r"has no attribute '(\w+)'", msg)] if m})
    other = [(t, msg) for t, msg in fails if 'has no attribute' not in msg]
    print(', '.join(f'{v} {k}' for k, v in counts.items()), f'({len(files)} files)')
    print(f'{len(fails) - len(other)} failed for a missing function ({len(missing)} functions: '
          f'{", ".join(missing)})')
    print(f'{len(other)} other failures:')
    for t, msg in other:
        print(f'  {t}: {msg[:150]}')

if __name__ == '__main__':
    main()
