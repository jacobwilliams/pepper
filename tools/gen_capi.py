#!/usr/bin/env python3
"""Generate PEPPER's C interface and PepperyPy's wrappers from the Fortran
interfaces (plans/pepperypy_plan.md).

Reads:
  src/pepper_instance_module.f90   the context's procedures (their interfaces) and
                            the instance's (spice_*)
  src/pepper_math.f90       the pure math procedures
  src/pepper_constants.f90  the constants (dpr, rpd, ...)

Writes:
  src/capi/pepper_capi.f90              the bind(c) shims (module pepper_capi)
  include/pepper.h                      the C declarations (with the core's)
  python/src/pepperypy/_pepper.pxd      the Cython declarations
  python/src/pepperypy/_context_methods.pxi   methods of Context
  python/src/pepperypy/_spice_methods.pxi     methods of Spice
  python/src/pepperypy/_functions.pxi   the module-level functions
  python/COVERAGE.md                    SpiceyPy's functions against PepperyPy's

A procedure is wrapped when all its arguments are of a kind the generator
knows: scalars; strings in, out and in/out; arrays of fixed size (also
named sizes: planes, ellipses) or sized by integer arguments; assumed-size
arrays in, in/out, and out (sized by OUT_SIZES); arrays of strings and of
logicals; workspace (WORK, made by the wrapper); assumed-shape arrays in;
allocatable results; the GF step, refinement, report and interrupt
routines (the context's hooks, set from Python). The others are listed in
COVERAGE.md; a few have hand-written wrappers (the core,
python/src/pepperypy/_compat.py). String arguments that CSPICE checks
(tools/cspice_empty_strings.json) raise SPICE(EMPTYSTRING) when empty.

The Python signatures follow SpiceyPy's where SpiceyPy has the function
(it is imported to read them): its parameter names, and its extra
parameters (lenout, room, ...) accepted and ignored.

Run in the python environment: pixi run -e python gen-capi
"""

import inspect
import keyword
import math
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# Context procedures with hand-written wrappers or not wrapped.
EXCLUDE_CTX = {
    'new_context', 'context', 'failed', 'sigerr', 'setmsg', 'errch', 'errdp', 'errint',
    'errfnm', 'errhan', 'reset', 'getmsg', 'message', 'erract', 'errdev', 'errprt',
    'return', 'outmsg', 'seterr', 'accept', 'allowd', 'getsms', 'chkin', 'chkout',
    'trcoff', 'trcdep', 'trcmxd', 'trcnam', 'qcktrc', 'getlms', 'putlms', 'putsms',
    'prepare_state', 'query_state', 'prepare_rotation', 'query_rotation', 'query_transform',
    'gf_user_scalar', 'gf_user_boolean', 'uddc', 'instance',
}
# Instance procedures with hand-written wrappers.
EXCLUDE_SPICE = {'furnsh', 'pcpool', 'pdpool', 'pipool', 'lmpool', 'add_ephemeris', 'add_frame',
                 'destroy',  # (destroy: the lifetime is pepper_spice_free's, Spice.close's)
                 'load_kernel',  # hand-written (the builder: pepper_builder_*, KernelBuilder)
                 'write_kernel',  # hand-written (its optional ck_file and text_file)
                 'remove_loader'}  # kernel loaders are Fortran types: not registered from C or Python
# Private routines SpiceyPy exports.
ZZ_WRAPPED = set()   # (zzdynrot: pepper_zzdynrot, hand-written)
# Shims generated, Python wrappers written by hand (_core.pyx).
NO_PYTHON = {'kclear', 'remove_ephemeris', 'remove_frame'}

# SpiceyPy's parameters that size outputs in C (accepted, ignored).
SIZE_PARAMS = {'lenout', 'lenvout', 'room', 'size', 'maxn', 'nintvls', 'nintvl', 'lenvals',
               'namlen', 'lenkind', 'maxval', 'maxwin', 'filen', 'typlen', 'srclen',
               'mxvrts', 'mxpart', 'buflen', 'maxrsz', 'lendesc', 'maxlen'}

STR_OUT_SIZE = 1024   # buffer of a string result (characters, with the NUL)

# CSPICE (so SpiceyPy) counts these from 0 where SPICELIB counts from 1: the
# inputs are increased by one, the outputs decreased by one.
ZERO_BASED_IN = {
    'kdata': ['which'], 'wnfetd': ['n'], 'eknelt': ['selidx', 'row'],
    'ekgc': ['selidx', 'row', 'elment'], 'ekgd': ['selidx', 'row', 'elment'],
    'ekgi': ['selidx', 'row', 'elment'], 'ektnam': ['n'], 'ekcii': ['cindex'],
    'cpos': ['start'], 'cposr': ['start'], 'ncpos': ['start'], 'ncposr': ['start'],
    'pos': ['start'], 'posr': ['start'],
    'lx4dec': ['first'], 'lx4num': ['first'], 'lx4sgn': ['first'], 'lx4uns': ['first'], 'lxqstr': ['first'],
    'dasadc': ['bpos', 'epos'], 'dasudc': ['bpos', 'epos'], 'dasrdc': ['bpos', 'epos'],
    'stpool': ['nth'],
    'dskd02': ['start'], 'dski02': ['start'],
}
# CSPICE numbers EK segments and records from 0.
EK_INDICES = {'segno', 'recno'}
# Arrays of indices counted from 0 by CSPICE: each element increased by one.
ZERO_BASED_ARRAY_IN = {'bschoc': ['order'], 'bschoi': ['order'], 'bschod': ['order']}
ZERO_BASED_OUT = {
    'cpos': ['fresult'], 'cposr': ['fresult'], 'ncpos': ['fresult'], 'ncposr': ['fresult'],
    'pos': ['fresult'], 'posr': ['fresult'], 'isrchc': ['fresult'], 'isrchd': ['fresult'],
    'isrchi': ['fresult'], 'bsrchc': ['fresult'], 'bsrchd': ['fresult'], 'bsrchi': ['fresult'],
    'esrchc': ['fresult'], 'lstlec': ['fresult'], 'lstled': ['fresult'], 'lstlei': ['fresult'],
    'lstltc': ['fresult'], 'lstltd': ['fresult'], 'lstlti': ['fresult'], 'ordc': ['fresult'],
    'ordd': ['fresult'], 'ordi': ['fresult'], 'lastnb': ['fresult'], 'frstnb': ['fresult'],
    'wnsumd': ['idxsml', 'idxlon'],
    'lx4dec': ['last'], 'lx4num': ['last'], 'lx4sgn': ['last'], 'lx4uns': ['last'], 'lxqstr': ['last'],
    'bschoc': ['fresult'], 'bschoi': ['fresult'], 'bschod': ['fresult'],
}
# Arrays of vectors in the Fortran (k, n) layout that CSPICE's callers write
# as rows (n, k), although their dimensions are fixed (else a fixed k x k
# array is a matrix).
ROWS = {'pltexp': ['iverts', 'overts'], 'dskb02': ['vtxbds']}
# Outputs SpiceyPy does not return.
DROP_OUTPUTS = {'dp2hx': ['hxssiz'], 'dskd02': ['n'], 'dski02': ['n'], 'dskp02': ['n'], 'dskv02': ['n'],
                'scpart': ['nparts'], 'ekffld': ['rcptrs']}
# The sizes of assumed-size results ({x}: the value of the integer input x).
OUT_SIZES = {
    'dafus': {'dc': '{nd}', 'ic': '{ni}'},
    'dafps': {'sum': '{nd} + ({ni} + 1) // 2'},
    'dafgda': {'data': '{eaddr} - {baddr} + 1'},
    'dafrda': {'data': '{end} - {begin} + 1'},
    'dafgsr': {'data': '{end} - {begin} + 1'},
    'dasrdd': {'data': '{last} - {first} + 1'},
    'dasrdi': {'data': '{last} - {first} + 1'},
    'ckgr02': {'record': '10'},
    'ckgr03': {'record': '8'},
    'dafgs': {'sum': '128'},     # the largest summary (_compat.dafgs takes SpiceyPy's n)
    'dlabbs': {'dladsc': '8'}, 'dlabfs': {'dladsc': '8'}, 'dlafns': {'nxtdsc': '8'}, 'dlafps': {'prvdsc': '8'},
    'dskgd': {'dskdsc': '24'},
    'dskd02': {'values': '{room}'}, 'dski02': {'values': '{room}'},
    'dskp02': {'plates': '{room}'}, 'dskv02': {'vrtces': '{room}'},
    'dskxv': {'xptarr': '{nrays}', 'fndarr': '{nrays}'},
    'dskxsi': {'dladsc': '8', 'dskdsc': '24', 'dc': '{maxd}', 'ic': '{maxi}'},
    'ev2lin': {'state': '6'}, 'oscltx': {'elts': '20'}, 'spkpds': {'descr': '5'}, 'spksfs': {'descr': '5'},
    'scpart': {'pstart': '9999', 'pstop': '9999'},   # MXPART
    'getfvn': {'bounds': '{room}'},
    'latsrf': {'srfpts': '{npts}'}, 'srfnrm': {'normls': '{npts}'},
    'chbder': {'dpdxs': '{nderiv} + 1'}, 'polyds': {'p': '{nderiv} + 1'},
    'ekifld': {'rcptrs': '{nrows}'},
    'dafec': {'buffer': '{bufsiz}'}, 'dasec': {'buffer': '{bufsiz}'},
    'dskmi2': {'spaixi': '{spxisz}'},
    # (wrapped in _compat.py for SpiceyPy's types)
    'ekpsel': {x: '50' for x in ('xbegs', 'xends', 'xtypes', 'xclass', 'tabs', 'cols')},   # MAXSEL
    'ekssum': {x: '100' for x in ('cnames', 'dtypes', 'sizes', 'strlns', 'indexd', 'nullok')},   # MXCLSG
}

# The routines that release the GIL while PEPPER works: searches, geometry,
# DSK, EK, files, coverage (long calls). The others (most take well under a
# microsecond) keep it: releasing and taking back the GIL around a short
# call costs more than the call when threads contend for the GIL. In
# free-threaded Python all calls run in parallel.
LONG_CALLS = re.compile(r'^(gf|dsk|ek|dla|limbpt|termpt|sincpt|subpnt|subslr|subpt|subsol|ilum|illum|occult|'
                        r'fovtrg|fovray|latsrf|srfnrm|spkcov|ckcov|pckcov|spkobj|ckobj|pckfrm|dafopr|dafopw|'
                        r'dafcls|dasopr|dascls|unload|ldpool|spklef|pcklof|cklpf|eklef|ckgp|ckgpav|ckfrot|'
                        r'ckcls|spkcls|spkopn|ckopn|pckopn|kdata|kinfo|ktotal|tkfram)|'
                        r'(_coverage|_objects|_frames|_surfaces|_points|_plates|_vertices|_comments)$')

# SpiceyPy returns these found flags as the result (no NotFoundError).
FOUND_IS_RESULT = {'expool', 'daffna', 'daffpa', 'dskx02'}

# Dimensions named by parameters: their values. Planes, ellipses and EK
# attribute descriptors are passed and returned as SpiceyPy's types.
CONST_DIMS = {'ubpl': '4', 'ubel': '9', 'adscsz': '6', 'ixdfix': '10'}
WRAP_DIMS = {'ubpl': 'Plane', 'ubel': 'Ellipse', 'adscsz': 'SpiceEKAttDsc'}
# Assumed-size arrays that are SpiceyPy types (by argument name).
WRAP_NAMES = {'dladsc': 'SpiceDLADescr', 'nxtdsc': 'SpiceDLADescr', 'prvdsc': 'SpiceDLADescr',
              'dskdsc': 'SpiceDSKDescr'}
# The GF routines given to GFEVNT, GFFOVE, GFOCCE: the C interface's hook
# routine for each interface, and its slot in Context._gf_hooks_on.
GF_HOOKS = {'gf_step': ('capi_gf_step', 'step'), 'gf_refine': ('capi_gf_refn', 'refn'),
            'gf_rep_init': ('capi_gf_repi', 'repi'), 'gf_rep_update': ('capi_gf_repu', 'repu'),
            'gf_rep_final': ('capi_gf_repf', 'repf'), 'gf_bail': ('capi_gf_bail', 'bail')}
# Workspace arrays: made by the wrapper, not arguments in Python (their
# size: {x} is the value of the integer input x).
WORK = {
    'chbder': {'partdp': '3 * ({nderiv} + 1)'},
    'lgrind': {'work': '4 * {n}'}, 'lgrint': {'work': '4 * {n}'}, 'lgresp': {'work': '4 * {n}'},
    'hrmint': {'work': '4 * {n}'}, 'hrmesp': {'work': '4 * {n}'},
    'dskmi2': {'work': '2 * {worksz}'},
}
# Integer inputs SpiceyPy does not take: their value.
FIXED_IN = {'dskxsi': {'maxd': '1', 'maxi': '1'}}
# Counts SpiceyPy takes from an array: the array (its number of elements,
# or of rows for a list of vectors).
COUNT_OF = {'spkw18': {'n': 'epochs'}, 'dskmi2': {'nv': 'vrtces', 'np': 'plates'}, 'dskrb2': {'nv': 'vrtces', 'np': 'plates'}, 'dskxsi': {'nsurf': 'srflst'},
            'dskxv': {'nsurf': 'srflst', 'nrays': 'vtxarr'}, 'latsrf': {'npts': 'lonlat'},
            'srfnrm': {'npts': 'srfpts'}}
# Results returned as their first n elements ({x}: the output x, or a number).
SLICE = {
    'getfvn': {'bounds': 'n'}, 'scpart': {'pstart': 'nparts', 'pstop': 'nparts'}, 'oscltx': {'elts': '11'},
    'dskd02': {'values': 'n'}, 'dski02': {'values': 'n'}, 'dskp02': {'plates': 'n'}, 'dskv02': {'vrtces': 'n'},
}
# The width of the strings of an array of strings out.
STRARR_WIDTH = 1024
# CSPICE's checks of string arguments (CHKFSTR): an empty string signals
# SPICE(EMPTYSTRING). From CSPICE N0067's sources (tools/cspice_empty_strings.json).
import json as _json  # noqa: E402
EMPTY_CHECKS = _json.loads((Path(__file__).resolve().parent / 'cspice_empty_strings.json').read_text())


def extent(d):
    """The extent of a dimension: a number, 'lower:upper', or an argument."""
    m = re.fullmatch(r'(-?\d+):(-?\d+)', d)
    if m:
        return str(int(m.group(2)) - int(m.group(1)) + 1)
    return d


@dataclass(eq=False)
class Arg:
    name: str
    ftype: str          # char, int, real, logical, string
    intent: str         # in, out, inout
    dims: list          # [] scalar; ['6'], ['3','3'], [':'], ['*'], ['n'] ...
    alloc: bool = False
    deferred: bool = False   # character(:)
    doc: str = ''
    kind: str = ''      # classification (see classify)
    wrap: str = ''      # the SpiceyPy type of the array (Plane, Ellipse, ...)
    proc: tuple = None  # a GF routine: (the C interface's hook routine, its slot)

    @property
    def rank(self):
        return len(self.dims)


@dataclass
class Proc:
    name: str           # the Python and binding name
    impl: str           # the Fortran procedure
    owner: str          # ctx, spice, pure
    args: list
    result: Arg = None  # a function's result
    doc: list = field(default_factory=list)
    skip: str = ''      # why it is not wrapped

    @property
    def cname(self):
        return {'ctx': 'pepper_', 'spice': 'pepper_spice_', 'pure': 'pepper_'}[self.owner] + self.name


# ----------------------------------------------------------------------
# Parsing


def join_continuations(lines):
    out = []
    buf = ''
    for line in lines:
        s = line.rstrip()
        code = s.split('!')[0] if '!!' not in s else s
        if buf:
            s = buf + ' ' + s.lstrip().lstrip('&')
            buf = ''
        if re.search(r'&\s*(!.*)?$', s) and not s.lstrip().startswith('!'):
            buf = re.sub(r'&\s*(!.*)?$', '', s)
            continue
        out.append(s)
    return out


DECL = re.compile(r'^\s*([a-z][^:!]*?)\s*::\s*([^!]+?)\s*(?:!!(.*))?$', re.I)


def parse_decls(lines, args):
    """Declarations of the arguments: name -> Arg."""
    decl = {}
    for s in lines:
        if ' :: ' not in s or s.lstrip().startswith('!'):
            continue
        left, right = s.split(' :: ', 1)
        right, _, doc = right.partition('!!')
        t = re.sub(r'\s+', ' ', left.strip().lower())
        doc = doc.strip()
        for v in re.split(r',(?![^(]*\))', right):
            vm = re.match(r'\s*(\w+)\s*(\(.*\))?\s*$', v)
            if not vm:
                continue
            name = vm.group(1).lower()
            if name not in args:
                continue
            dims = []
            if vm.group(2):
                dims = [d.strip() for d in re.split(r',(?![^(]*\))', vm.group(2).strip()[1:-1].replace(' ', ''))]
            decl[name] = make_arg(name, t, dims, doc)
    return decl


def make_arg(name, t, dims, doc):
    wrap = ''
    for k, d in enumerate(dims):
        if d in WRAP_DIMS:
            wrap = WRAP_DIMS[d]
        if d in CONST_DIMS:
            dims[k] = CONST_DIMS[d]
        elif d.startswith('0:') and d != '0:*' and not re.fullmatch(r'0:-?\d+', d):
            dims[k] = d[2:] + '+1'      # 0:n has n + 1 elements
        elif d == '0:*':
            dims[k] = '*'
    if name in WRAP_NAMES:
        wrap = WRAP_NAMES[name]
    m = re.match(r'procedure\((\w+)\)', t)
    if m:
        hook = GF_HOOKS.get(m.group(1).lower())
        return Arg(name=name, ftype='gfproc' if hook else 'other', intent='in', dims=[], doc=doc,
                   proc=hook)
    m = re.search(r'intent\((in|out|inout)\)', t)
    intent = m.group(1) if m else ''
    # An optional output is always passed by the shim (so always made); an
    # optional input is not supported (its absence has a meaning).
    if ('optional' in t and intent != 'out') or 'value' in t.split(',')[0:1]:
        intent = 'optional' if 'optional' in t else intent
    if t.startswith('character'):
        ftype = 'char'
    elif t.startswith('integer'):
        ftype = 'int' if 'integer(ip)' in t or t.startswith('integer,') or t == 'integer' or t.startswith('integer(c_int)') else 'other'
    elif t.startswith('real'):
        ftype = 'real' if 'real(wp)' in t else 'other'
    elif t.startswith('logical'):
        ftype = 'logical'
    elif t.startswith('type(string)'):
        ftype = 'string'
    else:
        ftype = 'other'
    return Arg(name=name, ftype=ftype, intent=intent, dims=dims, alloc='allocatable' in t,
               deferred=t.startswith('character(:)'), doc=doc, wrap=wrap)


HEADER = re.compile(r'^\s*(?:recursive\s+)?module\s+(?:(integer\(ip\)|real\(wp\)|logical)\s+)?'
                    r'(subroutine|function)\s+(\w+)\s*\(([^)]*)\)', re.I)


_IMPL_DOCS = None


def impl_docs():
    """The docstrings of the procedures in the submodules (the summaries are
    there, not in the interfaces): name -> lines."""
    global _IMPL_DOCS
    if _IMPL_DOCS is None:
        _IMPL_DOCS = {}
        head = re.compile(r'^\s*(?:\w+(?:\(\w+\))?\s+)*module\s+(?:\w+(?:\(\w+\))?\s+)*(?:subroutine|function)\s+(\w+)',
                          re.I)
        for f in sorted((ROOT / 'src').glob('*.f90')):
            if f.name == 'pepper_instance_module.f90':
                continue
            lines = f.read_text().split('\n')
            for i, l in enumerate(lines):
                m = head.match(l)
                if not m:
                    continue
                doc = []
                k = i - 1
                while k >= 0 and lines[k].strip().startswith('!>'):
                    doc.insert(0, lines[k].strip()[2:].strip())
                    k -= 1
                _IMPL_DOCS.setdefault(m.group(1).lower(), doc)
    return _IMPL_DOCS


def parse_context_interfaces():
    """The context's procedures: binding name -> Proc."""
    lines = join_continuations((ROOT / 'src/pepper_instance_module.f90').read_text().split('\n'))
    start = next(i for i, l in enumerate(lines) if re.match(r'\s*type :: pepper_context', l))
    end = next(i for i, l in enumerate(lines) if i > start and re.match(r'\s*end type pepper_context', l))
    binds = {}
    for l in lines[start:end]:
        m = re.match(r'\s*procedure, non_overridable :: (\w+)(?:\s*=>\s*(\w+))?', l)
        if m:
            binds[m.group(1).lower()] = (m.group(2) or m.group(1)).lower()
    ifs = {}
    i = 0
    while i < len(lines):
        m = HEADER.match(lines[i])
        if m:
            rtype, kind, name, arglist = m.groups()
            args = [a.strip().lower() for a in arglist.split(',') if a.strip()]
            doc = []
            k = i - 1
            while k >= 0 and lines[k].strip().startswith('!>'):
                doc.insert(0, lines[k].strip()[2:].strip())
                k -= 1
            j = i + 1
            while not re.match(r'\s*end\s+(subroutine|function)', lines[j], re.I):
                j += 1
            decl = parse_decls(lines[i + 1:j], set(args))
            if not doc:
                doc = impl_docs().get(name.lower(), [])
            ifs[name.lower()] = (kind.lower(), rtype, args, decl, doc)
            i = j
        i += 1
    procs = {}
    for b, impl in binds.items():
        if impl not in ifs:
            continue
        kind, rtype, args, decl, doc = ifs[impl]
        if not args or args[0] != 'ctx':
            continue
        p = Proc(name=b, impl=impl, owner='ctx', args=[], doc=doc)
        for a in args[1:]:
            p.args.append(decl.get(a, Arg(a, 'other', '', [])))
        if kind == 'function':
            p.result = make_arg('fresult', (rtype or 'other').lower() + ', intent(out)', [], 'the result')
        procs[b] = p
    return procs


def parse_spice_methods():
    """The instance's procedures (spice_*): name -> Proc."""
    lines = join_continuations((ROOT / 'src/pepper_instance_module.f90').read_text().split('\n'))
    procs = {}
    for i, l in enumerate(lines):
        m = re.match(r'^\s*(?:module\s+)?subroutine spice_(\w+)\s*\(([^)]*)\)', l)
        if not m:
            continue
        name = m.group(1).lower()
        args = [a.strip().lower() for a in m.group(2).split(',') if a.strip()]
        doc = []
        k = i - 1
        while k >= 0 and lines[k].strip().startswith('!>'):
            doc.insert(0, lines[k].strip()[2:].strip())
            k -= 1
        j = i + 1
        while not re.match(r'\s*end\s+subroutine', lines[j]):
            j += 1
        decl = parse_decls(lines[i + 1:j], set(args))
        p = Proc(name=name, impl='spice_' + name, owner='spice', args=[decl.get(a, Arg(a, 'other', '', [])) for a in args[1:]], doc=doc)
        procs[name] = p
    return procs


MATH_HEADER = re.compile(r'^\s*(?:(?:pure|elemental)\s+)+(?:(integer\(ip\)|real\(wp\)|logical)\s+)?'
                         r'(subroutine|function)\s+(\w+)\s*\(([^)]*)\)', re.I)


PURE_MODULES = ['pepper_math', 'pepper_strings', 'pepper_arrays']


def parse_math(module='pepper_math'):
    text = (ROOT / f'src/{module}.f90').read_text()
    lines = join_continuations(text.split('\n'))
    public = set()
    for line in lines:
        m = re.match(r'\s*public ::(.*)', line)
        if m:
            public |= {w.strip().lower() for w in re.split(r'[,&\s]+', m.group(1)) if w.strip()}
    procs = {}
    for i, l in enumerate(lines):
        m = MATH_HEADER.match(l)
        if not m:
            continue
        rtype, kind, name, arglist = m.groups()
        name = name.lower()
        if name not in public:
            continue
        args = [a.strip().lower() for a in arglist.split(',') if a.strip()]
        doc = []
        k = i - 1
        while k >= 0 and lines[k].strip().startswith('!>'):
            doc.insert(0, lines[k].strip()[2:].strip())
            k -= 1
        j = i + 1
        while not re.match(r'\s*end\s+(subroutine|function)', lines[j], re.I):
            j += 1
        decl = parse_decls(lines[i + 1:j], set(args))
        p = Proc(name=name, impl=name, owner='pure', args=[decl.get(a, Arg(a, 'other', '', [])) for a in args], doc=doc)
        p.module = module
        if kind.lower() == 'function':
            p.result = make_arg('fresult', (rtype or 'other').lower() + ', intent(out)', [], 'the result')
        procs[name] = p
    return procs


def parse_constants():
    out = {}
    for line in (ROOT / 'src/pepper_constants.f90').read_text().split('\n'):
        m = re.match(r'\s*real\(wp\), parameter, public :: (\w+)\s*=\s*([^!]+?)\s*(?:!!(.*))?$', line)
        if m:
            expr = re.sub(r'(\d)_wp', r'\1', m.group(2)).replace('acos', 'math.acos')
            out[m.group(1).lower()] = (eval(expr, {'math': math}), (m.group(3) or '').strip())
    return out


# ----------------------------------------------------------------------
# Classification


def classify(p):
    names = {a.name for a in p.args}
    int_in = {a.name for a in p.args if a.ftype == 'int' and a.intent == 'in' and not a.dims}
    allargs = p.args + ([p.result] if p.result else [])
    sized = OUT_SIZES.get(p.name, {})
    work = WORK.get(p.name, {})

    def size_ok(expr):
        return all(n in int_in for n in re.findall(r'\{(\w+)\}', expr))

    for a in allargs:
        if a.ftype == 'gfproc':
            if p.owner != 'ctx':
                return f'argument {a.name}: GF routine'
            a.kind = 'hook'
            continue
        if a.ftype == 'other' or a.intent in ('', 'optional'):
            return f'argument {a.name}: unsupported type'
        if a.name in ('ctx', 'self', 'fresult') and a is not p.result:
            return f'argument {a.name}'
        if a.name in work:
            if a.ftype not in ('real', 'int') or not size_ok(work[a.name]):
                return f'argument {a.name}: workspace'
            a.kind = 'work'
            continue
        if a.ftype == 'char':
            if a.deferred and a.alloc and a.intent == 'out' and not a.dims and p.owner == 'ctx':
                a.kind = 'astr_out'
            elif a.rank == 1 and (a.dims[0] == '*' or a.dims[0].isdigit()) and a.intent == 'in':
                a.kind = 'strarr_in'
            elif a.dims == ['*'] and a.intent == 'out' and a.name in sized and size_ok(sized[a.name]):
                a.kind = 'strarr_out'
            elif a.dims:
                return f'argument {a.name}: array of strings'
            elif a.intent == 'in':
                a.kind = 'str_in'
            elif a.intent == 'out':
                a.kind = 'str_out'
            else:
                a.kind = 'str_inout'
        elif a.ftype == 'string':
            if a.alloc and a.intent == 'out' and a.dims == [':'] and p.owner == 'ctx':
                a.kind = 'strlist_out'
            else:
                return f'argument {a.name}: list of strings'
        elif not a.dims:
            a.kind = 'num_' + a.intent
        elif a.alloc:
            if a.intent == 'out' and all(d == ':' for d in a.dims) and a.rank <= 2 and a.ftype in ('real', 'int') and p.owner == 'ctx':
                a.kind = 'alloc_out'
            else:
                return f'argument {a.name}: allocatable'
        elif all(d == ':' for d in a.dims):
            if a.intent == 'in' and a.rank <= 2 and a.ftype in ('real', 'int'):
                a.kind = 'shape_in'
            else:
                return f'argument {a.name}: assumed shape {a.intent}'
        elif a.dims == ['lbcell:*']:
            if a.ftype not in ('real', 'int'):
                return f'argument {a.name}: cell of {a.ftype}'
            a.kind = 'cell'
        elif a.ftype == 'logical':
            if a.intent == 'in' and a.rank == 1 and (a.dims[0] == '*' or a.dims[0].isdigit()):
                a.kind = 'logarr_in'
            elif a.intent == 'out' and a.dims == ['*'] and a.name in sized and size_ok(sized[a.name]):
                a.kind = 'logarr_out'
            else:
                return f'argument {a.name}: logical array'
        elif a.dims[-1] == '*' and a.intent == 'out' and a.name in sized and size_ok(sized[a.name]) \
                and a.ftype in ('real', 'int') and (a.rank == 1 or (a.rank == 2 and extent(a.dims[0]).isdigit())):
            a.kind = 'ptr_out'
        elif a.dims[-1] == '*':
            if a.intent not in ('in', 'inout') or a.ftype not in ('real', 'int') or a.rank > 2:
                return f'argument {a.name}: assumed size {a.intent}'
            if a.intent == 'inout' and a.rank != 1:
                return f'argument {a.name}: assumed size inout'
            if a.rank == 2 and not (extent(a.dims[0]).isdigit() or a.dims[0] in int_in):
                return f'argument {a.name}: dimension {a.dims[0]}'
            a.kind = 'ptr_in' if a.intent == 'in' else 'ptr_inout'
        else:
            if a.rank > 2:
                return f'argument {a.name}: rank {a.rank}'
            for d in a.dims:
                if not (d.isdigit() or re.fullmatch(r'-?\d+:-?\d+', d) or
                        (re.fullmatch(r'[\w*+\- ]+', d) and all(x in int_in or x.isdigit()
                                                               for x in re.findall(r'\w+', d)))):
                    return f'argument {a.name}: dimension {d}'
            a.kind = 'arr_' + a.intent
    return ''


# ----------------------------------------------------------------------
# SpiceyPy's signatures


def spiceypy_info():
    try:
        import spiceypy
        from spiceypy import spiceypy as sp
    except ImportError:
        print('warning: SpiceyPy is not installed: Fortran names used', file=sys.stderr)
        return {}
    info = {}
    for name in dir(sp):
        f = getattr(sp, name)
        if not inspect.isfunction(f) or name.startswith('_'):
            continue
        try:
            sig = inspect.signature(f)
            src = inspect.getsource(f)
        except (TypeError, ValueError, OSError):
            continue
        params = [(q.name, q.default) for q in sig.parameters.values()
                  if q.kind in (q.POSITIONAL_OR_KEYWORD, q.POSITIONAL_ONLY, q.KEYWORD_ONLY)]
        vec = None
        for q, _ in params:
            if re.search(rf'hasattr\(\s*{q}\s*,\s*"__iter__"\s*\)|is_iterable\(\s*{q}\s*\)', src):
                vec = q
                break
        info[name] = (params, vec)
    return info


def pyname(a):
    return safe_py(a.name)


def safe_py(n):
    """A name usable as a parameter in Cython."""
    return n + '_' if keyword.iskeyword(n) or n in C_RESERVED else n


COUNT_NAMES = {'n', 'ndim', 'nvals', 'nelts', 'nitems', 'count', 'nlines', 'ncvals', 'nnames', 'ncols', 'nrows'}
SIZE_LIKE = re.compile(r'^(len\w*|\w*len|room|maxn|size|nintvls?|mx\w+|max\w+|buflen|\w+len\w*)$')


def derivable(p, a, given):
    """How to compute an integer input SpiceyPy omits from the arrays given:
    a Python expression on their parameters' names, or None."""
    if a.ftype != 'int' or a.dims or a.intent != 'in':
        return None
    if a.name in FIXED_IN.get(p.name, {}):
        return FIXED_IN[p.name][a.name]
    src = COUNT_OF.get(p.name, {}).get(a.name)
    if src:
        b = next((x for x in p.args if x.name == src and x in given), None)
        if b is None:
            return None
        return f'(np.shape({given[b]})[0] if np.ndim({given[b]}) > 1 else np.size({given[b]}))' \
            if b.rank > 1 else f'np.size({given[b]})'
    arrays = [b for b in p.args if b is not a and b in given and b.kind in ('arr_in', 'arr_inout', 'ptr_in',
                                                                          'strarr_in', 'shape_in', 'logarr_in')]
    for b in arrays:
        for d in b.dims:
            m = re.fullmatch(r'(\d+)\*(\w+)', d)
            if m and m.group(2) == a.name and b.rank == 1:
                return f'(np.size({given[b]}) // {m.group(1)})'
        if a.name in b.dims:
            k = b.dims.index(a.name)
            if b.rank == 2 and b.dims[0].isdigit() and k == 1:     # rows (n, k)
                return f'np.shape({given[b]})[0]'
            return f'np.shape({given[b]})[{k}]' if b.rank > 1 else f'np.size({given[b]})'
    if a.name in COUNT_NAMES:
        cands = [b for b in arrays if b.kind in ('ptr_in', 'strarr_in') or (b.dims and b.dims[-1] == '*')]
        if cands:      # (arrays of the same count: the first one's)
            b = cands[0]
            if b.kind == 'strarr_in':
                return f'(1 if isinstance({given[b]}, (str, bytes)) else len({given[b]}))'
            return f'(np.shape({given[b]})[0] if np.ndim({given[b]}) > 1 else np.size({given[b]}))' \
                if b.rank > 1 else f'np.size({given[b]})'
    return None


def python_params(p, spy):
    """The Python parameters of a procedure: [(python name, input Arg or None, default)],
    whether they follow SpiceyPy's, and the vectorized parameter. A cell the
    procedure fills (out, inout) is an optional parameter: made if not given.
    p.derived: the integer inputs computed from arrays (SpiceyPy's omits
    them: vnormg(v) for VNORMG(V, NDIM))."""
    p.derived = {}
    cellouts = [a for a in p.args if a.kind == 'cell' and a.intent in ('out', 'inout')]
    required = [a for a in p.args if a.intent in ('in', 'inout') and a not in cellouts and a.kind != 'work']
    withcells = [a for a in p.args if a in required or a in cellouts]
    if p.name in spy:
        params, vec = spy[p.name]
        # by name first
        byname = {}
        for n, d in params:
            for a in withcells:
                if a not in byname.values() and n.lower() in (a.name, pyname(a)):
                    byname[n] = a
                    break
        rest_spy = [(n, d) for n, d in params if n not in byname]
        rest_f = [a for a in withcells if a not in byname.values()]
        sized = [(n, d) for n, d in rest_spy if n in SIZE_PARAMS or SIZE_LIKE.match(n)]
        unsized = [(n, d) for n, d in rest_spy if (n, d) not in sized]
        mapping = dict(byname)
        ok = False
        for keep_sized in (False, True):
            cand_spy = unsized + (sized if keep_sized else [])
            cand_spy = [x for x in params if x in cand_spy]    # SpiceyPy's order
            # Fortran inputs SpiceyPy omits: cells to fill (optional), or derived
            given = {}
            m = dict(mapping)
            fl = [a for a in rest_f]
            # positional for the rest
            optional_cells = [a for a in fl if a in cellouts]
            needed = [a for a in fl if a not in cellouts]
            if len(cand_spy) == len(fl):
                for (n, d), a in zip(cand_spy, fl):
                    m[n] = a
                ok = True
            elif len(cand_spy) == len(needed):
                for (n, d), a in zip(cand_spy, needed):
                    m[n] = a
                ok = True
            else:
                # some integer inputs may be derived from arrays
                given = {a: safe_py(n) for n, a in m.items()}
                der = [a for a in needed if derivable(p, a, {**given, **{b: safe_py(nn) for (nn, dd), b in
                                                                        zip(cand_spy, [x for x in needed])}})]
                still = [a for a in needed if a not in der]
                if len(cand_spy) == len(still):
                    for (n, d), a in zip(cand_spy, still):
                        m[n] = a
                    given = {a: safe_py(n) for n, a in m.items()}
                    derived = {a: derivable(p, a, given) for a in der}
                    if all(derived.values()):
                        p.derived = derived
                        ok = True
            if ok:
                mapping = m
                ignored = [n for n, d in params if n not in mapping]
                break
        if ok:
            out = []
            for n, d in params:
                if n in mapping:
                    a = mapping[n]
                    dflt = None
                    if d is not inspect.Parameter.empty:
                        dflt = repr(d)
                    if a in cellouts:
                        dflt = 'None'
                    out.append((safe_py(n), a, dflt))
                else:
                    out.append((safe_py(n), None, 'None'))
            for a in cellouts:
                if a not in mapping.values():
                    out.append((pyname(a), a, 'None'))
            # a parameter without a default may not follow one with
            seen_default = False
            for k, (n, a, d) in enumerate(out):
                if d is not None:
                    seen_default = True
                elif seen_default:
                    out[k] = (n, a, 'None')
            vec = safe_py(vec) if vec else vec
            if vec and not any(n == vec for n, a, d in out if a is not None):
                vec = None
            return out, True, vec
        p.derived = {}
    out = [(pyname(a), a, None) for a in required]
    out += [(pyname(a), a, 'None') for a in cellouts]
    return out, False, None


# ----------------------------------------------------------------------
# Fortran shims

FTYPE_C = {'int': 'integer(c_int)', 'real': 'real(c_double)', 'logical': 'integer(c_int)'}
FTYPE_W = {'int': 'integer(ip)', 'real': 'real(wp)', 'logical': 'logical'}


def fortran_shim(p):
    dummies = []      # names of the C dummy arguments
    decls = []
    locs = []
    pre = []
    post = []
    callargs = []
    rbuf = ibuf = sbuf = 0
    if p.owner == 'ctx':
        dummies.append('ctx')
        decls.append('type(c_ptr), value :: ctx  !! the context')
        locs.append('type(capi_context), pointer :: c_')
        pre.append('c_ => context_of(ctx)')
    elif p.owner == 'spice':
        dummies.append('spice')
        decls.append('type(c_ptr), value :: spice  !! the instance')
        locs.append('type(pepper_instance), pointer :: s_')
        pre.append('s_ => spice_of(spice)')
    allargs = p.args + ([p.result] if p.result else [])
    for a in allargs:
        n = a.name
        doc = (a.doc or '').replace('!!', '')
        k = a.kind
        if k == 'str_in':
            dummies.append(n)
            decls.append(f'character(kind=c_char), intent(in) :: {n}(*)  !! {doc}')
            locs.append(f'character(:), allocatable :: f_{n}')
            pre.append(f'call c_to_f({n}, f_{n})')
            callargs.append(f'f_{n}')
        elif k == 'str_out':
            dummies += [f'{n}_size', n]
            decls.append(f'integer(c_int), value :: {n}_size  !! the size of {n} (with the NUL)')
            decls.append(f'character(kind=c_char), intent(out) :: {n}(*)  !! {doc}')
            locs.append(f'character(len=max({n}_size - 1, 1)) :: f_{n}')
            pre.append(f"f_{n} = ' '")
            post.append(f'call f_to_c(f_{n}, {n}_size, {n})')
            callargs.append(f'f_{n}')
        elif k == 'astr_out':
            sbuf += 1
            dummies += [n, f'{n}_len']
            decls.append(f'type(c_ptr), intent(out) :: {n}  !! {doc} (in the context)')
            decls.append(f'integer(c_int), intent(out) :: {n}_len  !! its length')
            locs.append(f'character(:), allocatable :: f_{n}')
            post.append(f"if (.not. allocated(f_{n})) f_{n} = ''")
            post.append(f'call set_string_result(c_, {sbuf}, f_{n}, {n}, {n}_len)')
            callargs.append(f'f_{n}')
        elif k == 'strlist_out':
            sbuf += 1
            dummies += [n, f'{n}_n']
            decls.append(f'type(c_ptr), intent(out) :: {n}  !! {doc} (in the context, each NUL-terminated)')
            decls.append(f'integer(c_int), intent(out) :: {n}_n  !! the number of strings')
            locs.append(f'type(string), allocatable :: f_{n}(:)')
            post.append(f'if (.not. allocated(f_{n})) allocate (f_{n}(0))')
            post.append(f'call set_strings_result(c_, {sbuf}, f_{n}, {n}, {n}_n)')
            callargs.append(f'f_{n}')
        elif k == 'num_in':
            dummies.append(n)
            decls.append(f'{FTYPE_C[a.ftype]}, value :: {n}  !! {doc}')
            callargs.append(f'({n} /= 0)' if a.ftype == 'logical' else n)
        elif k in ('num_out', 'num_inout'):
            dummies.append(n)
            intent = k[4:]
            decls.append(f'{FTYPE_C[a.ftype]}, intent({intent}) :: {n}  !! {doc}')
            if a.ftype == 'logical' or a is p.result:
                locs.append(f'{FTYPE_W[a.ftype]} :: f_{n}')
                if k == 'num_inout':
                    pre.append(f'f_{n} = ({n} /= 0)' if a.ftype == 'logical' else f'f_{n} = {n}')
                post.append(f'{n} = merge(1_c_int, 0_c_int, f_{n})' if a.ftype == 'logical' else f'{n} = f_{n}')
                if a is not p.result:
                    callargs.append(f'f_{n}')
            else:
                callargs.append(n)
        elif k.startswith('arr_'):
            dummies.append(n)
            decls.append(f'{FTYPE_C[a.ftype]}, intent({k[4:]}) :: {n}({",".join(a.dims)})  !! {doc}')
            callargs.append(n)
        elif k == 'cell':
            dummies.append(n)
            decls.append(f'{FTYPE_C[a.ftype]}, intent({a.intent}) :: {n}(-5:*)  !! {doc} (a cell: the control area first)')
            callargs.append(n)
        elif k in ('ptr_in', 'ptr_out', 'ptr_inout'):
            dummies.append(n)
            decls.append(f'{FTYPE_C[a.ftype]}, intent({a.intent}) :: {n}({",".join(a.dims)})  !! {doc}')
            callargs.append(n)
        elif k == 'strarr_in':
            dummies += [f'{n}_n', f'{n}_width', n]
            decls.append(f'integer(c_int), value :: {n}_n  !! the number of strings in {n}')
            decls.append(f'integer(c_int), value :: {n}_width  !! the width of each string in {n}')
            decls.append(f'character(kind=c_char), intent(in) :: {n}(*)  !! {doc}')
            locs.append(f'character(:), allocatable :: f_{n}(:)')
            pre.append(f'call c_to_f_array({n}, {n}_n, {n}_width, f_{n})')
            callargs.append(f'f_{n}')
        elif k == 'hook':
            callargs.append(a.proc[0])
        elif k == 'work':
            dummies.append(n)
            decls.append(f'{FTYPE_C[a.ftype]}, intent(inout) :: {n}(*)  !! workspace')
            callargs.append(n)
        elif k == 'str_inout':
            dummies += [f'{n}_size', n]
            decls.append(f'integer(c_int), value :: {n}_size  !! the size of {n} (with the NUL)')
            decls.append(f'character(kind=c_char), intent(inout) :: {n}({n}_size)  !! {doc}')
            locs.append(f'character(len=max({n}_size - 1, 1)) :: f_{n}')
            locs.append(f'character(:), allocatable :: t_{n}')
            pre.append(f'call c_to_f({n}, t_{n})')
            pre.append(f'f_{n} = t_{n}')
            post.append(f'call f_to_c(f_{n}, {n}_size, {n})')
            callargs.append(f'f_{n}')
        elif k == 'strarr_out':
            dummies += [f'{n}_n', f'{n}_width', n]
            decls.append(f'integer(c_int), value :: {n}_n  !! the number of strings {n} holds')
            decls.append(f'integer(c_int), value :: {n}_width  !! the size of each (with the NUL)')
            decls.append(f'character(kind=c_char), intent(out) :: {n}({n}_n * {n}_width)  !! {doc}')
            locs.append(f'character(len=max({n}_width - 1, 1)) :: f_{n}(max({n}_n, 1))')
            locs.append(f'integer :: i_{n}')
            pre.append(f"f_{n} = ' '")
            post.append(f'do i_{n} = 1, {n}_n')
            post.append(f'   call f_to_c(f_{n}(i_{n}), {n}_width, {n}((i_{n} - 1) * {n}_width + 1:i_{n} * {n}_width))')
            post.append('end do')
            callargs.append(f'f_{n}')
        elif k in ('logarr_in', 'logarr_out'):
            io = k[7:]
            dummies += [f'{n}_n', n]
            decls.append(f'integer(c_int), value :: {n}_n  !! the number of flags in {n}')
            decls.append(f'integer(c_int), intent({io}) :: {n}(max({n}_n, 1))  !! {doc} (0: false)')
            locs.append(f'logical :: f_{n}(max({n}_n, 1))')
            if io == 'in':
                pre.append(f'f_{n} = {n} /= 0')
            else:
                pre.append(f'f_{n} = .false.')
                post.append(f'{n} = merge(1_c_int, 0_c_int, f_{n})')
            callargs.append(f'f_{n}')
        elif k == 'shape_in':
            if a.rank == 1:
                dummies += [f'{n}_n', n]
                decls.append(f'integer(c_int), value :: {n}_n  !! the size of {n}')
                decls.append(f'{FTYPE_C[a.ftype]}, intent(in) :: {n}({n}_n)  !! {doc}')
            else:
                dummies += [f'{n}_rows', f'{n}_cols', n]
                decls.append(f'integer(c_int), value :: {n}_rows  !! the number of rows of {n} (Fortran: its columns)')
                decls.append(f'integer(c_int), value :: {n}_cols  !! the number of columns of {n} (Fortran: its rows)')
                decls.append(f'{FTYPE_C[a.ftype]}, intent(in) :: {n}({n}_cols, {n}_rows)  !! {doc}')
            callargs.append(n)
        elif k == 'alloc_out':
            if a.ftype == 'real':
                rbuf += 1
                kb, setter, ctype = rbuf, 'set_real_result', 'real(wp)'
            else:
                ibuf += 1
                kb, setter, ctype = ibuf, 'set_int_result', 'integer(ip)'
            dummies.append(n)
            decls.append(f'type(c_ptr), intent(out) :: {n}  !! {doc} (in the context)')
            if a.rank == 1:
                dummies.append(f'{n}_n')
                decls.append(f'integer(c_int), intent(out) :: {n}_n  !! its size')
                locs.append(f'{ctype}, allocatable :: f_{n}(:)')
                post.append(f'if (.not. allocated(f_{n})) allocate (f_{n}(0))')
                post.append(f'call {setter}(c_, {kb}, f_{n}, {n}, {n}_n)')
            else:
                dummies += [f'{n}_rows', f'{n}_cols']
                decls.append(f'integer(c_int), intent(out) :: {n}_rows  !! its number of rows (Fortran: columns)')
                decls.append(f'integer(c_int), intent(out) :: {n}_cols  !! its number of columns (Fortran: rows)')
                locs.append(f'{ctype}, allocatable :: f_{n}(:,:)')
                locs.append(f'integer(c_int) :: {n}_total')
                post.append(f'if (.not. allocated(f_{n})) allocate (f_{n}(0, 0))')
                post.append(f'call {setter}(c_, {kb}, reshape(f_{n}, [size(f_{n})]), {n}, {n}_total)')
                post.append(f'{n}_rows = int(size(f_{n}, 2), c_int)')
                post.append(f'{n}_cols = int(size(f_{n}, 1), c_int)')
            callargs.append(f'f_{n}')
        else:
            raise ValueError(f'{p.name}: {a.name}: {k}')
    if rbuf > 4 or ibuf > 4 or sbuf > 4:
        raise ValueError(p.name + ': too many results')
    target = {'ctx': f'c_%{p.name}', 'spice': f's_%{p.name}', 'pure': p.impl}[p.owner]
    call = f'{target}({", ".join(callargs)})'
    if p.result:
        callstmt = f'f_fresult = {call}'
    else:
        callstmt = f'call {call}'
    status = {'ctx': 'status_of(c_)', 'spice': 'spice_status(s_)', 'pure': '0_c_int'}[p.owner]
    summary = ' '.join(p.doc).split('. ')[0].rstrip('.') if p.doc else p.name
    fname = 'c_' + p.cname
    out = []
    out.append(f'   !> C interface of `{p.name}`: {summary}.')
    head = f'   integer(c_int) function {fname} ( {", ".join(dummies)} ) bind(c, name=\'{p.cname}\')'
    out.extend(wrap_fortran(head))
    # The scalars that are array bounds are declared before the arrays: a
    # name in a specification expression must have its type already
    # (implicit none; using it first is a GNU extension).
    def declared(d):
        return re.match(r'.*?::\s*(\w+)', d).group(1)
    bounds = set()
    for d in decls:
        m = re.match(r'.*?::\s*\w+\(([^)]*)\)', d.split('!!')[0])
        if m:
            bounds |= set(re.findall(r'[a-z_]\w*', m.group(1), re.I))
    first = [d for d in decls if declared(d) in bounds and '(' not in d.split('::')[1].split('!!')[0]]
    decls = first + [d for d in decls if d not in first]
    for d in decls:
        out.append('      ' + d.rstrip().rstrip('!').rstrip())
    for d in locs:
        out.append('      ' + d)
    for s in pre:
        out.append('      ' + s)
    out.extend('      ' + s for s in wrap_fortran(callstmt, indent=''))
    for s in post:
        out.append('      ' + s)
    out.append(f'      {fname} = {status}')
    out.append(f'   end function {fname}')
    out.append('')
    return '\n'.join(out)


def wrap_fortran(line, indent='', width=120):
    if len(indent + line) <= width:
        return [indent + line]
    parts = []
    cur = indent + line
    while len(cur) > width:
        cut = cur.rfind(', ', 0, width - 2)
        if cut < 0:
            break
        parts.append(cur[:cut + 1] + ' &')
        cur = '         ' + cur[cut + 2:]
    parts.append(cur)
    return parts


# ----------------------------------------------------------------------
# C declarations

CTYPE = {'int': 'int', 'real': 'double', 'logical': 'int'}


C_RESERVED = set('''auto break case char const continue default do double else enum extern float for goto if
inline int long register restrict return short signed sizeof static struct switch typedef union unsigned void
volatile while bool class delete new operator private protected public template this throw try virtual
cdef cpdef ctypedef cimport include gil nogil from in is not and or lambda def del global pass raise with yield
None True False print exec async await namespace friend using typename export'''.split())


def cname_of(n):
    """An argument's name in C (and Cython) declarations."""
    return n + '_' if n in C_RESERVED else n


def c_params(p):
    ps = []
    if p.owner == 'ctx':
        ps.append('void *ctx')
    elif p.owner == 'spice':
        ps.append('void *spice')
    for a in p.args + ([p.result] if p.result else []):
        n = cname_of(a.name)
        k = a.kind
        t = CTYPE.get(a.ftype)
        if k == 'str_in':
            ps.append(f'const char *{n}')
        elif k == 'str_out':
            ps += [f'int {n}_size', f'char *{n}']
        elif k == 'astr_out':
            ps += [f'const char **{n}', f'int *{n}_len']
        elif k == 'strlist_out':
            ps += [f'const char **{n}', f'int *{n}_n']
        elif k == 'num_in':
            ps.append(f'{t} {n}')
        elif k in ('num_out', 'num_inout'):
            ps.append(f'{t} *{n}')
        elif k == 'cell':
            ps.append(f'{"const " if a.intent == "in" else ""}{t} *{n}')
        elif k in ('arr_in', 'ptr_in'):
            ps.append(f'const {t} *{n}')
        elif k == 'strarr_in':
            ps += [f'int {n}_n', f'int {n}_width', f'const char *{n}']
        elif k in ('arr_out', 'arr_inout', 'ptr_out'):
            ps.append(f'{t} *{n}')
        elif k == 'work':
            ps.append(f'{t} *{n}')
        elif k == 'str_inout':
            ps += [f'int {n}_size', f'char *{n}']
        elif k == 'strarr_out':
            ps += [f'int {n}_n', f'int {n}_width', f'char *{n}']
        elif k == 'logarr_in':
            ps += [f'int {n}_n', f'const int *{n}']
        elif k == 'logarr_out':
            ps += [f'int {n}_n', f'int *{n}']
        elif k == 'ptr_inout':
            ps.append(f'{t} *{n}')
        elif k == 'shape_in':
            if a.rank == 1:
                ps += [f'int {n}_n', f'const {t} *{n}']
            else:
                ps += [f'int {n}_rows', f'int {n}_cols', f'const {t} *{n}']
        elif k == 'alloc_out':
            if a.rank == 1:
                ps += [f'const {t} **{n}', f'int *{n}_n']
            else:
                ps += [f'const {t} **{n}', f'int *{n}_rows', f'int *{n}_cols']
    return ps


# ----------------------------------------------------------------------
# Cython


def py_doc(p, params, aligned, outs, found):
    lines = []
    text = ' '.join(l for l in p.doc if not l.startswith('PEPPER:') or True).strip()
    if text:
        # first paragraph
        para = []
        for l in p.doc:
            if not l:
                if para:
                    break
                continue
            para.append(l)
        lines.append(' '.join(para).replace('[[', '`').replace(']]', '`').replace('"', "'"))
    else:
        lines.append(p.name)
    lines.append('')
    if '_' not in p.name:
        lines.append(f'https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/{p.name}.html')
        lines.append('')
    for pn, a, d in params:
        if a is None:
            lines.append(f':param {pn}: (SpiceyPy compatibility; ignored)')
        else:
            lines.append(f':param {pn}: {clean(a.doc)}')
    if outs:
        lines.append(':return: ' + '; '.join(clean(a.doc) or a.name for a in outs))
    if found:
        lines.append('    (found: NotFoundError if not found, or the flag returned last with no_found_check)')
    return '\n'.join(lines)


def clean(s):
    return (s or '').replace('[[', '`').replace(']]', '`').replace('"', "'").replace('\\', '/').strip()


def csize(expr):
    """A size expression ({x}: the integer input x) in Cython."""
    return re.sub(r'\{(\w+)\}', r'_c_\1', expr)


def cdims(dims):
    """Fixed dimensions (numbers, ranges, expressions of integer inputs) in Cython."""
    out = []
    for d in dims:
        e = extent(d)
        out.append(e if e.lstrip('-').isdigit() else re.sub(r'([a-z_]\w*)', r'_c_\1', e))
    return ', '.join(out)


def cython_method(p, spy, where):
    """A method (where = 'ctx' or 'spice') or a module-level function ('pure')."""
    params, aligned, vec = python_params(p, spy)
    allargs = p.args + ([p.result] if p.result else [])
    found = next((a for a in p.args if a.name in ('found', 'isname') and a.kind == 'num_out'
                  and a.ftype == 'logical' and p.name not in FOUND_IS_RESULT), None)
    outs = [a for a in allargs if a.intent in ('out', 'inout') and a is not found and a.kind != 'work']
    sig = []
    for pn, a, d in params:
        sig.append(f'{pn}={d}' if d is not None else pn)
    selfp = ['self'] if where in ('ctx', 'spice') else []
    body = []
    I = '        ' if where in ('ctx', 'spice') else '    '
    if vec:
        others = ', '.join(pn for pn, a, d in params)
        call_i = ', '.join('_v' if pn == vec else pn for pn, a, d in params)
        prefix = 'self.' if where in ('ctx', 'spice') else ''
        body.append(f'if _iterable({vec}):')
        body.append(f'    return _stack([{prefix}{p.name}({call_i}) for _v in {vec}])')
    pymap = {id(a): pn for pn, a, d in params if a is not None}
    cargs = []
    if where == 'ctx':
        cargs.append('self._ptr')
    elif where == 'spice':
        cargs.append('self._ptr')
    cdefs = []
    conv = []
    results = []
    hooks = {}
    checks = []
    for a in allargs:
        n = a.name
        pn = pymap.get(id(a), n)
        k = a.kind
        t = CTYPE.get(a.ftype)
        dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
        ct = 'double' if a.ftype == 'real' else 'int'
        if k == 'str_in' and n in EMPTY_CHECKS.get(p.name, ()) and id(a) in pymap:
            checks.append(f'if not {pn}: _empty_string({p.name!r}, {n!r})')
        if k == 'hook':
            hooks[a.proc[1]] = pn
        elif k == 'work':
            conv.append(f'cdef cnp.ndarray _a_{n} = _empty({dt}, max(<int>({csize(WORK[p.name][n])}), 1), -1)')
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
        elif k == 'str_inout':
            conv.append(f"cdef bytearray _ba_{n} = bytearray(_b({pn} if {pn} is not None else '')[:{STR_OUT_SIZE - 1}]"
                        f".ljust({STR_OUT_SIZE}, b'\\0'))")
            conv.append(f'cdef char *_c_{n} = _ba_{n}')
            cargs += [str(STR_OUT_SIZE), f'_c_{n}']
            results.append((a, f'_s(_c_{n})'))
        elif k == 'strarr_out':
            conv.append(f'cdef int _n_{n} = max(<int>({csize(OUT_SIZES[p.name][n])}), 1)')
            conv.append(f'cdef bytearray _ba_{n} = bytearray(_n_{n} * {STRARR_WIDTH})')
            conv.append(f'cdef char *_c_{n} = _ba_{n}')
            cargs += [f'_n_{n}', str(STRARR_WIDTH), f'_c_{n}']
            results.append((a, f'_strs(_ba_{n}, _n_{n}, {STRARR_WIDTH})'))
        elif k == 'logarr_in':
            conv.append(f'cdef cnp.ndarray _a_{n} = _rows(np.asarray({pn}).astype(bool).astype(np.int32), NPY_INT32, 1)')
            cargs += [f'<int>cnp.PyArray_DIM(_a_{n}, 0)', f'<const int*>cnp.PyArray_DATA(_a_{n})']
        elif k == 'logarr_out':
            conv.append(f'cdef int _n_{n} = max(<int>({csize(OUT_SIZES[p.name][n])}), 0)')
            conv.append(f'cdef cnp.ndarray _a_{n} = _empty(NPY_INT32, max(_n_{n}, 1), -1)')
            cargs += [f'_n_{n}', f'<int*>cnp.PyArray_DATA(_a_{n})']
            results.append((a, f'(_a_{n}[:_n_{n}] != 0)'))
        elif k == 'ptr_inout':
            conv.append(f'cdef cnp.ndarray _a_{n} = _rows(np.array({pn}, dtype=np.float64 if {dt} == NPY_DOUBLE '
                        f'else np.int32), {dt}, 1)')
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
            results.append((a, f'_a_{n}'))
        elif k == 'ptr_out' and a.rank == 2:
            conv.append(f'cdef int _n_{n} = max(<int>({csize(OUT_SIZES[p.name][n])}), 1)')
            conv.append(f'cdef cnp.ndarray _a_{n} = np.zeros((_n_{n}, {extent(a.dims[0])}), '
                        f'dtype=np.float64 if {dt} == NPY_DOUBLE else np.int32)')
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
            results.append((a, f'_a_{n}'))
        elif k == 'str_in':
            conv.append(f'cdef bytes _b_{n} = _b({pn})')
            cdefs.append(f'cdef const char *_c_{n} = _b_{n}')
            cargs.append(f'_c_{n}')
        elif k == 'str_out':
            cdefs.append(f'cdef char _c_{n}[{STR_OUT_SIZE}]')
            conv.append(f'_c_{n}[0] = 0')
            cargs += [str(STR_OUT_SIZE), f'_c_{n}']
            results.append((a, f'_s(_c_{n})'))
        elif k == 'astr_out':
            cdefs.append(f'cdef const char *_c_{n} = NULL')
            cdefs.append(f'cdef int _c_{n}_len = 0')
            cargs += [f'&_c_{n}', f'&_c_{n}_len']
            results.append((a, f'_sn(_c_{n}, _c_{n}_len)'))
        elif k == 'strlist_out':
            cdefs.append(f'cdef const char *_c_{n} = NULL')
            cdefs.append(f'cdef int _c_{n}_n = 0')
            cargs += [f'&_c_{n}', f'&_c_{n}_n']
            results.append((a, f'_slist(_c_{n}, _c_{n}_n)'))
        elif k == 'num_in' and a in getattr(p, 'derived', {}):
            cdefs.append(f'cdef {t} _c_{n} = {p.derived[a]}')
            cargs.append(f'_c_{n}')
        elif k == 'num_in':
            if a.ftype == 'logical':
                cdefs.append(f'cdef int _c_{n} = 1 if {pn} else 0')
            elif n in ZERO_BASED_IN.get(p.name, ()) or (p.name.startswith('ek') and n in EK_INDICES):
                cdefs.append(f'cdef {t} _c_{n} = {pn} + 1')
            else:
                cdefs.append(f'cdef {t} _c_{n} = {pn}')
            cargs.append(f'_c_{n}')
        elif k == 'num_out':
            cdefs.append(f'cdef {t} _c_{n} = 0')
            cargs.append(f'&_c_{n}')
            if a is not found and n not in DROP_OUTPUTS.get(p.name, ()):
                if n in ZERO_BASED_OUT.get(p.name, ()) or (p.name.startswith('ek') and n in EK_INDICES):
                    results.append((a, f'(_c_{n} - 1)'))
                else:
                    results.append((a, f'(_c_{n} != 0)' if a.ftype == 'logical' else f'_c_{n}'))
        elif k == 'num_inout' and p.name.startswith('ek') and n in EK_INDICES:
            # an input in practice (CSPICE passes it by value), from 0
            cdefs.append(f'cdef {t} _c_{n} = {pn} + 1')
            cargs.append(f'&_c_{n}')
        elif k == 'num_inout':
            if a.ftype == 'logical':
                cdefs.append(f'cdef int _c_{n} = 1 if {pn} else 0')
                results.append((a, f'(_c_{n} != 0)'))
            else:
                cdefs.append(f'cdef {t} _c_{n} = {pn}')
                results.append((a, f'_c_{n}'))
            cargs.append(f'&_c_{n}')
        elif k.startswith('arr_') and a.rank == 2 and (a.name in ROWS.get(p.name, ()) or
                                                       (a.dims[0].isdigit() and not a.dims[1].isdigit())):
            # a list of vectors: Fortran (k, n) is the memory of rows (n, k)
            dk = a.dims[0]
            dn = extent(a.dims[1]) if extent(a.dims[1]).isdigit() else f'_c_{a.dims[1]}'
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            if k == 'arr_out':
                conv.append(f'cdef cnp.ndarray _a_{n} = np.empty(({dn}, {dk}), dtype=np.float64 if {dt} == NPY_DOUBLE else np.int32)')
                results.append((a, f'_a_{n}'))
            else:
                conv.append(f'cdef cnp.ndarray _a_{n} = _vrows({pn}, {dt}, {dk}, {dn}, {"True" if k == "arr_inout" else "False"})')
                if k == 'arr_inout':
                    results.append((a, f'_a_{n}'))
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
        elif k.startswith('arr_'):
            dims = cdims(a.dims)
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            if k == 'arr_out':
                conv.append(f'cdef cnp.ndarray _a_{n} = _empty({dt}, {dims}{"" if a.rank == 2 else ", -1"})')
                results.append((a, f'_a_{n}'))
            else:
                conv.append(f'cdef cnp.ndarray _a_{n} = _input({pn}, {dt}, {dims}{"" if a.rank == 2 else ", -1"}, {"True" if k == "arr_inout" else "False"})')
                if k == 'arr_inout':
                    results.append((a, f'_a_{n}'))
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
        elif k == 'cell':
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            if a.intent in ('out', 'inout'):
                others = [pymap.get(id(b), b.name) for b in allargs if b.kind == 'cell' and b is not a and id(b) in pymap]
                conv.append(f'if {pn} is None: {pn} = _autocell({dt}, [{", ".join(others)}])')
                results.append((a, pn))
            conv.append(f'cdef cnp.ndarray _a_{n} = _cellbuf({pn}, {dt}, {"True" if a.intent == "in" else "False"})')
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
        elif k == 'ptr_out':
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            size = csize(OUT_SIZES[p.name][a.name])
            conv.append(f'cdef cnp.ndarray _a_{n} = _empty({dt}, max(<int>({size}), 1), -1)')
            cargs.append(f'<{ct}*>cnp.PyArray_DATA(_a_{n})')
            results.append((a, f'_a_{n}'))
        elif k == 'ptr_in':
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            if n in ZERO_BASED_ARRAY_IN.get(p.name, ()):
                conv.append(f'cdef cnp.ndarray _a_{n} = _rows(np.asarray({pn}) + 1, {dt}, {a.rank})')
            else:
                conv.append(f'cdef cnp.ndarray _a_{n} = _rows({pn}, {dt}, {a.rank})')
            cargs.append(f'<const {ct}*>cnp.PyArray_DATA(_a_{n})')
        elif k == 'strarr_in':
            conv.append(f'cdef bytes _b_{n}')
            conv.append(f'cdef int _n_{n}, _w_{n}')
            conv.append(f'cdef const char *_p_{n}')
            conv.append(f'_b_{n}, _n_{n}, _w_{n} = _strblock({pn})')
            conv.append(f'_p_{n} = _b_{n}')
            cargs += [f'_n_{n}', f'_w_{n}', f'_p_{n}']
        elif k == 'shape_in':
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            ct = 'double' if a.ftype == 'real' else 'int'
            if a.rank == 1:
                conv.append(f'cdef cnp.ndarray _a_{n} = _rows({pn}, {dt}, 1)')
                cargs += [f'<int>cnp.PyArray_DIM(_a_{n}, 0)', f'<const {ct}*>cnp.PyArray_DATA(_a_{n})']
            else:
                conv.append(f'cdef cnp.ndarray _a_{n} = _rows({pn}, {dt}, 2)')
                cargs += [f'<int>cnp.PyArray_DIM(_a_{n}, 0)', f'<int>cnp.PyArray_DIM(_a_{n}, 1)',
                          f'<const {ct}*>cnp.PyArray_DATA(_a_{n})']
        elif k == 'alloc_out':
            ct = 'double' if a.ftype == 'real' else 'int'
            dt = 'NPY_DOUBLE' if a.ftype == 'real' else 'NPY_INT32'
            cdefs.append(f'cdef const {ct} *_c_{n} = NULL')
            if a.rank == 1:
                cdefs.append(f'cdef int _c_{n}_n = 0')
                cargs += [f'&_c_{n}', f'&_c_{n}_n']
                results.append((a, f'_copy1({dt}, _c_{n}, _c_{n}_n)'))
            else:
                cdefs.append(f'cdef int _c_{n}_rows = 0')
                cdefs.append(f'cdef int _c_{n}_cols = 0')
                cargs += [f'&_c_{n}', f'&_c_{n}_rows', f'&_c_{n}_cols']
                results.append((a, f'_copy2({dt}, _c_{n}, _c_{n}_rows, _c_{n}_cols)'))
    ordered = []
    # cdef declarations must precede other statements in Cython functions
    for c in cdefs:
        if ' = _b_' in c:
            continue
        ordered.append(c)
    body_decl = [c for c in ordered]
    body_decl.append('cdef int _st = 0')
    strptrs = [c for c in cdefs if ' = _b_' in c]
    lines = []
    lines += body_decl
    lines += [c for c in conv if c.startswith('cdef bytes')]
    lines += [c.replace('cdef const char *', 'cdef const char *') for c in strptrs]
    lines += [c for c in conv if not c.startswith('cdef bytes')]
    callexpr = f'{p.cname}({", ".join(cargs)})'
    lines = checks + lines
    if where == 'ctx':
        if hooks:
            lines.append('_hk = self._gf_hooks_on(' + ', '.join(f'{k}={v}' for k, v in hooks.items()) + ')')
        lines.append('self._enter()')
        lines.append('try:')
        if LONG_CALLS.search(p.name):
            lines.append('    with nogil:')
            lines.append(f'        _st = {callexpr}')
        else:
            lines.append(f'    _st = {callexpr}')
        lines.append('    if _st:')
        lines.append('        self._raise()')
        lines.append('finally:')
        if hooks:
            lines.append('    self._gf_hooks_off()')
        lines.append('    self._leave()')
    elif where == 'spice':
        lines.append('self._check()')
        lines.append('with self._lock:')
        lines.append('    with nogil:')
        lines.append(f'        _st = {callexpr}')
        lines.append('    if _st:')
        lines.append('        self._raise()')
    else:
        lines.append(f'{callexpr}')
    drop = set(DROP_OUTPUTS.get(p.name, ()))
    sl = SLICE.get(p.name, {})
    names_out = {a.name for a in allargs}
    ret = []
    for a, r in results:
        if a.name in drop and a.kind != 'num_out':
            continue
        if a.name in sl:
            m = sl[a.name]
            r = f'{r}[:{"_c_" + m if m in names_out else m}]'
        if a.wrap:
            r = f'{a.wrap}._from_array({r})'
        ret.append(r)
    if found:
        lines.append(f'return _found({p.name!r}, ({", ".join(ret)}{"," if len(ret) == 1 else ""}), _c_{found.name} != 0)')
    elif not ret:
        lines.append('return None')
    elif len(ret) == 1:
        lines.append(f'return {ret[0]}')
    else:
        lines.append(f'return {", ".join(ret)}')
    # vectorized check goes before cdefs: Cython allows cdef after statements in def functions
    doc = py_doc(p, params, aligned, outs, found)
    out = []
    ind = '    ' if where in ('ctx', 'spice') else ''
    out.append(f'{ind}def {p.name}({", ".join(selfp + sig)}):')
    out.append(f'{ind}    """{doc}"""'.replace('\n', '\n' + ind + '    '))
    for l in body + lines:
        out.append(f'{ind}    {l}')
    out.append('')
    return '\n'.join(out), params, aligned, vec


# The vectorized routines (SpiceyPy's cyice `_v` functions): the arguments
# looped over together (numbers, rows of vectors, or strings), broadcast as
# NumPy broadcasts (a single value is used for every element).
VECTORIZED = {
    'ckgp': ('sclkdp',), 'ckgpav': ('sclkdp',), 'convrt': ('x',), 'deltet': ('epoch',), 'et2lst': ('et',),
    'et2utc': ('et',), 'etcal': ('et',), 'fovray': ('et',), 'fovtrg': ('et',), 'lspcn': ('et',),
    'pxform': ('et',), 'scdecd': ('sclkdp',), 'scencd': ('sclkch',), 'sce2c': ('et',), 'sce2s': ('et',),
    'scs2e': ('sclkch',), 'sct2e': ('sclkdp',), 'spkapo': ('et',), 'spkcpo': ('et',), 'spkcpt': ('et',),
    'spkcvo': ('et',), 'spkcvt': ('et',), 'spkez': ('et',), 'spkezp': ('et',), 'spkezr': ('et',),
    'spkgeo': ('et',), 'spkgps': ('et',), 'spkpos': ('et',), 'spkpvn': ('et',), 'spkssb': ('et',),
    'str2et': ('timstr',), 'sincpt': ('et',), 'subpnt': ('et',), 'subslr': ('et',), 'sxform': ('et',),
    'timout': ('et',), 'trgsep': ('et',), 'unitim': ('epoch',), 'utc2et': ('utcstr',),
    'azlcpo': ('et', 'obspos'), 'occult': ('et',), 'phaseq': ('et',), 'tangpt': ('et',),
    'conics': ('elts', 'et'), 'evsgp4': ('et', 'elems'), 'oscelt': ('state', 'et'),
    'illumf': ('et', 'spoint'), 'illumg': ('et', 'spoint'), 'ilumin': ('et', 'spoint'),
    'xfmsta': ('istate',), 'srfrec': ('lon', 'lat'),
    'georec': ('lon', 'lat', 'alt'), 'pgrrec': ('lon', 'lat', 'alt'),
    'azlrec': ('range', 'az', 'el'), 'radrec': ('range', 'ra', 'dec'),
    'cyllat': ('r', 'clon', 'z'), 'cylrec': ('r', 'clon', 'z'), 'cylsph': ('r', 'clon', 'z'),
    'latcyl': ('radius', 'lon', 'lat'), 'latrec': ('radius', 'lon', 'lat'), 'latsph': ('radius', 'lon', 'lat'),
    'sphcyl': ('radius', 'colat', 'slon'), 'sphlat': ('r', 'colat', 'slon'), 'sphrec': ('r', 'colat', 'slon'),
    'recazl': ('rectan',), 'reccyl': ('rectan',), 'recgeo': ('rectan',), 'reclat': ('rectan',),
    'recpgr': ('rectan',), 'recrad': ('rectan',), 'recsph': ('rectan',),
}
# Routines evaluated at every pair (epoch, vector) of their two arrays, as
# cyice does: results (n epochs, m vectors, ...).
VEC_OUTER = {'azlcpo', 'illumf', 'illumg', 'ilumin'}
# Coordinates returned as an (n, 3) array (as cyice does), not three arrays.
VEC_STACKED = {'cyllat', 'cylsph', 'latcyl', 'latsph', 'recazl', 'reccyl', 'recgeo', 'reclat', 'recpgr',
               'recrad', 'recsph', 'sphcyl', 'sphlat'}
VEC_STR_SIZE = 256   # the buffer of each string result of a vectorized routine


def cython_vec_method(p, spy):
    """`<name>_v`: the routine at each element of arrays, in a C loop over
    PEPPER's C interface with the GIL released. The other arguments are
    those of the scalar function; the results are arrays whose first
    dimension is the number of elements (a found flag is returned last, as
    an array). (method code, module-level function), or None if the
    routine's arguments do not allow it."""
    vnames = VECTORIZED.get(p.name)
    if not vnames or p.owner not in ('ctx', 'pure'):
        return None
    allargs = p.args + ([p.result] if p.result else [])
    if any(a.kind not in ('str_in', 'num_in', 'arr_in', 'num_out', 'arr_out', 'str_out') for a in allargs):
        return None
    vecs = [next((a for a in p.args if a.name == v), None) for v in vnames]
    if None in vecs or any(a.kind not in ('num_in', 'str_in', 'arr_in') or (a.kind == 'arr_in' and a.rank != 1)
                           for a in vecs):
        return None
    params, aligned, _ = python_params(p, spy)
    pymap = {id(a): pn for pn, a, d in params if a is not None}
    if any(id(a) not in pymap for a in vecs):
        return None
    ctx = p.owner == 'ctx'
    outer = p.name in VEC_OUTER and len(vecs) == 2

    def index(a):
        """The element of a looped argument at step _i of the loop."""
        if not outer:
            return '_i'
        return '(_i // _n2)' if a is vecs[0] else '(_i % _n2)'

    checks, decl, conv, cargs, results = [], [], [], (['self._ptr'] if ctx else []), []
    sizes = []      # the number of elements of each looped argument
    found = None
    nout = []
    have_n = False
    stacked = None
    for a in allargs:
        n = a.name
        pn = pymap.get(id(a), n)
        k = a.kind
        t = CTYPE.get(a.ftype)
        ct = 'double' if a.ftype == 'real' else 'int'
        npt = 'np.float64' if a.ftype == 'real' else 'np.int32'
        if a in vecs and k == 'num_in':
            conv.append(f'cdef cnp.ndarray _v_{n} = np.ascontiguousarray({pn}, dtype={npt}).reshape(-1)')
            conv.append(f'cdef const {ct} *_pv_{n} = <const {ct}*>cnp.PyArray_DATA(_v_{n})')
            conv.append(f'cdef Py_ssize_t _s_{n} = 0 if _v_{n}.shape[0] == 1 else 1')
            sizes.append(f'_v_{n}.shape[0]')
            cargs.append(f'_pv_{n}[{index(a)} * _s_{n}]' + (' + 1' if n in ZERO_BASED_IN.get(p.name, ()) else ''))
        elif a in vecs and k == 'arr_in':
            m = extent(a.dims[0])
            conv.append(f'cdef cnp.ndarray _v_{n} = np.ascontiguousarray({pn}, dtype={npt})')
            conv.append(f'if _v_{n}.ndim == 1: _v_{n} = _v_{n}.reshape(1, -1)')
            conv.append(f'if _v_{n}.ndim != 2 or _v_{n}.shape[1] != {m}: '
                        f'raise ValueError("{pn}: expected vectors of {m} values")')
            conv.append(f'cdef const {ct} *_pv_{n} = <const {ct}*>cnp.PyArray_DATA(_v_{n})')
            conv.append(f'cdef Py_ssize_t _s_{n} = 0 if _v_{n}.shape[0] == 1 else {m}')
            sizes.append(f'_v_{n}.shape[0]')
            cargs.append(f'_pv_{n} + {index(a)} * _s_{n}')
        elif a in vecs:     # strings
            checks.append(f'if np.ndim({pn}) != 1: raise ValueError("{pn}: expected a 1-D array of strings")')
            conv.append(f'cdef bytes _bv_{n}')
            conv.append(f'cdef int _nv_{n}, _wv_{n}')
            conv.append(f'_bv_{n}, _nv_{n}, _wv_{n} = _strblock({pn})')
            conv.append(f'cdef const char *_pv_{n} = _bv_{n}')
            conv.append(f'cdef Py_ssize_t _s_{n} = 0 if _nv_{n} == 1 else _wv_{n}')
            sizes.append(f'_nv_{n}')
            cargs.append(f'_pv_{n} + _i * _s_{n}')
        elif k == 'str_in':
            if n in EMPTY_CHECKS.get(p.name, ()):
                checks.append(f'if not {pn}: _empty_string({p.name!r}, {n!r})')
            conv.append(f'cdef bytes _b_{n} = _b({pn})')
            conv.append(f'cdef const char *_c_{n} = _b_{n}')
            cargs.append(f'_c_{n}')
        elif k == 'num_in':
            if a.ftype == 'logical':
                decl.append(f'cdef int _c_{n} = 1 if {pn} else 0')
            elif n in ZERO_BASED_IN.get(p.name, ()):
                decl.append(f'cdef {t} _c_{n} = {pn} + 1')
            else:
                decl.append(f'cdef {t} _c_{n} = {pn}')
            cargs.append(f'_c_{n}')
        elif k == 'arr_in':
            conv.append(f'cdef cnp.ndarray _a_{n} = _input({pn}, {"NPY_DOUBLE" if a.ftype == "real" else "NPY_INT32"}, '
                        f'{cdims(a.dims)}{"" if a.rank == 2 else ", -1"}, False)')
            conv.append(f'cdef const {ct} *_p_{n} = <const {ct}*>cnp.PyArray_DATA(_a_{n})')
            cargs.append(f'_p_{n}')
        else:
            if not sizes:
                return None     # (outputs follow the inputs)
            if not have_n:
                if outer:
                    conv.append(f'cdef Py_ssize_t _n1 = {sizes[0]}')
                    conv.append(f'cdef Py_ssize_t _n2 = {sizes[1]}')
                    conv.append('cdef Py_ssize_t _n = _n1 * _n2')
                else:
                    conv.append(f'cdef Py_ssize_t _n = _vlen([{", ".join(sizes)}])')
                have_n = True
            if k == 'num_out' and p.name in VEC_STACKED:
                # the coordinates go straight into the columns of one (n, k) array
                if stacked is None:
                    stacked = [a2 for a2 in allargs if a2.kind == 'num_out']
                    conv.append(f'cdef cnp.ndarray _o_stack = np.empty((max(_n, 1), {len(stacked)}), dtype={npt})')
                    conv.append(f'cdef {t} *_q_stack = <{t}*>cnp.PyArray_DATA(_o_stack)')
                    results.append('_o_stack[:_n]')
                cargs.append(f'_q_stack + _i * {len(stacked)} + {stacked.index(a)}')
            elif k == 'num_out':
                conv.append(f'cdef cnp.ndarray _o_{n} = np.empty(max(_n, 1), dtype={npt})')
                conv.append(f'cdef {t} *_q_{n} = <{t}*>cnp.PyArray_DATA(_o_{n})')
                cargs.append(f'_q_{n} + _i')
                r = f'_o_{n}[:_n]'
                if a.ftype == 'logical':
                    r = f'({r} != 0)'
                elif n in ZERO_BASED_OUT.get(p.name, ()):
                    r = f'({r} - 1)'
                if n in ('found', 'isname') and a.ftype == 'logical':
                    found = r
                else:
                    results.append(r)
                    nout.append(a)
            elif k == 'arr_out':
                ext = [extent(d) for d in a.dims]
                if not all(x.isdigit() for x in ext):
                    return None
                size = math.prod(int(x) for x in ext)
                conv.append(f'cdef cnp.ndarray _o_{n} = np.empty((max(_n, 1), {size}), dtype={npt})')
                conv.append(f'cdef {ct} *_q_{n} = <{ct}*>cnp.PyArray_DATA(_o_{n})')
                cargs.append(f'_q_{n} + _i * {size}')
                if a.rank == 1:
                    results.append(f'_o_{n}[:_n]')
                else:   # Fortran's column-major matrices: (n, rows, columns)
                    results.append(f'np.ascontiguousarray(_o_{n}[:_n].reshape(_n, {ext[1]}, {ext[0]})'
                                   f'.transpose(0, 2, 1))')
            elif k == 'str_out':
                conv.append(f'cdef bytearray _ba_{n} = bytearray(max(_n, 1) * {VEC_STR_SIZE})')
                conv.append(f'cdef char *_q_{n} = _ba_{n}')
                cargs += [str(VEC_STR_SIZE), f'_q_{n} + _i * {VEC_STR_SIZE}']
                results.append(f'np.array(_strs(_ba_{n}, _n, {VEC_STR_SIZE}), dtype=np.str_)')
    if outer:   # (n epochs, m vectors, ...)
        results = [f'_outer({r}, _n1, _n2)' for r in results]
        found = f'_outer({found}, _n1, _n2)' if found else None
    numeric = [pymap[id(a)] for a in vecs if a.kind == 'num_in']
    if numeric and len(numeric) == len(vecs):
        checks.append(f'if {" and ".join(f"np.ndim({x}) == 0" for x in numeric)}: '
                      f'raise ValueError("expected an array (a scalar is {p.name}\'s)")')
    lines = checks + decl + ['cdef Py_ssize_t _i', 'cdef int _st = 0'] + conv
    call = f'{p.cname}({", ".join(cargs)})'
    if ctx:
        lines += ['self._enter()', 'try:', '    with nogil:', '        for _i in range(_n):',
                  f'            _st = {call}', '            if _st:', '                break',
                  '    if _st:', '        self._raise()', 'finally:', '    self._leave()']
    else:
        lines += ['with nogil:', '    for _i in range(_n):', f'        {call}']
    if found:   # SpiceyPy's (cyice's) found handling
        lines.append(f'return _found_v({p.name!r}, ({", ".join(results)},), {found})')
    else:
        lines.append(f'return {results[0]}' if len(results) == 1 else f'return {", ".join(results)}')
    # the looped arguments and the dimensions of one element, for the dispatch
    vinfo = [(pymap[id(a)], 1 if a.kind == 'arr_in' else 0) for a in vecs]
    vlist = ', '.join(f'`{pymap[id(a)]}`' for a in vecs)
    doc = (f'`{p.name}` at each element of {vlist} (arrays{", broadcast together" if len(vecs) > 1 else ""}), '
           f'in a C loop with the GIL released (SpiceyPy\'s cyice `{p.name}_v`).\n\n'
           f'The other arguments are those of `{p.name}`. The results are arrays whose first dimension is the '
           f'number of elements. {"A found flag false at any element raises NotFoundError (the flags are returned last under `no_found_check`)." if found else ""}'.rstrip())
    psig = ', '.join(f'{pn}={d}' if d is not None else pn for pn, a, d in params)
    if ctx:
        out = [f'    def {p.name}_v({", ".join(["self"] + ([psig] if psig else []))}):',
               f'        """{doc}"""'.replace('\n', '\n        ')]
        out += ['        ' + l for l in lines]
        out.append('')
        module = (f'def {p.name}_v({psig}):\n'
                  f'    """{doc}"""\n'
                  f'    return _context().{p.name}_v({", ".join(pn for pn, a, d in params)})\n')
        return '\n'.join(out), module, vinfo
    out = [f'def {p.name}_v({psig}):', f'    """{doc}"""'.replace('\n', '\n    ')]
    out += ['    ' + l for l in lines]
    out.append('')
    return None, '\n'.join(out), vinfo


def dispatcher(p, params, vinfo, scalar, doc):
    """`<name>`: `<name>_v` when a looped argument is an array of elements,
    else the scalar routine (`scalar`: the call expression's function), as
    SpiceyPy's cyice dispatches."""
    sig = ', '.join(f'{pn}={d}' if d is not None else pn for pn, a, d in params)
    looped = {pn for pn, d in vinfo}
    call = ', '.join(pn for pn, a, d in params)
    scall = ', '.join(f'_sc({pn})' if pn in looped else pn for pn, a, d in params)
    test = ' or '.join(f'_isvec({pn}, {d})' for pn, d in vinfo)
    names = ', '.join(f'`{pn}`' for pn, d in vinfo)
    doc = doc.rstrip() + (f'\n\n    With arrays for {names}: `{p.name}_v` (PepperyPy\'s, and SpiceyPy\'s cyice, '
                          f'dispatch); `{p.name}_s` is always the scalar form.')
    return (f'def {p.name}({sig}):\n'
            f'    """{doc}"""\n'
            f'    if {test}:\n'
            f'        return {p.name}_v({call})\n'
            f'    return {scalar}({scall})\n')


def inline_dispatcher(p, params, vinfo, code):
    """`dispatcher` for a routine without a context: the scalar function's
    own body after the test (no second Python call)."""
    a = code.index('"""') + 3
    b = code.index('"""', a)
    names = ', '.join(f'`{pn}`' for pn, d in vinfo)
    extra = (f'\n\n    With arrays for {names}: `{p.name}_v` (PepperyPy\'s, and SpiceyPy\'s cyice, '
             f'dispatch); `{p.name}_s` is always the scalar form.')
    call = ', '.join(pn for pn, a_, d in params)
    test = ' or '.join(f'_isvec({pn}, {d})' for pn, d in vinfo)
    lines = [f'    if {test}:', f'        return {p.name}_v({call})']
    lines += [f'    {pn} = _sc({pn})' for pn, d in vinfo]
    return code[:b].rstrip() + extra + '"""\n' + '\n'.join(lines) + code[b + 3:]


def module_function(p, params, where, doc):
    sig = []
    for pn, a, d in params:
        sig.append(f'{pn}={d}' if d is not None else pn)
    call = ', '.join(pn for pn, a, d in params)
    getter = '_context()' if where == 'ctx' else '_instance()'
    return (f'def {p.name}({", ".join(sig)}):\n'
            f'    """{doc}"""\n'
            f'    return {getter}.{p.name}({call})\n')


# ----------------------------------------------------------------------


def main():
    ctx = parse_context_interfaces()
    spice = parse_spice_methods()
    math_ = {}
    for mod in PURE_MODULES:
        for name, p in parse_math(mod).items():
            math_.setdefault(name, p)
    consts = parse_constants()
    spy = spiceypy_info()

    wrapped = {}
    skipped = {}
    for group, procs, excl in (('ctx', ctx, EXCLUDE_CTX), ('spice', spice, EXCLUDE_SPICE), ('pure', math_, set())):
        for name, p in sorted(procs.items()):
            if (name.startswith('zz') and name not in ZZ_WRAPPED) or name in excl or keyword.iskeyword(name):
                continue
            if name in wrapped:
                continue
            why = classify(p)
            if why:
                skipped[name] = why
                continue
            wrapped[name] = p

    # Fortran
    shims = [fortran_shim(p) for p in wrapped.values()]
    f90 = ['!> PEPPER\'s C interface: the shims of the routines (generated by',
           '!> tools/gen_capi.py from the interfaces; do not edit). See',
           '!> [[pepper_capi_core]] for the conventions, and include/pepper.h.',
           'module pepper_capi',
           '',
           '   use, intrinsic :: iso_c_binding',
           '   use pepper_kinds, only: ip, wp',
           '   use pepper_containers, only: string',
           '   use pepper_instance_module, only: pepper_instance',
           '   use pepper_capi_core',
           '   use pepper_math',
           '   use pepper_strings, only: ' + ', '.join(sorted(p.impl for p in wrapped.values()
                                                            if getattr(p, 'module', '') == 'pepper_strings')),
           '   use pepper_arrays, only: ' + ', '.join(sorted(p.impl for p in wrapped.values()
                                                           if getattr(p, 'module', '') == 'pepper_arrays')),
           '   implicit none',
           '',
           'contains',
           '']
    f90 += shims
    f90.append('end module pepper_capi')
    (ROOT / 'src/capi/pepper_capi.f90').write_text('\n'.join(f90) + '\n')

    # C header
    core_h = (ROOT / 'tools/pepper_core.h').read_text()
    h = [core_h.rstrip(), '', '/* ---- The routines (generated) ---- */', '']
    for p in wrapped.values():
        summary = ' '.join(p.doc).split('. ')[0].rstrip('.') if p.doc else p.name
        h.append(f'/* {p.name}: {summary.replace("*/", "")}. */')
        h.append(f'int {p.cname}({", ".join(c_params(p))});')
    h += ['', '#ifdef __cplusplus', '}', '#endif', '', '#endif /* PEPPER_H */', '']
    (ROOT / 'include').mkdir(exist_ok=True)
    (ROOT / 'include/pepper.h').write_text('\n'.join(h))

    # Cython declarations
    pkg = ROOT / 'python/src/pepperypy'
    pkg.mkdir(parents=True, exist_ok=True)
    pxd = ['# Generated by tools/gen_capi.py from include/pepper.h; do not edit.',
           'from libc.stdint cimport int64_t',
           'cdef extern from "pepper.h" nogil:']
    flat = re.sub(r'/\*.*?\*/', '', core_h, flags=re.S)
    flat = re.sub(r',\s*\n\s*', ', ', flat)
    for line in flat.split('\n'):
        line = line.strip()
        m = re.match(r'^(\w[\w\s\*]*?\b)(\w+)\s*\((.*)\);', line)
        if m and not line.startswith('typedef'):
            pxd.append(f'    {m.group(1)}{m.group(2)}({"" if m.group(3) == "void" else m.group(3)})')
        m = re.match(r'^typedef (.*)\(\*(\w+)\)\((.*)\);', line)
        if m:
            pxd.append(f'    ctypedef {m.group(1).strip()} (*{m.group(2)})({m.group(3)}) noexcept')
    for p in wrapped.values():
        pxd.append(f'    int {p.cname}({", ".join(c_params(p))})')
    # typedefs before use
    pxd = pxd[:3] + [l for l in pxd[3:] if 'ctypedef' in l] + [l for l in pxd[3:] if 'ctypedef' not in l]
    (pkg / '_pepper.pxd').write_text('\n'.join(pxd) + '\n')

    ctx_methods = ['# Generated by tools/gen_capi.py; do not edit. Included in _core.pyx.', '',
                   'cdef class _ContextRoutines(_ContextBase):',
                   '    """The SPICELIB and PEPPER routines of a context (generated)."""', '']
    spice_methods = ['# Generated by tools/gen_capi.py; do not edit. Included in _core.pyx.', '',
                     'cdef class _SpiceRoutines(_SpiceBase):',
                     '    """The kernel changes of an instance (generated)."""', '']
    functions = ['# Generated by tools/gen_capi.py; do not edit. The module-level functions.', '']
    info = {}
    vectorized = []
    for p in wrapped.values():
        if p.name in NO_PYTHON:
            continue
        code, params, aligned, vec = cython_method(p, spy, p.owner)
        info[p.name] = (p, aligned, vec)
        doc = py_doc(p, params, aligned, None, None)
        if p.owner == 'ctx':
            ctx_methods.append(code)
            fdoc = code.split('"""')[1].replace('\n        ', '\n    ')
            v = cython_vec_method(p, spy)
            if v:
                ctx_methods.append(v[0])
                functions.append(v[1])
                # <name>_s, and <name> dispatching to <name>_s or <name>_v
                functions.append(module_function(p, params, 'ctx', fdoc).replace(
                    f'def {p.name}(', f'def {p.name}_s(', 1))
                functions.append(dispatcher(p, params, v[2], f'_context().{p.name}', fdoc))
                vectorized.append(p.name)
            else:
                functions.append(module_function(p, params, 'ctx', fdoc))
        elif p.owner == 'spice':
            spice_methods.append(code)
            functions.append(module_function(p, params, 'spice', code.split('"""')[1].replace('\n        ', '\n    ')))
        else:
            v = cython_vec_method(p, spy)
            if v:
                functions.append(code.replace(f'def {p.name}(', f'def {p.name}_s(', 1))
                functions.append(v[1])
                functions.append(inline_dispatcher(p, params, v[2], code))
                vectorized.append(p.name)
            else:
                functions.append(code)
    for name, (value, doc) in sorted(consts.items()):
        functions.append(f'def {name}():\n    """{doc or name} (a constant)."""\n    return {value!r}\n')
    (pkg / '_context_methods.pxi').write_text('\n'.join(ctx_methods))
    (pkg / '_spice_methods.pxi').write_text('\n'.join(spice_methods))
    (pkg / '_functions.pxi').write_text('\n'.join(functions))

    # names for the coverage table
    names = sorted(wrapped) + sorted(consts)
    (pkg / '_generated_names.py').write_text(
        '# Generated by tools/gen_capi.py; do not edit.\n'
        '#: The functions generated from PEPPER\'s interfaces.\n'
        f'GENERATED = {names!r}\n'
        '#: Those whose Python signature follows SpiceyPy\'s.\n'
        f'ALIGNED = {sorted(n for n, (p, al, v) in info.items() if al)!r}\n'
        '#: The routines with a vectorized form (<name>_v, a C loop).\n'
        f'VECTORIZED = {sorted(vectorized)!r}\n'
        '#: Not generated, and why.\n'
        f'SKIPPED = {dict(sorted(skipped.items()))!r}\n')
    print(f'{len(wrapped)} wrapped ({sum(1 for p in wrapped.values() if p.owner == "ctx")} context, '
          f'{sum(1 for p in wrapped.values() if p.owner == "spice")} instance, '
          f'{sum(1 for p in wrapped.values() if p.owner == "pure")} math), '
          f'{len(consts)} constants, {len(skipped)} skipped')


if __name__ == '__main__':
    main()
