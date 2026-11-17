# PepperyPy: PEPPER's Python interface (plans/pepperypy_plan.md), built by
# meson-python (pyproject.toml). PEPPER and its C interface (src/, src/capi)
# become a static library, linked into the Cython extension module.
#
# fpm stays the build of PEPPER itself (fpm.toml); this file builds the same
# sources for the Python package (and a C test of the C interface).
#
#   pixi run -e python py-build    # editable install
#   pixi run -e python py-test
project('pepperypy', 'c', 'cython', 'fortran',
        version: '0.1.0',
        meson_version: '>= 1.6',
        default_options: ['buildtype=release', 'b_ndebug=if-release', 'warning_level=0'])

py = import('python').find_installation(pure: false)
fc = meson.get_compiler('fortran')

# -frecursive (gfortran) / -recursive (Intel): locals on the stack, for
# reentrant, thread-safe code; -ffp-contract=off: no fused multiply-adds,
# SPICELIB's roundings (see fpm.toml).
add_project_arguments(fc.get_supported_arguments(['-frecursive', '-recursive', '-ffp-contract=off']), language: 'fortran')

# All of src/ (meson has no globbing; reconfigure after adding a source).
# (src/legacy, SPICELIB's interface, is not needed by the Python package)
pepper_sources = run_command(py, '-c',
  'import glob; print("\\n".join(sorted(f for f in glob.glob("src/**/*.f90", recursive=True) if "/legacy/" not in f)))',
  check: true).stdout().strip().split('\n')

pepper_lib = static_library('pepper', pepper_sources, pic: true)

incdir = include_directories('include')

# gfortran: programs whose main is not Fortran call pepper_runtime_init()
# (include/pepper.h), defined for gfortran by PEPPER_GFORTRAN.
capi_args = fc.get_id() == 'gcc' ? ['-DPEPPER_GFORTRAN'] : []

# The C interface's own test (C, no Python).
test_capi = executable('test_capi', 'capi/test_capi.c',
                       include_directories: incdir, link_with: pepper_lib, c_args: capi_args,
                       install: false)
test('capi', test_capi, workdir: meson.project_source_root())

# ---- the Python package
numpy_dep = dependency('numpy')

py.extension_module('_core',
  'python/src/pepperypy/_core.pyx',
  include_directories: [incdir, include_directories('python/src/pepperypy')],
  dependencies: [numpy_dep],
  link_with: pepper_lib,
  c_args: ['-DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION'] + capi_args,
  cython_args: ['--directive', 'language_level=3'],
  install: true,
  subdir: 'pepperypy')

py.install_sources(
  'python/src/pepperypy/__init__.py',
  'python/src/pepperypy/_compat.py',
  'python/src/pepperypy/_generated_names.py',
  'python/src/pepperypy/_version.py',
  'python/src/pepperypy/cells.py',
  'python/src/pepperypy/config.py',
  'python/src/pepperypy/exceptions.py',
  'python/src/pepperypy/found_catcher.py',
  'python/src/pepperypy/py.typed',
  subdir: 'pepperypy')

py.install_sources(
  'python/src/pepperypy/utils/__init__.py',
  'python/src/pepperypy/utils/callbacks.py',
  'python/src/pepperypy/utils/exceptions.py',
  'python/src/pepperypy/utils/support_types.py',
  subdir: 'pepperypy/utils')
