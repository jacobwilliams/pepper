#!/usr/bin/env node
import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import fs from "node:fs";
import fsp from "node:fs/promises";
import https from "node:https";
import { createRequire } from "node:module";
import os from "node:os";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { promisify } from "node:util";

import * as esbuild from "esbuild";
import { FlatcRunner } from "flatc-wasm";
import {
  SDS_MANIFEST_SECTION_NAME,
  appendWasmCustomSection,
} from "space-data-module-sdk/bundle";
import {
  ModuleThreadModel,
  assertSequentialArtifact,
  assertSimdArtifact,
  deriveGuestLinkObject,
  getFlatbuffersCppRuntimeHeaders,
  getInvokeCppSchemaHeaders,
  resolveWasiThreadsToolchain,
  writeGuestLinkFiles,
} from "space-data-module-sdk/compiler";
import { validateArtifactWithStandards } from "space-data-module-sdk/compliance";
import { runConformance } from "space-data-module-sdk/conformance";
import { generateEmbeddedManifestSource } from "space-data-module-sdk/embedded-manifest";
import { createBrowserWasiShim } from "space-data-module-sdk/host/wasi-shim";
import { encodePluginManifest } from "space-data-module-sdk/manifest";

const execFileAsync = promisify(execFile);
const require = createRequire(import.meta.url);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, "..");

function findPackageRoot(specifier) {
  let dir = path.dirname(require.resolve(specifier));
  while (!fs.existsSync(path.join(dir, "package.json"))) {
    const parent = path.dirname(dir);
    if (parent === dir) throw new Error(`Could not locate package.json for ${specifier}`);
    dir = parent;
  }
  return dir;
}

const SDK_ROOT = findPackageRoot("space-data-module-sdk");
const SDS_ROOT = findPackageRoot("spacedatastandards.org");

const { generateInvokeSupportHeader, generateInvokeSupportSource } = await import(
  pathToFileURL(path.join(SDK_ROOT, "src", "compiler", "invokeGlue.js")).href
);

import { ensureLlvmSource, verifyDownload } from "./toolchain-downloads.mjs";

const FLANG_RELEASE_URL =
  "https://github.com/miinso/flang-releases/releases/download/v21.1.8/flang+llvm-21.1.8-arm64-apple-darwin.tar.xz";
const FLANG_RELEASE_SHA256 =
  "89cbbe09bb35fac6ec1781b74637e40a58eddc89475d6a6a1e49c0b15a6c5eb5";

function sha256Hex(buf) {
  return createHash("sha256").update(buf).digest("hex");
}

function downloadFile(url, destPath) {
  return new Promise((resolve, reject) => {
    const request = (targetUrl, redirects = 0) => {
      if (redirects > 6) {
        reject(new Error(`Too many redirects fetching ${url}`));
        return;
      }
      https
        .get(targetUrl, (res) => {
          if (
            res.statusCode &&
            res.statusCode >= 300 &&
            res.statusCode < 400 &&
            res.headers.location
          ) {
            res.resume();
            request(new URL(res.headers.location, targetUrl).toString(), redirects + 1);
            return;
          }
          if (res.statusCode !== 200) {
            res.resume();
            reject(new Error(`HTTP ${res.statusCode} downloading ${targetUrl}`));
            return;
          }
          const out = fs.createWriteStream(destPath);
          res.pipe(out);
          out.on("finish", () => out.close(resolve));
          out.on("error", reject);
        })
        .on("error", reject);
    };
    request(url);
  });
}

async function ensureFlangWasmToolchain() {
  const envDir = process.env.FLANG_WASM_DIR;
  if (envDir && fs.existsSync(path.join(envDir, "bin", "flang"))) {
    return envDir;
  }
  const cacheDir = path.join(ROOT, ".cache", "flang-wasm");
  if (fs.existsSync(path.join(cacheDir, "bin", "flang"))) {
    return cacheDir;
  }
  if (process.platform !== "darwin" || process.arch !== "arm64") {
    throw new Error(
      "Set FLANG_WASM_DIR to an LLVM Flang installation supporting --target=wasm32-unknown-wasi.",
    );
  }
  await fsp.mkdir(cacheDir, { recursive: true });
  const archivePath = path.join(ROOT, ".cache", "flang-wasm.tar.xz");
  if (!fs.existsSync(archivePath)) {
    console.log("[wasm] Downloading Flang v21.1.8 WASM toolchain...");
    await downloadFile(FLANG_RELEASE_URL, archivePath);
  }
  const actualSha = sha256Hex(await fsp.readFile(archivePath));
  if (actualSha !== FLANG_RELEASE_SHA256) {
    throw new Error(
      `Flang archive SHA-256 mismatch: expected ${FLANG_RELEASE_SHA256}, got ${actualSha}`,
    );
  }
  await execFileAsync("tar", ["-xf", archivePath, "--strip-components=1", "-C", cacheDir]);
  return cacheDir;
}

async function ensureWasiFlangRuntime(flangDir, tc) {
  const envArchive = process.env.FLANG_WASM_RT_ARCHIVE;
  if (envArchive && fs.existsSync(envArchive)) {
    return envArchive;
  }
  const cacheDir = path.join(ROOT, ".cache", "flang-wasm");
  const archivePath = path.join(cacheDir, "libflang_rt.wasi.a");
  const runtimeSizeMarker = path.join(cacheDir, ".pepper-runtime-findloc-oz-v1");
  const wasm32ModMarkerCheck = path.join(flangDir, "include", "flang", ".pepper-wasm32-mods-v1");
  if (fs.existsSync(archivePath) && fs.existsSync(wasm32ModMarkerCheck) && fs.existsSync(runtimeSizeMarker)) {
    return archivePath;
  }

  console.log("[wasm] Building native WASI LLVM Flang 21 runtime (libflang_rt.wasi.a)...");
  const srcRoot = path.join(cacheDir, "flang-rt-src");
  const outDir = path.join(cacheDir, "flang-rt-build");
  await fsp.mkdir(cacheDir, { recursive: true });

  if (!fs.existsSync(path.join(srcRoot, "flang-rt", "lib", "runtime"))) {
    await fsp.rm(srcRoot, { recursive: true, force: true });
    const sourceRoot = await ensureLlvmSource();
    for (const rel of ["flang-rt", "flang/include/flang", "flang/lib/Decimal", "flang/module"]) {
      await fsp.cp(path.join(sourceRoot, rel), path.join(srcRoot, rel), { recursive: true });
    }
  }

  // Ensure 32-bit wasm32 intrinsic .mod files (c_ptr/c_funptr/c_size_t = 32-bit)
  // so Flang emits R_WASM_TABLE_INDEX_I32 relocations for vtables & FINAL bindings.
  const incFlangDir = path.join(flangDir, "include", "flang");
  const wasm32ModMarker = path.join(incFlangDir, ".pepper-wasm32-mods-v1");
  if (!fs.existsSync(wasm32ModMarker)) {
    console.log("[wasm] Compiling 32-bit wasm32 intrinsic .mod files...");
    const origBackup = path.join(flangDir, "include", "flang-host-orig");
    if (!fs.existsSync(origBackup)) {
      await fsp.cp(incFlangDir, origBackup, { recursive: true });
    }
    const modSrcDir = path.join(srcRoot, "flang", "module");
    const patchedModDir = path.join(cacheDir, "wasm32-mod-src");
    await fsp.mkdir(patchedModDir, { recursive: true });

    let builtinsSrc = await fsp.readFile(path.join(modSrcDir, "__fortran_builtins.f90"), "utf8");
    builtinsSrc = builtinsSrc
      .replace(
        "  integer, parameter :: int64 = selected_int_kind(18)\n\n  type, bind(c), public :: __builtin_c_ptr\n    integer(kind=int64), private :: __address\n  end type\n\n  type, bind(c), public :: __builtin_c_funptr\n    integer(kind=int64), private :: __address\n  end type",
        "  integer, parameter :: int64 = selected_int_kind(18)\n  integer, parameter :: intptr = selected_int_kind(9)\n\n  type, bind(c), public :: __builtin_c_ptr\n    integer(kind=intptr), private :: __address\n  end type\n\n  type, bind(c), public :: __builtin_c_funptr\n    integer(kind=intptr), private :: __address\n  end type",
      )
      .replace(
        "    __builtin_c_funloc = __builtin_c_funptr(loc(x))",
        "    __builtin_c_funloc = __builtin_c_funptr(int(loc(x), kind=intptr))",
      );
    await fsp.writeFile(path.join(patchedModDir, "__fortran_builtins.f90"), builtinsSrc);

    let typeInfoSrc = await fsp.readFile(path.join(modSrcDir, "__fortran_type_info.f90"), "utf8");
    typeInfoSrc = typeInfoSrc.replace(
      "    integer(1) :: __padding0(4)\n    type(__builtin_c_funptr) :: proc",
      "    type(__builtin_c_funptr) :: proc",
    );
    await fsp.writeFile(path.join(patchedModDir, "__fortran_type_info.f90"), typeInfoSrc);

    let isoCSrc = await fsp.readFile(path.join(modSrcDir, "iso_c_binding.f90"), "utf8");
    isoCSrc = isoCSrc
      .replace(
        "    c_long = c_int64_t, &\n    c_long_long = c_int64_t, &\n    c_signed_char = c_int8_t, &\n    c_size_t = kind(c_sizeof(1)), &",
        "    c_long = c_int32_t, &\n    c_long_long = c_int64_t, &\n    c_signed_char = c_int8_t, &\n    c_size_t = c_int32_t, &",
      )
      .replace("    c_unsigned_long = c_uint64_t,", "    c_unsigned_long = c_uint32_t,");
    await fsp.writeFile(path.join(patchedModDir, "iso_c_binding.f90"), isoCSrc);

    const modNames = [
      "__fortran_builtins.mod",
      "__fortran_type_info.mod",
      "iso_c_binding.mod",
      "iso_fortran_env_impl.mod",
      "iso_fortran_env.mod",
      "__fortran_ieee_exceptions.mod",
      "ieee_exceptions.mod",
      "ieee_features.mod",
      "ieee_arithmetic.mod",
    ];
    for (const m of modNames) {
      await fsp.rm(path.join(incFlangDir, m), { force: true });
    }
    const flangBin = path.join(flangDir, "bin", "flang");
    const modBuildOrder = [
      path.join(patchedModDir, "__fortran_builtins.f90"),
      path.join(patchedModDir, "__fortran_type_info.f90"),
      path.join(patchedModDir, "iso_c_binding.f90"),
      path.join(modSrcDir, "iso_fortran_env_impl.f90"),
      path.join(modSrcDir, "iso_fortran_env.f90"),
      path.join(modSrcDir, "__fortran_ieee_exceptions.f90"),
      path.join(modSrcDir, "ieee_exceptions.f90"),
      path.join(modSrcDir, "ieee_features.f90"),
      path.join(modSrcDir, "ieee_arithmetic.f90"),
    ];
    for (const modSrcPath of modBuildOrder) {
      await execFileAsync(flangBin, [
        "--target=wasm32-unknown-wasi",
        "-cpp",
        `-I${path.join(srcRoot, "flang", "include")}`,
        "-fintrinsic-modules-path",
        incFlangDir,
        `-J${incFlangDir}`,
        "-fsyntax-only",
        modSrcPath,
      ]);
    }
    await fsp.writeFile(wasm32ModMarker, "ok\n");
  }

  // Patch 64-bit CFI descriptor dimensions/elem_len to match LLVM Flang 21 wasm32 codegen
  const isoPath = path.join(srcRoot, "flang", "include", "flang", "ISO_Fortran_binding.h");
  let iso = await fsp.readFile(isoPath, "utf8");
  if (!iso.includes("PEPPER_WASM64_DESC")) {
    iso = iso
      .replace("#include <stddef.h>\n", "#include <stddef.h>\n#include <stdint.h>\n")
      .replace(
        "typedef ptrdiff_t CFI_index_t;",
        "/* PEPPER_WASM64_DESC */ typedef long long CFI_index_t;",
      )
      .replace(
        "size_t elem_len; /* element size in bytes */",
        "unsigned long long elem_len; /* element size in bytes */",
      );
    await fsp.writeFile(isoPath, iso);
  }

  const fsToolsPath = path.join(
    srcRoot,
    "flang",
    "include",
    "flang",
    "Runtime",
    "freestanding-tools.h",
  );
  let fst = await fsp.readFile(fsToolsPath, "utf8");
  if (!fst.includes("#include <cstdlib>")) {
    fst = fst.replace("#include <cstring>", "#include <cstdlib>\n#include <cstring>");
    await fsp.writeFile(fsToolsPath, fst);
  }

  const ioApiPath = path.join(srcRoot, "flang", "include", "flang", "Runtime", "io-api.h");
  let ioApi = await fsp.readFile(ioApiPath, "utf8");
  ioApi = ioApi.replace(
    "using InquiryKeywordHash = std::uint64_t;",
    "using InquiryKeywordHash = std::size_t;",
  );
  await fsp.writeFile(ioApiPath, ioApi);

  const execCppPath = path.join(srcRoot, "flang-rt", "lib", "runtime", "execute.cpp");
  let execCpp = await fsp.readFile(execCppPath, "utf8");
  if (!execCpp.includes("defined(__wasi__)")) {
    execCpp = execCpp
      .replace(
        "#include <sys/wait.h>",
        "#ifndef __wasi__\n#include <sys/wait.h>\n#elif !defined(WEXITSTATUS)\n#define WEXITSTATUS(s) ((s) & 0xff)\n#endif",
      )
      .replace(
        "    pid_t pid{fork()};",
        "#ifdef __wasi__\n    errno = ENOSYS;\n    if (!cmdstat) terminator.Crash(\"Command line execution is not supported on WASI.\");\n#else\n    pid_t pid{fork()};",
      )
      .replace("      exit(status);\n    }\n#endif", "      exit(status);\n    }\n#endif\n#endif");
    await fsp.writeFile(execCppPath, execCpp);
  }

  const extCppPath = path.join(srcRoot, "flang-rt", "lib", "runtime", "extensions.cpp");
  let extCpp = await fsp.readFile(extCppPath, "utf8");
  if (!extCpp.includes("defined(__wasi__)")) {
    extCpp = extCpp
      .replaceAll("#ifdef _WIN32\n  // Group IDs", "#if defined(_WIN32) || defined(__wasi__)\n  // Group IDs")
      .replaceAll("#ifdef _WIN32\n  // User IDs", "#if defined(_WIN32) || defined(__wasi__)\n  // User IDs")
      .replace(
        "#if _REENTRANT || _POSIX_C_SOURCE >= 199506L",
        "#if (_REENTRANT || _POSIX_C_SOURCE >= 199506L) && !defined(__wasi__)",
      )
      .replace(
        "  if (gethostname(hn, length) < 0) {",
        "#ifdef __wasi__\n  if ((errno = ENOSYS, true)) {\n#else\n  if (gethostname(hn, length) < 0) {\n#endif",
      );
    await fsp.writeFile(extCppPath, extCpp);
  }

  const fileCppPath = path.join(srcRoot, "flang-rt", "lib", "runtime", "file.cpp");
  let fileCpp = await fsp.readFile(fileCppPath, "utf8");
  if (!fileCpp.includes("defined(__wasi__)")) {
    fileCpp = fileCpp.replace(
      "  char path[]{\"/tmp/Fortran-Scratch-XXXXXX\"};\n  int fd{::mkstemp(path)};",
      "  char path[]{\"/tmp/Fortran-Scratch-XXXXXX\"};\n#ifdef __wasi__\n  errno = ENOSYS; int fd{-1};\n#else\n  int fd{::mkstemp(path)};\n#endif",
    );
    await fsp.writeFile(fileCppPath, fileCpp);
  }

  await fsp.mkdir(path.join(outDir, "include", "flang-rt", "runtime"), { recursive: true });
  await fsp.writeFile(
    path.join(outDir, "include", "flang-rt", "runtime", "config.h"),
    "#pragma once\n#define FLANG_LITTLE_ENDIAN 1\n",
  );

  const rtDir = path.join(srcRoot, "flang-rt", "lib", "runtime");
  const decDir = path.join(srcRoot, "flang", "lib", "Decimal");
  const exclude = new Set(["main.c", "stop-coat.cpp"]);
  const sources = [
    ...fs
      .readdirSync(rtDir)
      .filter((f) => (f.endsWith(".cpp") || f.endsWith(".c")) && !exclude.has(f))
      .map((f) => path.join(rtDir, f)),
    ...fs
      .readdirSync(decDir)
      .filter((f) => f.endsWith(".cpp"))
      .map((f) => path.join(decDir, f)),
  ];
  const includes = [
    `-I${path.join(outDir, "include")}`,
    `-I${path.join(outDir, "include", "flang-rt", "runtime")}`,
    `-I${path.join(srcRoot, "flang-rt", "include")}`,
    `-I${rtDir}`,
    `-I${path.join(srcRoot, "flang", "include")}`,
  ];
  const objs = [];
  await runPool(sources, Math.max(1, Math.min(4, os.availableParallelism())), async (src) => {
    const isC = src.endsWith(".c");
    const obj = path.join(outDir, `${path.basename(src)}.o`);
    objs.push(obj);
    const isFindloc = path.basename(src) === "findloc.cpp";
    if (fs.existsSync(obj) && (!isFindloc || fs.existsSync(runtimeSizeMarker))) return;
    await execFileAsync(isC ? tc.clang : tc.clangxx, [
      ...tc.toolchainArgs,
      isFindloc ? "-Oz" : "-O2",
      "-mbulk-memory",
      "-fno-exceptions",
      ...(isC ? ["-std=c11"] : ["-std=c++17", "-fno-rtti", "-Wno-c++11-narrowing"]),
      "-D_WASI_EMULATED_SIGNAL",
      "-D_WASI_EMULATED_PROCESS_CLOCKS",
      "-D_WASI_EMULATED_GETPID",
      ...includes,
      "-c",
      src,
      "-o",
      obj,
    ]);
  });

  const arBin = fs.existsSync(path.join(flangDir, "bin", "llvm-ar"))
    ? path.join(flangDir, "bin", "llvm-ar")
    : "ar";
  await fsp.rm(archivePath, { force: true });
  await execFileAsync(arBin, ["rcs", archivePath, ...objs.sort()]);
  await fsp.writeFile(runtimeSizeMarker, "findloc.cpp: -Oz\n");
  return archivePath;
}

function discoverFortranWaves(srcDir) {
  const files = [];
  const walk = (dir) => {
    for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
      const full = path.join(dir, ent.name);
      if (ent.isDirectory()) {
        if (ent.name === "legacy") continue;
        walk(full);
      } else if (ent.name.endsWith(".f90")) {
        files.push(full);
      }
    }
  };
  walk(srcDir);
  files.sort();

  const providers = new Map();
  const parsed = files.map((file) => {
    const src = fs.readFileSync(file, "utf8");
    const defs = [];
    const uses = new Set();
    for (const line of src.split("\n")) {
      const stripped = line.replace(/!.*$/, "").trim();
      if (!stripped) continue;
      const modMatch = stripped.match(/^module\s+([a-z0-9_]+)\s*$/i);
      if (modMatch && modMatch[1].toLowerCase() !== "procedure") {
        defs.push(modMatch[1].toLowerCase());
      }
      const subMatch = stripped.match(/^submodule\s*\(\s*([a-z0-9_:]+)\s*\)\s*([a-z0-9_]+)/i);
      if (subMatch) {
        const parent = subMatch[1].toLowerCase().split(":").pop();
        uses.add(parent);
        defs.push(subMatch[2].toLowerCase());
      }
      const useMatch = stripped.match(/^use(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*::|\s+|\s*::\s*)([a-z0-9_]+)/i);
      if (useMatch) {
        uses.add(useMatch[1].toLowerCase());
      }
    }
    for (const d of defs) providers.set(d, file);
    return { file, src, defs, uses };
  });

  const depsByFile = new Map();
  for (const item of parsed) {
    const fileDeps = new Set();
    for (const u of item.uses) {
      const prov = providers.get(u);
      if (prov && prov !== item.file) fileDeps.add(prov);
    }
    depsByFile.set(item.file, fileDeps);
  }

  const remaining = new Map(parsed.map((p) => [p.file, p]));
  const waves = [];
  while (remaining.size > 0) {
    const ready = [];
    for (const [file, item] of remaining.entries()) {
      const deps = depsByFile.get(file);
      let ok = true;
      for (const d of deps) {
        if (remaining.has(d)) {
          ok = false;
          break;
        }
      }
      if (ok) ready.push(item);
    }
    if (ready.length === 0) {
      throw new Error("Cyclic Fortran module dependency detected in src/");
    }
    for (const r of ready) remaining.delete(r.file);
    waves.push(ready);
  }
  return waves;
}

async function runPool(items, concurrency, worker) {
  let index = 0;
  const runners = Array.from({ length: Math.min(concurrency, items.length) }, async () => {
    while (index < items.length) {
      const current = index++;
      await worker(items[current], current);
    }
  });
  await Promise.all(runners);
}

async function compileFortranLibrary(flangDir, buildDir) {
  const flangBin = path.join(flangDir, "bin", "flang");
  const arBin = fs.existsSync(path.join(flangDir, "bin", "llvm-ar"))
    ? path.join(flangDir, "bin", "llvm-ar")
    : "ar";
  const modsDir = path.join(buildDir, "mods");
  const objsDir = path.join(buildDir, "objs");
  await fsp.mkdir(modsDir, { recursive: true });
  await fsp.mkdir(objsDir, { recursive: true });

  const cachePath = path.join(buildDir, "fortran-cache.json");
  let cache = {};
  if (fs.existsSync(cachePath)) {
    try {
      cache = JSON.parse(await fsp.readFile(cachePath, "utf8"));
    } catch {
      cache = {};
    }
  }

  const waves = discoverFortranWaves(path.join(ROOT, "src"));
  const flags = ["--target=wasm32-unknown-wasi", "-O3", "-fPIC", `-J${modsDir}`, `-I${modsDir}`];
  const intrinsicDigest = sha256Hex(
    Buffer.concat(
      ["__fortran_builtins.mod", "__fortran_type_info.mod", "iso_c_binding.mod"].map((m) =>
        fs.readFileSync(path.join(flangDir, "include", "flang", m)),
      ),
    ),
  );
  const concurrency = Math.max(1, Math.min(4, os.availableParallelism()));
  const objPaths = [];
  let compiledCount = 0;
  let cachedCount = 0;

  for (const wave of waves) {
    let waveChanged = false;
    await runPool(wave, concurrency, async (item) => {
      const rel = path.relative(ROOT, item.file).replace(/[\\/]/g, "__");
      const objPath = path.join(objsDir, `${rel}.o`);
      objPaths.push(objPath);
      const depDigests = [...item.uses].sort().join(",");
      const key = sha256Hex(
        Buffer.from(
          JSON.stringify({
            flags,
            intrinsicDigest,
            src: sha256Hex(Buffer.from(item.src)),
            depDigests,
          }),
        ),
      );
      if (!waveChanged && cache[rel] === key && fs.existsSync(objPath)) {
        cachedCount += 1;
        return;
      }
      await execFileAsync(flangBin, [...flags, "-c", item.file, "-o", objPath], {
        cwd: buildDir,
        maxBuffer: 16 * 1024 * 1024,
      });
      cache[rel] = key;
      compiledCount += 1;
      waveChanged = true;
    });
  }

  await fsp.writeFile(cachePath, JSON.stringify(cache, null, 2));
  const archivePath = path.join(buildDir, "libpepper_fortran.a");
  if (compiledCount > 0 || !fs.existsSync(archivePath)) {
    await fsp.rm(archivePath, { force: true });
    await execFileAsync(arBin, ["rcs", archivePath, ...objPaths.sort()]);
  }
  console.log(
    `[wasm] Fortran library ready (${compiledCount} compiled, ${cachedCount} cached -> ${path.relative(ROOT, archivePath)})`,
  );
  return archivePath;
}

async function generateSdsAndBridgeSources(manifest, buildDir) {
  const incDir = path.join(buildDir, "include");
  const genDir = path.join(buildDir, "gen");
  await fsp.mkdir(incDir, { recursive: true });
  await fsp.mkdir(genDir, { recursive: true });

  const rtHeaders = await getFlatbuffersCppRuntimeHeaders();
  const invokeHeaders = await getInvokeCppSchemaHeaders();
  for (const [rel, content] of Object.entries({ ...rtHeaders, ...invokeHeaders })) {
    const dst = path.join(incDir, rel);
    await fsp.mkdir(path.dirname(dst), { recursive: true });
    await fsp.writeFile(dst, content);
  }

  const sdsSchemaDir = path.join(SDS_ROOT, "schema");
  const schemaFiles = {};
  for (const d of await fsp.readdir(sdsSchemaDir)) {
    const f = path.join(sdsSchemaDir, d, "main.fbs");
    if (fs.existsSync(f)) {
      schemaFiles[`/sds/${d}/main.fbs`] = await fsp.readFile(f, "utf8");
    }
  }

  const flatc = await FlatcRunner.init();
  const roots = ["MBL", "NCD", "OEM", "AEM", "FRM", "TIM", "EVL", "OMM"];
  const visited = new Set();
  const emitSchema = async (name) => {
    if (visited.has(name)) return;
    visited.add(name);
    const src = schemaFiles[`/sds/${name}/main.fbs`];
    if (!src) throw new Error(`Missing SDS schema ${name}`);
    for (const m of src.matchAll(/include\s+"\.\.\/([A-Za-z0-9_]+)\/main\.fbs"\s*;/g)) {
      await emitSchema(m[1]);
    }
    const gen = flatc.generateCode({ entry: `/sds/${name}/main.fbs`, files: schemaFiles }, "cpp", {
      keepPrefix: true,
    });
    const guarded = gen["main_generated.h"].replaceAll(
      "FLATBUFFERS_GENERATED_MAIN_H_",
      `FLATBUFFERS_GENERATED_SDS_${name}_MAIN_H_`,
    );
    const dst = path.join(incDir, "sds", name, "main_generated.h");
    await fsp.mkdir(path.dirname(dst), { recursive: true });
    await fsp.writeFile(dst, guarded);
  };
  for (const r of roots) await emitSchema(r);

  const manifestEmbeddedPath = path.join(genDir, "plugin_manifest_embedded.c");
  const invokeHeaderPath = path.join(genDir, "space_data_module_invoke.h");
  const invokeHeaderAliasPath = path.join(genDir, "plugin_invoke_support.h");
  const invokeSourcePath = path.join(genDir, "plugin_invoke_support.cpp");

  const headerText = generateInvokeSupportHeader();
  await fsp.writeFile(manifestEmbeddedPath, generateEmbeddedManifestSource({ manifest }));
  await fsp.writeFile(invokeHeaderPath, headerText);
  await fsp.writeFile(invokeHeaderAliasPath, headerText);
  await fsp.writeFile(
    invokeSourcePath,
    'extern "C" __attribute__((weak)) void emscripten_notify_memory_growth(int) {}\n' +
      generateInvokeSupportSource({ manifest, includeCommandMain: true }),
  );

  // Generate pepper_wasm.h: matches LLVM Flang 21 wasm32 bind(c) trailing i32
  // character-length arguments (with C++ default = 1 so call sites stay clean).
  const rawPepperH = await fsp.readFile(path.join(ROOT, "include", "pepper.h"), "utf8");
  const wasmPepperH = rawPepperH.replace(
    /((?:int|void|double)\s+(pepper_[A-Za-z0-9_]+)\s*\()([\s\S]*?)(\)\s*;)/g,
    (full, prefix, fnName, params, suffix) => {
      const parts = params
        .split(",")
        .map((p) => p.trim())
        .filter(Boolean);
      if (parts.length === 0 || (parts.length === 1 && parts[0] === "void")) {
        return full;
      }
      let charPtrCount = 0;
      if (fnName === "pepper_spice_write_kernel") {
        // ck_file and text_file are type(c_ptr), value in pepper_capi_core.f90
        charPtrCount = 2;
      } else {
        for (const p of parts) {
          if (/\bchar\s*\*\s*[A-Za-z0-9_]+\s*$/.test(p)) {
            charPtrCount += 1;
          }
        }
      }
      if (charPtrCount === 0) return full;
      const extras = Array.from(
        { length: charPtrCount },
        (_, i) => `int32_t _flang_len_${i} PEPPER_FLANG_DEFAULT_LENGTH`,
      ).join(", ");
      return `${prefix}${params}, ${extras}${suffix}`;
    },
  );
  const publicWasmHeader = "#ifdef __cplusplus\n#define PEPPER_FLANG_DEFAULT_LENGTH = 1\n#else\n#define PEPPER_FLANG_DEFAULT_LENGTH\n#endif\n" + wasmPepperH + "\n#undef PEPPER_FLANG_DEFAULT_LENGTH\n";
  await fsp.writeFile(path.join(genDir, "pepper_wasm.h"), publicWasmHeader);
  const publicIncludeDir = path.join(ROOT, "dist", "include");
  await fsp.mkdir(publicIncludeDir, { recursive: true });
  await fsp.writeFile(path.join(publicIncludeDir, "pepper_wasm.h"), publicWasmHeader);

  return { incDir, genDir, manifestEmbeddedPath, invokeSourcePath };
}

/**
 * Pure-JS bit-exact NAIF DAF Type-2 Chebyshev SPK subsetter.
 * Slices a Type-2 planetary ephemeris (such as de440s.bsp) to [etStart, etStop]
 * while preserving the exact DAF file record, FTP validation string, comment area,
 * segment ordering, and IEEE-754 Chebyshev polynomial coefficients.
 */
function subsetType2SpkDaf(srcBuffer, etStart, etStop) {
  const REC_BYTES = 1024;
  const view = new DataView(srcBuffer.buffer, srcBuffer.byteOffset, srcBuffer.byteLength);
  const magic = Buffer.from(srcBuffer.subarray(0, 7)).toString("ascii");
  if (magic !== "DAF/SPK") {
    throw new Error(`Expected DAF/SPK container, got ${JSON.stringify(magic)}`);
  }
  const nd = view.getInt32(8, true);
  const ni = view.getInt32(12, true);
  if (nd !== 2 || ni !== 6) {
    throw new Error(`Expected SPK ND=2, NI=6, got ND=${nd}, NI=${ni}`);
  }
  const fward = view.getInt32(76, true);
  const sumRecOffset = (fward - 1) * REC_BYTES;
  const nextSum = view.getFloat64(sumRecOffset + 0, true);
  const nsum = Math.round(view.getFloat64(sumRecOffset + 16, true));
  if (nextSum !== 0 || nsum <= 0 || nsum > 25) {
    throw new Error(`Unsupported multi-record summary layout (next=${nextSum}, nsum=${nsum})`);
  }
  const nameRecOffset = fward * REC_BYTES;

  // Data words begin at the record immediately after the name record (fward + 2)
  const firstDataRecord = fward + 2; // 1-based record index
  let nextFreeWord = (firstDataRecord - 1) * 128 + 1; // 1-based float64 word address

  const outSegs = [];
  for (let s = 0; s < nsum; s++) {
    const sOff = sumRecOffset + 24 + s * 40;
    const segStart = view.getFloat64(sOff + 0, true);
    const segStop = view.getFloat64(sOff + 8, true);
    const targetId = view.getInt32(sOff + 16, true);
    const centerId = view.getInt32(sOff + 20, true);
    const frameId = view.getInt32(sOff + 24, true);
    const segType = view.getInt32(sOff + 28, true);
    const initAddr = view.getInt32(sOff + 32, true);
    const finalAddr = view.getInt32(sOff + 36, true);
    const segName = Buffer.from(srcBuffer.subarray(nameRecOffset + s * 40, nameRecOffset + (s + 1) * 40));

    if (segStop <= etStart || segStart >= etStop) continue;
    if (segType !== 2) {
      // Copy non-Type-2 segment verbatim
      const byteStart = (initAddr - 1) * 8;
      const byteEnd = finalAddr * 8;
      const rawWords = srcBuffer.subarray(byteStart, byteEnd);
      const wordCount = rawWords.byteLength / 8;
      const newInitAddr = nextFreeWord;
      const newFinalAddr = nextFreeWord + wordCount - 1;
      nextFreeWord = newFinalAddr + 1;
      outSegs.push({
        dc0: segStart,
        dc1: segStop,
        targetId,
        centerId,
        frameId,
        segType,
        initAddr: newInitAddr,
        finalAddr: newFinalAddr,
        segName,
        dataBytes: Buffer.from(rawWords),
      });
      continue;
    }

    // SPK Type 2 directory: [INIT, INTLEN, RSIZE, N] at (finalAddr - 3 .. finalAddr)
    const dirByteOff = (finalAddr - 4) * 8;
    const initEpoch = view.getFloat64(dirByteOff + 0, true);
    const intlen = view.getFloat64(dirByteOff + 8, true);
    const rsize = Math.round(view.getFloat64(dirByteOff + 16, true));
    const nRecs = Math.round(view.getFloat64(dirByteOff + 24, true));

    const rec0 = Math.max(0, Math.floor((etStart - initEpoch) / intlen));
    const rec1 = Math.min(nRecs - 1, Math.floor((etStop - initEpoch) / intlen));
    if (rec1 < rec0) continue;

    const newN = rec1 - rec0 + 1;
    const newInitEpoch = initEpoch + rec0 * intlen;
    const newDc0 = Math.max(segStart, newInitEpoch);
    const newDc1 = Math.min(segStop, newInitEpoch + newN * intlen);

    const sliceByteStart = ((initAddr - 1) + rec0 * rsize) * 8;
    const sliceByteLen = newN * rsize * 8;
    const dataBuf = Buffer.alloc(sliceByteLen + 32);
    srcBuffer.copy(dataBuf, 0, sliceByteStart, sliceByteStart + sliceByteLen);
    const dirView = new DataView(dataBuf.buffer, dataBuf.byteOffset + sliceByteLen, 32);
    dirView.setFloat64(0, newInitEpoch, true);
    dirView.setFloat64(8, intlen, true);
    dirView.setFloat64(16, rsize, true);
    dirView.setFloat64(24, newN, true);

    const wordCount = dataBuf.byteLength / 8;
    const newInitAddr = nextFreeWord;
    const newFinalAddr = nextFreeWord + wordCount - 1;
    nextFreeWord = newFinalAddr + 1;

    outSegs.push({
      dc0: newDc0,
      dc1: newDc1,
      targetId,
      centerId,
      frameId,
      segType,
      initAddr: newInitAddr,
      finalAddr: newFinalAddr,
      segName,
      dataBytes: dataBuf,
    });
  }

  const totalBytesUnpadded = (nextFreeWord - 1) * 8;
  const totalRecords = Math.ceil(totalBytesUnpadded / REC_BYTES);
  const outBuf = Buffer.alloc(totalRecords * REC_BYTES, 0);

  // Copy file record + reserved comment records (records 1 .. fward-1) verbatim
  srcBuffer.copy(outBuf, 0, 0, (fward - 1) * REC_BYTES);
  const outView = new DataView(outBuf.buffer, outBuf.byteOffset, outBuf.byteLength);
  outView.setInt32(76, fward, true);
  outView.setInt32(80, fward, true);
  outView.setInt32(84, nextFreeWord, true);

  // Write summary record (fward) and name record (fward + 1)
  outBuf.fill(0x20, nameRecOffset, nameRecOffset + REC_BYTES);
  outView.setFloat64(sumRecOffset + 0, 0.0, true);
  outView.setFloat64(sumRecOffset + 8, 0.0, true);
  outView.setFloat64(sumRecOffset + 16, outSegs.length, true);

  for (let i = 0; i < outSegs.length; i++) {
    const seg = outSegs[i];
    const sOff = sumRecOffset + 24 + i * 40;
    outView.setFloat64(sOff + 0, seg.dc0, true);
    outView.setFloat64(sOff + 8, seg.dc1, true);
    outView.setInt32(sOff + 16, seg.targetId, true);
    outView.setInt32(sOff + 20, seg.centerId, true);
    outView.setInt32(sOff + 24, seg.frameId, true);
    outView.setInt32(sOff + 28, seg.segType, true);
    outView.setInt32(sOff + 32, seg.initAddr, true);
    outView.setInt32(sOff + 36, seg.finalAddr, true);
    seg.segName.copy(outBuf, nameRecOffset + i * 40, 0, 40);
    seg.dataBytes.copy(outBuf, (seg.initAddr - 1) * 8);
  }

  return outBuf;
}

async function resolveSourceKernel(filename, naifSubpath) {
  const candidates = [
    process.env.PEPPER_KERNELS_DIR && path.join(process.env.PEPPER_KERNELS_DIR, filename),
    path.join(ROOT, "build", "compare", "kernels", filename),
    path.resolve(ROOT, "..", "tools", "xval", "naif-kernels", filename),
    path.join(ROOT, ".cache", "kernels", filename),
  ].filter(Boolean);

  for (const c of candidates) {
    if (fs.existsSync(c)) {
      await verifyDownload(filename, c);
      return c;
    }
  }

  const cacheDest = path.join(ROOT, ".cache", "kernels", filename);
  await fsp.mkdir(path.dirname(cacheDest), { recursive: true });
  const url = `https://naif.jpl.nasa.gov/pub/naif/generic_kernels/${naifSubpath}/${filename}`;
  console.log(`[wasm] Downloading NAIF kernel ${filename}...`);
  await downloadFile(url, cacheDest);
  await verifyDownload(filename, cacheDest);
  return cacheDest;
}

async function buildBrowserAssets() {
  const webWasmDir = path.join(ROOT, "web", "wasm");
  const kernelsOutDir = path.join(webWasmDir, "kernels");
  await fsp.mkdir(kernelsOutDir, { recursive: true });

  // 1. Bundle browser SDK + FlatBuffers + SDS JS codecs into a single zero-CDN ESM bundle
  const entryPath = path.join(ROOT, "build", "wasm", "gen", "pepper-sdk-client-entry.mjs");
  await fsp.writeFile(
    entryPath,
    `export * as flatbuffers from "flatbuffers";
export { loadModule } from "space-data-module-sdk/host/isomorphic";
export { decodePluginManifest } from "space-data-module-sdk/manifest";
export { encodeModuleBundle, decodeModuleBundle, ModuleBundleEntryRole, ModulePayloadEncoding } from "space-data-module-sdk/bundle";
export * as MBL_NS from "spacedatastandards.org/lib/js/MBL/main.js";
export * as NCD_NS from "spacedatastandards.org/lib/js/NCD/main.js";
export * as OEM_NS from "spacedatastandards.org/lib/js/OEM/main.js";
export * as AEM_NS from "spacedatastandards.org/lib/js/AEM/main.js";
export * as FRM_NS from "spacedatastandards.org/lib/js/FRM/main.js";
export * as RFM_NS from "spacedatastandards.org/lib/js/RFM/main.js";
export * as TIM_NS from "spacedatastandards.org/lib/js/TIM/main.js";
export * as EVL_NS from "spacedatastandards.org/lib/js/EVL/main.js";
export * as OMM_NS from "spacedatastandards.org/lib/js/OMM/main.js";
export * as PCE_NS from "spacedatastandards.org/lib/js/PCE/main.js";
export * as CAT_NS from "spacedatastandards.org/lib/js/CAT/main.js";
`,
  );

  const clientOutPath = path.join(webWasmDir, "pepper-sdk-client.js");
  await esbuild.build({
    entryPoints: [entryPath],
    bundle: true,
    format: "esm",
    platform: "browser",
    target: ["es2022"],
    outfile: clientOutPath,
    logLevel: "warning",
  });
  console.log(`[wasm] Bundled browser SDK client -> ${path.relative(ROOT, clientOutPath)}`);

  // 2. Copy LSK + PCK and slice genuine JPL DE440 SPK (1999-12-31 .. 2036-01-02)
  const lskSrc = await resolveSourceKernel("naif0012.tls", "lsk");
  const pckSrc = await resolveSourceKernel("pck00011.tpc", "pck");
  const spkSrc = await resolveSourceKernel("de440s.bsp", "spk/planets");

  await fsp.copyFile(lskSrc, path.join(kernelsOutDir, "naif0012.tls"));
  await fsp.copyFile(pckSrc, path.join(kernelsOutDir, "pck00011.tpc"));

  const subsetDest = path.join(kernelsOutDir, "de440_2000_2035.bsp");
  if (!fs.existsSync(subsetDest)) {
    const fullSpk = await fsp.readFile(spkSrc);
    // 1999-12-25T00:00:00 TDB .. 2036-01-05T00:00:00 TDB
    const etStart = -7 * 86400;
    const etStop = 36 * 365.25 * 86400;
    const sliced = subsetType2SpkDaf(fullSpk, etStart, etStop);
    await fsp.writeFile(subsetDest, sliced);
    console.log(
      `[wasm] Subsetted JPL DE440 SPK (${(fullSpk.byteLength / 1e6).toFixed(1)} MB -> ${(sliced.byteLength / 1e6).toFixed(2)} MB) -> ${path.relative(ROOT, subsetDest)}`,
    );
  }
}

async function main() {
  const startedAt = performance.now();
  const manifestPath = path.join(ROOT, "plugin-manifest.json");
  const manifest = JSON.parse(await fsp.readFile(manifestPath, "utf8"));

  const buildDir = path.join(ROOT, "build", "wasm");
  const distIsoDir = path.join(ROOT, "dist", "isomorphic");
  const distGuestLinkDir = path.join(ROOT, "dist", "guest-link");
  await fsp.mkdir(buildDir, { recursive: true });
  await fsp.mkdir(distIsoDir, { recursive: true });

  const flangDir = await ensureFlangWasmToolchain();
  const tc = resolveWasiThreadsToolchain();
  const flangRtArchive = await ensureWasiFlangRuntime(flangDir, tc);

  const fortranArchive = await compileFortranLibrary(flangDir, buildDir);
  const { incDir, genDir, manifestEmbeddedPath, invokeSourcePath } =
    await generateSdsAndBridgeSources(manifest, buildDir);

  const cObjsDir = path.join(buildDir, "cobjs");
  await fsp.mkdir(cObjsDir, { recursive: true });

  const commonCFlags = [
    ...tc.toolchainArgs,
    "-O3",
    "-mbulk-memory",
    `-I${path.join(ROOT, "include")}`,
    `-I${path.join(SDK_ROOT, "include")}`,
    `-I${incDir}`,
    `-I${genDir}`,
  ];

  const moduleObj = path.join(cObjsDir, "pepper_module.o");
  const invokeObj = path.join(cObjsDir, "plugin_invoke_support.o");
  const manifestObj = path.join(cObjsDir, "plugin_manifest_embedded.o");
  const shimObj = path.join(cObjsDir, "wasm_flang_shim.o");

  await Promise.all([
    execFileAsync(tc.clangxx, [
      ...commonCFlags,
      "-std=c++17",
      "-fno-exceptions",
      "-c",
      path.join(ROOT, "wasm", "pepper_module.cpp"),
      "-o",
      moduleObj,
    ]),
    execFileAsync(tc.clangxx, [
      ...commonCFlags,
      "-std=c++17",
      "-fno-exceptions",
      "-c",
      invokeSourcePath,
      "-o",
      invokeObj,
    ]),
    execFileAsync(tc.clang, [...commonCFlags, "-c", manifestEmbeddedPath, "-o", manifestObj]),
    execFileAsync(tc.clang, [
      ...commonCFlags,
      "-c",
      path.join(ROOT, "wasm", "wasm_flang_shim.c"),
      "-o",
      shimObj,
    ]),
  ]);

  const methodExports = manifest.methods.map((m) => m.methodId);
  const propagatorAbiExports = [
    "plugin_init",
    "plugin_init_omm",
    "plugin_ingest_omm_one",
    "plugin_init_elements",
    "plugin_init_ephemeris",
    "plugin_propagate",
    "plugin_propagate_batch",
    "plugin_entity_count",
    "plugin_destroy",
  ];
  const exportedSymbols = [
    "_start",
    "plugin_alloc",
    "plugin_free",
    "plugin_invoke_stream",
    "plugin_get_manifest_flatbuffer",
    "plugin_get_manifest_flatbuffer_size",
    ...methodExports,
    ...propagatorAbiExports,
  ];

  const outputPath = path.join(distIsoDir, "module.wasm");
  const rawLinkedPath = path.join(buildDir, "module.raw.wasm");

  await execFileAsync(tc.clangxx, [
    ...tc.toolchainArgs,
    "-mbulk-memory",
    "-fno-exceptions",
    "-Wl,-O2",
    "-Wl,--gc-sections",
    "-Wl,--strip-all",
    "-Wl,--compress-relocations",
    "-Wl,--initial-memory=67108864",
    "-Wl,--max-memory=2147483648",
    "-Wl,-z,stack-size=4194304",
    ...exportedSymbols.map((sym) => `-Wl,--export=${sym}`),
    moduleObj,
    invokeObj,
    manifestObj,
    shimObj,
    fortranArchive,
    flangRtArchive,
    "-o",
    rawLinkedPath,
  ]);

  if (process.env.PEPPER_WASM_OPT === "1") {
    const optimizedPath = path.join(buildDir, "module.optimized.wasm");
    await execFileAsync("wasm-opt", [
      "-Oz", "--enable-threads", "--enable-bulk-memory",
      "--enable-mutable-globals", "--enable-sign-ext",
      "--enable-nontrapping-float-to-int",
      rawLinkedPath, "-o", optimizedPath,
    ]);
    await fsp.rename(optimizedPath, rawLinkedPath);
  }

  const rawWasmBytes = new Uint8Array(await fsp.readFile(rawLinkedPath));
  const wasmBytes = appendWasmCustomSection(
    rawWasmBytes,
    SDS_MANIFEST_SECTION_NAME,
    encodePluginManifest(manifest),
  );
  await fsp.writeFile(outputPath, wasmBytes);
  await fsp.copyFile(manifestPath, path.join(ROOT, "dist", "plugin-manifest.json"));

  // Emit guest-link artifact directory (including Fortran & Flang runtime archives for `space-data-module flow compile`)
  const guestLink = deriveGuestLinkObject({
    objectBytes: new Uint8Array(await fsp.readFile(moduleObj)),
    pluginId: manifest.pluginId,
    methodIds: methodExports,
  });
  const shimBytes = new Uint8Array(await fsp.readFile(shimObj));
  const fortranArchiveBytes = new Uint8Array(await fsp.readFile(fortranArchive));
  const flangRtBytes = new Uint8Array(await fsp.readFile(flangRtArchive));
  const guestLibraries = [
    {
      name: "pepper_flang_shim",
      version: "0.1.0",
      language: "c",
      namespace: "sdm_lib_pepper_flang_shim_",
      sha256: sha256Hex(shimBytes),
      objectBytes: shimBytes,
    },
    {
      name: "pepper_fortran",
      version: "0.1.0",
      language: "fortran",
      namespace: "sdm_lib_pepper_fortran_",
      sha256: sha256Hex(fortranArchiveBytes),
      objectBytes: fortranArchiveBytes,
    },
    {
      name: "flang_rt_wasi",
      version: "21.1.8",
      language: "c++",
      namespace: "sdm_lib_flang_rt_wasi_",
      sha256: sha256Hex(flangRtBytes),
      objectBytes: flangRtBytes,
    },
  ];
  await writeGuestLinkFiles(
    {
      guestLink: {
        format: "wasm-object",
        language: "cpp",
        symbolPrefix: guestLink.symbolPrefix,
        methodSymbols: guestLink.methodSymbols,
        threadModel: ModuleThreadModel.WASI_SEQUENTIAL,
        objectBytes: guestLink.objectBytes,
        libraries: guestLibraries,
      },
    },
    distGuestLinkDir,
  );

  // Emit two-body propagator reference corpus (vectors/vectors.json) for CLI `space-data-module conformance propagator`
  const { buildSelfTestCorpus } = await import(
    pathToFileURL(path.join(SDK_ROOT, "src", "conformance", "selfTestCorpus.js")).href
  );
  const vectorsDir = path.join(ROOT, "vectors");
  await fsp.mkdir(vectorsDir, { recursive: true });
  await fsp.writeFile(
    path.join(vectorsDir, "vectors.json"),
    JSON.stringify(buildSelfTestCorpus(), null, 2) + "\n",
  );

  // Gate 1: Sequential & SIMD WASI artifact guards
  assertSequentialArtifact(wasmBytes, {
    source: outputPath,
    target: tc.target,
  });
  const simdReport = assertSimdArtifact(wasmBytes, {
    optedIn: false,
    source: outputPath,
  });
  if (simdReport.warning) {
    console.warn(`[wasm] SIMD notice: ${simdReport.warning}`);
  }

  // Gate 2: Browser WASI import compatibility gate (zero unimplemented imports)
  const compiledMod = new WebAssembly.Module(wasmBytes);
  const dummyShim = createBrowserWasiShim();
  const shimNamespaces = dummyShim.imports;
  for (const imp of WebAssembly.Module.imports(compiledMod)) {
    const ns = shimNamespaces[imp.module];
    if (!ns || typeof ns[imp.name] === "undefined") {
      throw new Error(
        `Unsupported WASM import for browser WASI shim: ${imp.module}.${imp.name} (${imp.kind})`,
      );
    }
  }

  // Gate 3: SDK manifest + SpaceDataStandards.org catalog compliance
  const compliance = await validateArtifactWithStandards({
    manifest,
    wasmPath: outputPath,
  });
  if (!compliance.ok) {
    throw new Error(
      `SDK compliance failed:\n${JSON.stringify(compliance.errors, null, 2)}`,
    );
  }

  // Gate 4: Official SDK Propagator Conformance Suite (including 200-cycle zero-leak gate)
  const conformance = await runConformance({
    family: "propagator",
    artifactPath: outputPath,
  });
  const failedChecks = conformance.checks.filter((c) => c.status === "fail");
  if (failedChecks.length > 0) {
    throw new Error(
      `Propagator conformance failed (${conformance.verdict}):\n${JSON.stringify(failedChecks, null, 2)}`,
    );
  }
  console.log(
    `[wasm] SDK compliance OK & propagator conformance ${conformance.verdict} (${conformance.checks.filter((c) => c.status === "pass").length}/${conformance.checks.length} checks passed, 0 failures)`,
  );

  // Build browser JS bundle and subsetted kernels
  await buildBrowserAssets();

  const elapsedSec = ((performance.now() - startedAt) / 1000).toFixed(2);
  console.log(
    `[wasm] Built ${path.relative(ROOT, outputPath)} (${(wasmBytes.byteLength / (1024 * 1024)).toFixed(2)} MiB) in ${elapsedSec}s`,
  );
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
