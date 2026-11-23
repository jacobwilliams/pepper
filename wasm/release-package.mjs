import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const [mode, version] = process.argv.slice(2);
assert.match(version ?? "", /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?$/, "Use a release version such as 0.1.0 or 0.1.0-rc.1");
const manifest = JSON.parse(await fs.readFile(path.join(root, "plugin-manifest.json")));
const pkg = JSON.parse(await fs.readFile(path.join(root, "package.json")));
assert.equal(manifest.version, version, "Requested version differs from plugin-manifest.json");
assert.equal(pkg.version, version, "Requested version differs from package.json");
if (mode === "version") {
  console.log(`Validated version ${version}`);
} else if (mode === "pack") {
  const wasmPath = path.join(root, "dist", "isomorphic", "module.wasm");
  const wasm = await fs.readFile(wasmPath);
  const { decodePluginManifest } = await import("space-data-module-sdk/manifest");
  const { SDS_MANIFEST_SECTION_NAME } = await import("space-data-module-sdk/bundle");
  const sections = WebAssembly.Module.customSections(new WebAssembly.Module(wasm), SDS_MANIFEST_SECTION_NAME);
  assert.equal(sections.length, 1, "One embedded plugin manifest is required");
  assert.equal(decodePluginManifest(new Uint8Array(sections[0])).version, version, "Built module version differs from requested version");
  const builtManifest = JSON.parse(await fs.readFile(path.join(root, "dist", "plugin-manifest.json")));
  assert.deepEqual(builtManifest, manifest, "Built manifest differs from package manifest");
  const dest = path.join(root, "dist", "release");
  await fs.rm(dest, { recursive: true, force: true });
  await fs.mkdir(dest, { recursive: true });
  const [packed] = JSON.parse(execFileSync("npm", ["pack", "--json", "--pack-destination", dest], { cwd: root, encoding: "utf8" }));
  const required = ["package.json", "LICENSE", "README.md", "plugin-manifest.json", "dist/isomorphic/module.wasm", "include/pepper.h", "dist/include/pepper_wasm.h", "dist/guest-link/module-link.o", "dist/guest-link/metadata.json"];
  const paths = packed.files.map((entry) => entry.path);
  for (const file of required) assert.ok(paths.includes(file), `Missing package file ${file}`);
  for (const file of paths) assert.ok(required.includes(file) || file.startsWith("dist/guest-link/"), `Unexpected package file ${file}`);
  assert.ok(paths.some((file) => file.includes("pepper_fortran")), "Missing Fortran archive");
  assert.ok(paths.some((file) => file.includes("flang_rt_wasi")), "Missing Flang runtime archive");
  assert.ok(paths.some((file) => file.includes("pepper_flang_shim")), "Missing filesystem shim");
  await fs.copyFile(wasmPath, path.join(dest, "module.wasm"));
  await fs.copyFile(path.join(root, "plugin-manifest.json"), path.join(dest, "plugin-manifest.json"));
  const sums = [];
  for (const name of [packed.filename, "module.wasm", "plugin-manifest.json"]) {
    const digest = createHash("sha256").update(await fs.readFile(path.join(dest, name))).digest("hex");
    sums.push(`${digest}  ${name}`);
  }
  await fs.writeFile(path.join(dest, "SHA256SUMS"), sums.join("\n") + "\n");
  console.log(JSON.stringify(packed, null, 2));
} else {
  throw new Error("Usage: node wasm/release-package.mjs <version|pack> <version>");
}
