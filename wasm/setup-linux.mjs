import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { downloads, fetchPinned, root, unpackPinned } from "./toolchain-downloads.mjs";

if (process.platform !== "linux" || process.arch !== "x64") {
  throw new Error("Release toolchain setup requires Linux x64.");
}
const cache = path.join(root, ".cache");
const wasi = await unpackPinned("wasi", path.join(cache, "wasi-sdk"));
const binaryen = await unpackPinned("binaryen", path.join(cache, "binaryen"));
const flang = path.join(cache, "flang-wasm");
const marker = path.join(flang, ".pepper-native-flang-sha256");
// The compiler is the prebuilt LLVM Flang release the macOS build already pins
// (the same release, its Linux x86-64 archive), unpacked as it is there.
if (await fs.readFile(marker, "utf8").catch(() => "") !== downloads.flangLinux.sha256) {
  const archive = await fetchPinned("flangLinux");
  await fs.rm(flang, { recursive: true, force: true });
  await fs.mkdir(flang, { recursive: true });
  execFileSync("tar", ["-xf", archive, "--strip-components=1", "-C", flang], { stdio: "inherit" });
  if (!await fs.stat(path.join(flang, "bin", "llvm-ar")).catch(() => null)) {
    await fs.copyFile(path.join(wasi, "bin", "llvm-ar"), path.join(flang, "bin", "llvm-ar"));
    await fs.chmod(path.join(flang, "bin", "llvm-ar"), 0o755);
  }
  execFileSync(path.join(flang, "bin", "flang"), ["--version"], { stdio: "inherit" });
  await fs.writeFile(marker, downloads.flangLinux.sha256);
}
const browserArchive = await fetchPinned("chromium");
const browserDir = path.join(cache, "chromium");
await fs.mkdir(browserDir, { recursive: true });
execFileSync("unzip", ["-q", "-o", browserArchive, "-d", browserDir], { stdio: "inherit" });
const env = {
  CHROME_BIN: path.join(browserDir, "chrome-linux64", "chrome"),
  FLANG_WASM_DIR: flang,
  SDN_WASI_CLANG: path.join(wasi, "bin", "clang"),
  SDN_WASI_CLANGXX: path.join(wasi, "bin", "clang++"),
  SDN_WASI_SYSROOT: path.join(wasi, "share", "wasi-sysroot"),
  SDN_WASI_RESOURCE_DIR: path.join(wasi, "lib", "clang", "21"),
};
if (!process.env.GITHUB_ENV || !process.env.GITHUB_PATH) throw new Error("GITHUB_ENV and GITHUB_PATH are required.");
await fs.appendFile(process.env.GITHUB_ENV, Object.entries(env).map(([k, v]) => `${k}=${v}\n`).join(""));
await fs.appendFile(process.env.GITHUB_PATH, `${path.join(binaryen, "bin")}\n`);
