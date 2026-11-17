import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
export const downloads = {
  chromium: {
    version: "159.0.8154.6",
    url: "https://cdn.playwright.dev/builds/cft/159.0.8154.6/linux64/chrome-linux64.zip",
    sha256: "370601106c1b5619aae5f06f7a3fdc60836486135934779461e2a208d777f596",
  },
  flangLinux: {
    version: "21.1.8",
    url: "https://github.com/miinso/flang-releases/releases/download/v21.1.8/flang+llvm-21.1.8-x86_64-unknown-linux-gnu.tar.gz",
    sha256: "673f4dc1e95d09b1253ca32078ae058540d68b533bdcbcbf72b7242fbed82dc6",
  },
  llvm: {
    version: "21.1.8",
    url: "https://github.com/llvm/llvm-project/releases/download/llvmorg-21.1.8/llvm-project-21.1.8.src.tar.xz",
    sha256: "4633a23617fa31a3ea51242586ea7fb1da7140e426bd62fc164261fe036aa142",
  },
  wasi: {
    version: "29.0",
    url: "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-29/wasi-sdk-29.0-x86_64-linux.tar.gz",
    sha256: "87d1d1a2879d139cdc624b968efad3d4a97b8078cdff95e63ac88ecafd1a0171",
  },
  binaryen: {
    version: "125",
    url: "https://github.com/WebAssembly/binaryen/releases/download/version_125/binaryen-version_125-x86_64-linux.tar.gz",
    sha256: "7c3bc16599c8274a04d34a504fe4be2047884f900e0e2da2f6fb9cd667183be4",
  },
  "naif0012.tls": {
    version: "0012",
    url: "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls",
    sha256: "678e32bdb5a744117a467cd9601cd6b373f0e9bc9bbde1371d5eee39600a039b",
  },
  "pck00011.tpc": {
    version: "00011",
    url: "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/pck00011.tpc",
    sha256: "3dff7b1dbeceaa01f25467767d3fa25816051c85d162d1edf04acb310ee28bb1",
  },
  "de440s.bsp": {
    version: "DE440s",
    url: "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de440s.bsp",
    sha256: "c1c7feeab882263fc493a9d5a5b2ddd71b54826cdf65d8d17a76126b260a49f2",
  },
};

export async function verifyDownload(name, file) {
  const expected = downloads[name].sha256;
  const actual = createHash("sha256").update(await fs.readFile(file)).digest("hex");
  if (actual !== expected) throw new Error(`${name} SHA-256 mismatch: expected ${expected}, got ${actual}`);
}

export async function fetchPinned(name) {
  const spec = downloads[name];
  const dest = path.join(root, ".cache", "downloads", path.basename(new URL(spec.url).pathname));
  await fs.mkdir(path.dirname(dest), { recursive: true });
  try { await fs.access(dest); } catch {
    const partial = `${dest}.partial`;
    execFileSync("curl", ["--fail", "--location", "--retry", "3", spec.url, "--output", partial], { stdio: "inherit" });
    await verifyDownload(name, partial);
    await fs.rename(partial, dest);
  }
  await verifyDownload(name, dest);
  return dest;
}

export async function unpackPinned(name, dest) {
  const archive = await fetchPinned(name);
  const marker = path.join(dest, ".pepper-download-sha256");
  if (await fs.readFile(marker, "utf8").catch(() => "") === downloads[name].sha256) return dest;
  await fs.rm(dest, { recursive: true, force: true });
  await fs.mkdir(dest, { recursive: true });
  execFileSync("tar", ["-xf", archive, "--strip-components=1", "-C", dest], { stdio: "inherit" });
  await fs.writeFile(marker, downloads[name].sha256);
  return dest;
}

export function ensureLlvmSource() {
  return unpackPinned("llvm", path.join(root, ".cache", "llvm-project"));
}
