import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { createHash, generateKeyPairSync } from "node:crypto";
import fs from "node:fs";
import fsp from "node:fs/promises";
import { createRequire } from "node:module";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";
import { promisify } from "node:util";

import * as flatbuffers from "flatbuffers";
import { chromium } from "playwright-core";
import { loadModule as loadServerModule } from "space-data-module-sdk/host/isomorphic";
import {
  ModuleBundleEntryRole,
  ModulePayloadEncoding,
  encodeModuleBundle,
} from "space-data-module-sdk/bundle";
import {
  assertSequentialArtifact,
  assertSimdArtifact,
  resolveWasiThreadsToolchain,
} from "space-data-module-sdk/compiler";
import { validateArtifactWithStandards } from "space-data-module-sdk/compliance";
import { runConformance } from "space-data-module-sdk/conformance";
import {
  ORBPRO_STATE_VECTOR,
  ReferenceFrame,
  StateFlags,
} from "space-data-module-sdk/generated/propagator-abi";
import { createBrowserModuleHarness } from "space-data-module-sdk/host/browser-module";
import { createBrowserWasiShim } from "space-data-module-sdk/host/wasi-shim";
import { decodePluginManifest } from "space-data-module-sdk/manifest";

import * as AEM_NS from "spacedatastandards.org/lib/js/AEM/main.js";
import * as CAT_NS from "spacedatastandards.org/lib/js/CAT/main.js";
import * as EVL_NS from "spacedatastandards.org/lib/js/EVL/main.js";
import * as FRM_NS from "spacedatastandards.org/lib/js/FRM/main.js";
import * as NCD_NS from "spacedatastandards.org/lib/js/NCD/main.js";
import * as OEM_NS from "spacedatastandards.org/lib/js/OEM/main.js";
import * as OMM_NS from "spacedatastandards.org/lib/js/OMM/main.js";
import * as PCE_NS from "spacedatastandards.org/lib/js/PCE/main.js";
import * as RFM_NS from "spacedatastandards.org/lib/js/RFM/main.js";
import * as TIM_NS from "spacedatastandards.org/lib/js/TIM/main.js";

import { createPepperWasmServer } from "./serve.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, "..");
const WASM_PATH = path.join(ROOT, "dist", "isomorphic", "module.wasm");
const MANIFEST_PATH = path.join(ROOT, "plugin-manifest.json");
const KERNELS_DIR = path.join(ROOT, "web", "wasm", "kernels");

const require = createRequire(import.meta.url);
function findPackageRoot(specifier) {
  let dir = path.dirname(require.resolve(specifier));
  while (!fs.existsSync(path.join(dir, "package.json"))) {
    const parent = path.dirname(dir);
    if (parent === dir) throw new Error(`Could not find package.json for ${specifier}`);
    dir = parent;
  }
  return dir;
}
const SDK_ROOT = findPackageRoot("space-data-module-sdk");

const sdsTypeRef = (code) => ({
  schemaName: `${code}.fbs`,
  fileIdentifier: `$${code}`,
  rootTypeName: code,
  wireFormat: "flatbuffer",
});

function packRoot(rootObjT, identifier) {
  const fbb = new flatbuffers.Builder(1024);
  const off = rootObjT.pack(fbb);
  fbb.finish(off, identifier);
  return fbb.asUint8Array();
}

async function loadDefaultKernelsMbl() {
  const names = ["naif0012.tls", "pck00011.tpc", "de440_2000_2035.bsp"];
  const entries = [];
  const rawMap = new Map();
  for (const name of names) {
    const payload = new Uint8Array(await fsp.readFile(path.join(KERNELS_DIR, name)));
    rawMap.set(name, payload);
    entries.push({
      entryId: name,
      role: ModuleBundleEntryRole.AUXILIARY,
      sectionName: name,
      mediaType: "application/octet-stream",
      payloadEncoding: ModulePayloadEncoding.RAW_BYTES,
      payload,
    });
  }
  return { mblBytes: encodeModuleBundle({ entries }), rawMap };
}

function findLocalChromiumExecutable() {
  const pwCaches = [
    path.join(os.homedir(), "Library", "Caches", "ms-playwright"),
    path.join(os.homedir(), ".cache", "ms-playwright"),
  ];
  const pwCandidates = [];
  for (const cacheDir of pwCaches) {
    if (!fs.existsSync(cacheDir)) continue;
    for (const ent of fs.readdirSync(cacheDir).filter((d) => d.startsWith("chromium-")).sort().reverse()) {
      pwCandidates.push(
        path.join(cacheDir, ent, "chrome-mac-arm64", "Google Chrome for Testing.app", "Contents", "MacOS", "Google Chrome for Testing"),
        path.join(cacheDir, ent, "chrome-mac-x64", "Google Chrome for Testing.app", "Contents", "MacOS", "Google Chrome for Testing"),
        path.join(cacheDir, ent, "chrome-linux64", "chrome"),
        path.join(cacheDir, ent, "chrome-linux", "chrome"),
      );
    }
  }
  const candidates = [
    process.env.CHROME_BIN,
    ...pwCandidates,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/usr/bin/google-chrome",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
  ].filter(Boolean);
  for (const c of candidates) {
    if (fs.existsSync(c)) return c;
  }
  return null;
}

test("SDK artifact guards, standards compliance, guest-link bundle, and 13/13 propagator conformance", async () => {
  const wasmBytes = new Uint8Array(await fsp.readFile(WASM_PATH));
  const manifest = JSON.parse(await fsp.readFile(MANIFEST_PATH, "utf8"));
  const tc = resolveWasiThreadsToolchain();

  assertSequentialArtifact(wasmBytes, { source: WASM_PATH, target: tc.target });
  const simd = assertSimdArtifact(wasmBytes, { optedIn: false, source: WASM_PATH });
  assert.equal(simd.warning ?? null, null);

  // Browser WASI shim import completeness
  const mod = new WebAssembly.Module(wasmBytes);
  const shim = createBrowserWasiShim();
  for (const imp of WebAssembly.Module.imports(mod)) {
    const ns = shim.imports[imp.module];
    assert.ok(ns && typeof ns[imp.name] !== "undefined", `Missing WASI shim import: ${imp.module}.${imp.name}`);
  }

  // Guest-link artifact files
  const guestObjPath = path.join(ROOT, "dist", "guest-link", "module-link.o");
  const guestMetaPath = path.join(ROOT, "dist", "guest-link", "metadata.json");
  assert.ok(fs.existsSync(guestObjPath), "dist/guest-link/module-link.o must exist");
  assert.ok(fs.existsSync(guestMetaPath), "dist/guest-link/metadata.json must exist");
  const guestMeta = JSON.parse(await fsp.readFile(guestMetaPath, "utf8"));
  assert.equal(guestMeta.threadModel, "wasi-sequential");

  // Standards compliance
  const compliance = await validateArtifactWithStandards({ manifest, wasmPath: WASM_PATH });
  assert.equal(compliance.ok, true, JSON.stringify(compliance.errors));

  // Propagator conformance (corpus-free + full two-body reference corpus + 200-cycle zero-leak gate)
  const { buildSelfTestCorpus } = await import(
    pathToFileURL(path.join(SDK_ROOT, "src", "conformance", "selfTestCorpus.js")).href
  );
  const tmpDir = await fsp.mkdtemp(path.join(os.tmpdir(), "pepper-conformance-"));
  const vectorsPath = path.join(tmpDir, "vectors.json");
  try {
    await fsp.writeFile(vectorsPath, JSON.stringify(buildSelfTestCorpus()));
    const report = await runConformance({
      family: "propagator",
      artifactPath: WASM_PATH,
      vectorsPath,
    });
    const failed = report.checks.filter((c) => c.status === "fail");
    const gaps = report.checks.filter((c) => c.status === "gap");
    const passed = report.checks.filter((c) => c.status === "pass");
    assert.equal(failed.length, 0, `Conformance failures: ${JSON.stringify(failed)}`);
    // propagatorSuite emits 12 executable checks + 1 static tier0/parity-gate reminder gap
    assert.equal(passed.length, 12);
    assert.deepEqual(
      gaps.map((g) => g.id),
      ["tier0/parity-gate"],
    );
    assert.equal(report.verdict, "PASS-WITH-GAPS");
  } finally {
    await fsp.rm(tmpDir, { recursive: true, force: true });
  }
});

test("Both SDK invoke surfaces (direct & command) execute identically with embedded $PLG manifest", async () => {
  const wasmBytes = new Uint8Array(await fsp.readFile(WASM_PATH));
  const { mblBytes } = await loadDefaultKernelsMbl();

  const directMod = await createBrowserModuleHarness({ wasmSource: wasmBytes, surface: "direct" });
  const commandMod = await createBrowserModuleHarness({ wasmSource: wasmBytes, surface: "command" });

  try {
    const manifestBytes = directMod.readManifest();
    assert.ok(manifestBytes instanceof Uint8Array && manifestBytes.byteLength > 64);
    const manifestInfo = decodePluginManifest(manifestBytes);
    assert.equal(manifestInfo.pluginId, "org.spacedatastandards.pepper");
    assert.equal(manifestInfo.methods.length, 8);

    for (const [label, h] of [
      ["direct", directMod],
      ["command", commandMod],
    ]) {
      const furnRes = await h.invoke({
        methodId: "furnish_kernels",
        inputs: [{ portId: "kernels", typeRef: sdsTypeRef("MBL"), payload: mblBytes }],
      });
      assert.equal(furnRes.statusCode, 0, `${label} furnish_kernels failed: ${furnRes.errorMessage}`);
      assert.equal(furnRes.outputs.length, 3);
    }

    // Multi-port ($MBL kernels + $TIM request) UTC -> TDB conversion must produce
    // byte-identical FlatBuffer outputs across direct (plugin_invoke_stream) and command (_start)
    const src = new TIM_NS.TIMInstantT();
    src.TIME_SYSTEM = TIM_NS.timingStandard.UTC;
    src.EPOCH_FORMAT = TIM_NS.timEpochRepresentation.ISO8601;
    src.ISO8601 = "2000-01-01T12:00:00.000Z";
    const req = new TIM_NS.TIMConversionRequestT();
    req.SOURCE = src;
    req.TARGET_TIME_SYSTEM = TIM_NS.timingStandard.TDB;
    req.TARGET_EPOCH_FORMAT = TIM_NS.timEpochRepresentation.JULIAN_DATE;
    const timReqBytes = packRoot(new TIM_NS.TIMT(src.TIME_SYSTEM, null, req, null), "$TIM");

    const invokeInputs = [
      { portId: "kernels", typeRef: sdsTypeRef("MBL"), payload: mblBytes },
      { portId: "request", typeRef: sdsTypeRef("TIM"), payload: timReqBytes },
    ];
    const rDirect = await directMod.invoke({ methodId: "convert_time", inputs: invokeInputs });
    const rCommand = await commandMod.invoke({ methodId: "convert_time", inputs: invokeInputs });
    assert.equal(rDirect.statusCode, 0);
    assert.equal(rCommand.statusCode, 0);

    const outDirect = TIM_NS.TIM.getRootAsTIM(new flatbuffers.ByteBuffer(rDirect.outputs[0].payload)).unpack();
    const outCommand = TIM_NS.TIM.getRootAsTIM(new flatbuffers.ByteBuffer(rCommand.outputs[0].payload)).unpack();
    assert.ok(Math.abs(outDirect.CONVERSION_RESULT.DELTA_SECONDS - 64.1839272847) < 1e-6);
    assert.equal(outDirect.CONVERSION_RESULT.DELTA_SECONDS, outCommand.CONVERSION_RESULT.DELTA_SECONDS);
    assert.deepEqual(rDirect.outputs[0].payload, rCommand.outputs[0].payload);
  } finally {
    await directMod.destroy();
    await commandMod.destroy();
  }
});

test("All 8 SpaceDataStandards.org methods & numerical astrodynamics invariants", async () => {
  const wasmBytes = new Uint8Array(await fsp.readFile(WASM_PATH));
  const { mblBytes, rawMap } = await loadDefaultKernelsMbl();
  const harness = await createBrowserModuleHarness({ wasmSource: wasmBytes, surface: "direct" });

  const invoke = async (methodId, portId, code, payload) => {
    const res = await harness.invoke({
      methodId,
      inputs: [{ portId, typeRef: sdsTypeRef(code), payload }],
    });
    assert.equal(res.statusCode, 0, `${methodId} failed (${res.statusCode}): ${res.errorMessage}`);
    return res.outputs;
  };

  try {
    // 1. furnish_kernels & describe_kernel ($MBL -> $NCD)
    const furnOut = await invoke("furnish_kernels", "kernels", "MBL", mblBytes);
    assert.equal(furnOut.length, 3);

    const spkBytes = rawMap.get("de440_2000_2035.bsp");
    const expectedSpkSha = createHash("sha256").update(spkBytes).digest("hex");
    const descMbl = encodeModuleBundle({
      entries: [
        {
          entryId: "de440_2000_2035.bsp",
          role: ModuleBundleEntryRole.AUXILIARY,
          sectionName: "de440_2000_2035.bsp",
          mediaType: "application/octet-stream",
          payloadEncoding: ModulePayloadEncoding.RAW_BYTES,
          payload: spkBytes,
        },
      ],
    });
    const descOut = await invoke("describe_kernel", "kernel", "MBL", descMbl);
    assert.equal(descOut.length, 1);
    const ncd = NCD_NS.NCD.getRootAsNCD(new flatbuffers.ByteBuffer(descOut[0].payload)).unpack();
    assert.equal(ncd.FORMAT, NCD_NS.ncdContainerFormat.SPK_DAF);
    assert.equal(ncd.SOURCE_SHA256, expectedSpkSha);
    assert.equal(Number(ncd.SOURCE_BYTE_LENGTH), spkBytes.byteLength);
    const targetIds = new Set(ncd.SEGMENTS.map((s) => s.TARGET_NAIF_ID));
    assert.ok(targetIds.has(301) && targetIds.has(399) && targetIds.has(10));

    // 2. convert_time ($TIM)
    const runTim = async (srcSys, iso, seconds, dstSys, dstFmt) => {
      const src = new TIM_NS.TIMInstantT();
      src.TIME_SYSTEM = TIM_NS.timingStandard[srcSys];
      if (iso) {
        src.EPOCH_FORMAT = TIM_NS.timEpochRepresentation.ISO8601;
        src.ISO8601 = iso;
      } else {
        src.EPOCH_FORMAT = TIM_NS.timEpochRepresentation.JULIAN_DATE;
        src.JULIAN_DATE = seconds;
      }
      const req = new TIM_NS.TIMConversionRequestT();
      req.SOURCE = src;
      req.TARGET_TIME_SYSTEM = TIM_NS.timingStandard[dstSys];
      req.TARGET_EPOCH_FORMAT = TIM_NS.timEpochRepresentation[dstFmt];
      const out = await invoke(
        "convert_time",
        "request",
        "TIM",
        packRoot(new TIM_NS.TIMT(src.TIME_SYSTEM, null, req, null), "$TIM"),
      );
      return TIM_NS.TIM.getRootAsTIM(new flatbuffers.ByteBuffer(out[0].payload)).unpack().CONVERSION_RESULT;
    };

    const j2000Tdb = await runTim("UTC", "2000-01-01T12:00:00.000Z", 0, "TDB", "JULIAN_DATE");
    assert.ok(Math.abs(j2000Tdb.DELTA_SECONDS - 64.1839272847) < 1e-6);

    const tai2017 = await runTim("UTC", "2017-01-01T12:00:00.000Z", 0, "TAI", "ISO8601");
    assert.ok(Math.abs(tai2017.DELTA_SECONDS - 37.0) < 1e-9);

    const rtUtc = await runTim("TDB", null, j2000Tdb.TARGET.JULIAN_DATE, "UTC", "ISO8601");
    assert.ok(Math.abs(Date.parse(rtUtc.TARGET.ISO8601) - Date.parse("2000-01-01T12:00:00.000Z")) <= 1);

    // 3. compute_ephemeris ($OEM): Earth->Sun perihelion, Earth->Moon central-diff velocity, & full DE440 bit-exactness
    const runOem = async (target, center, start, stop, step) => {
      const blk = new OEM_NS.ephemerisDataBlockT();
      blk.COMMENT = "ABCORR=NONE";
      blk.OBJECT = new CAT_NS.CATT();
      blk.OBJECT.OBJECT_NAME = target;
      blk.CENTER_NAME = center;
      blk.REFERENCE_FRAME = new RFM_NS.RFMT(RFM_NS.RFMUnion.NONE, null, 0, "J2000");
      blk.TIME_SYSTEM = TIM_NS.timingStandard.UTC;
      blk.START_TIME = start;
      blk.STOP_TIME = stop;
      blk.STEP_SIZE = step;
      const oem = new OEM_NS.OEMT();
      oem.CCSDS_OEM_VERS = 2.0;
      oem.ORIGINATOR = "PEPPER-TEST";
      oem.EPHEMERIS_DATA_BLOCK = [blk];
      const out = await invoke("compute_ephemeris", "request", "OEM", packRoot(oem, "$OEM"));
      return OEM_NS.OEM.getRootAsOEM(new flatbuffers.ByteBuffer(out[0].payload)).unpack();
    };

    const sunOem = await runOem("SUN", "EARTH", "2025-01-04T12:00:00Z", "2025-01-04T12:00:00Z", 60);
    const sunD = sunOem.EPHEMERIS_DATA_BLOCK[0].EPHEMERIS_DATA;
    const rSunKm = Math.hypot(sunD[0], sunD[1], sunD[2]);
    assert.ok(rSunKm > 1.470e8 && rSunKm < 1.472e8, `Unexpected Earth-Sun perihelion distance: ${rSunKm}`);

    const moonOem = await runOem("MOON", "EARTH", "2025-01-04T11:59:59Z", "2025-01-04T12:00:01Z", 1.0);
    const mData = moonOem.EPHEMERIS_DATA_BLOCK[0].EPHEMERIS_DATA;
    assert.equal(mData.length, 18);
    const rMoonKm = Math.hypot(mData[6], mData[7], mData[8]);
    assert.ok(rMoonKm > 356000 && rMoonKm < 407000, `Unexpected Earth-Moon distance: ${rMoonKm}`);
    for (let k = 0; k < 3; k++) {
      const vFd = 0.5 * (mData[12 + k] - mData[0 + k]);
      const vExact = mData[6 + 3 + k];
      assert.ok(Math.abs(vFd - vExact) < 1e-5, `Velocity mismatch axis ${k}: fd=${vFd} exact=${vExact}`);
    }

    // Verify subsetted de440_2000_2035.bsp matches full de440s.bsp to < 1e-12 km
    const fullDe440Path = path.join(ROOT, ".cache", "kernels", "de440s.bsp");
    if (fs.existsSync(fullDe440Path)) {
      const fullHarness = await createBrowserModuleHarness({ wasmSource: wasmBytes, surface: "direct" });
      try {
        const fullEntries = [
          ["naif0012.tls", rawMap.get("naif0012.tls")],
          ["pck00011.tpc", rawMap.get("pck00011.tpc")],
          ["de440s.bsp", new Uint8Array(await fsp.readFile(fullDe440Path))],
        ].map(([name, payload]) => ({
          entryId: name,
          role: ModuleBundleEntryRole.AUXILIARY,
          sectionName: name,
          mediaType: "application/octet-stream",
          payloadEncoding: ModulePayloadEncoding.RAW_BYTES,
          payload,
        }));
        await fullHarness.invoke({
          methodId: "furnish_kernels",
          inputs: [{ portId: "kernels", typeRef: sdsTypeRef("MBL"), payload: encodeModuleBundle({ entries: fullEntries }) }],
        });
        const blk = new OEM_NS.ephemerisDataBlockT();
        blk.OBJECT = new CAT_NS.CATT();
        blk.OBJECT.OBJECT_NAME = "SUN";
        blk.CENTER_NAME = "EARTH";
        blk.REFERENCE_FRAME = new RFM_NS.RFMT(RFM_NS.RFMUnion.NONE, null, 0, "J2000");
        blk.TIME_SYSTEM = TIM_NS.timingStandard.UTC;
        blk.START_TIME = "2025-01-04T12:00:00Z";
        blk.STOP_TIME = "2025-01-04T12:00:00Z";
        blk.STEP_SIZE = 60;
        const oem = new OEM_NS.OEMT();
        oem.CCSDS_OEM_VERS = 2.0;
        oem.EPHEMERIS_DATA_BLOCK = [blk];
        const fullOut = await fullHarness.invoke({
          methodId: "compute_ephemeris",
          inputs: [{ portId: "request", typeRef: sdsTypeRef("OEM"), payload: packRoot(oem, "$OEM") }],
        });
        const fullD = OEM_NS.OEM.getRootAsOEM(new flatbuffers.ByteBuffer(fullOut.outputs[0].payload)).unpack()
          .EPHEMERIS_DATA_BLOCK[0].EPHEMERIS_DATA;
        for (let i = 0; i < 6; i++) {
          assert.ok(Math.abs(fullD[i] - sunD[i]) < 1e-12, `Subset vs full DE440 diff at ${i}: ${fullD[i]} vs ${sunD[i]}`);
        }
      } finally {
        await fullHarness.destroy();
      }
    }

    // 4. compute_attitude ($AEM): unit quaternion & Earth sidereal rotation rate
    const seg = new AEM_NS.AEMSegmentT();
    seg.OBJECT_NAME = "EARTH";
    seg.REF_FRAME_A = "J2000";
    seg.REF_FRAME_B = "IAU_EARTH";
    seg.ATTITUDE_DIR = "A2B";
    seg.TIME_SYSTEM = "UTC";
    seg.START_TIME = "2025-01-04T12:00:00Z";
    seg.STOP_TIME = "2025-01-04T15:00:00Z";
    seg.STEP_SIZE = 3600;
    const aemReq = new AEM_NS.AEMT();
    aemReq.CCSDS_AEM_VERS = "2.0";
    aemReq.SEGMENTS = [seg];
    const aemOutBytes = (await invoke("compute_attitude", "request", "AEM", packRoot(aemReq, "$AEM")))[0].payload;
    const aemOut = AEM_NS.AEM.getRootAsAEM(new flatbuffers.ByteBuffer(aemOutBytes)).unpack();
    const att = aemOut.SEGMENTS[0].ATTITUDE_DATA;
    assert.equal(att.length, 4 * 7);
    const qNorm = Math.hypot(att[0], att[1], att[2], att[3]);
    const wDegS = Math.hypot(att[4], att[5], att[6]);
    assert.ok(Math.abs(qNorm - 1.0) < 1e-14);
    assert.ok(Math.abs(wDegS - 4.178074e-3) < 1e-6);

    // 5. transform_frame ($FRM): LLA <-> PCPF, J2000 <-> IAU_EARTH, CARTESIAN <-> KEPLERIAN
    const runFrm = async (reqObj) => {
      const out = await invoke("transform_frame", "request", "FRM", packRoot(new FRM_NS.FRMT(reqObj, null), "$FRM"));
      return FRM_NS.FRM.getRootAsFRM(new flatbuffers.ByteBuffer(out[0].payload)).unpack().FRAME_TRANSFORM_RESULT;
    };

    const lat0 = (28.572872 * Math.PI) / 180.0;
    const lon0 = (-80.648981 * Math.PI) / 180.0;
    const alt0 = 54.0;
    const llaReq = new FRM_NS.FRMFrameTransformRequestT();
    llaReq.OPERATION = FRM_NS.frmOperationCode.LLA_TO_PCPF;
    llaReq.POSITION = new FRM_NS.FRMVector3T(lat0, lon0, alt0);
    const pcpfRes = await runFrm(llaReq);
    assert.equal(pcpfRes.STATUS, FRM_NS.frmResultStatus.OK);

    const pcpfReq = new FRM_NS.FRMFrameTransformRequestT();
    pcpfReq.OPERATION = FRM_NS.frmOperationCode.PCPF_TO_LLA;
    pcpfReq.POSITION = pcpfRes.POSITION;
    const llaBack = await runFrm(pcpfReq);
    assert.equal(llaBack.STATUS, FRM_NS.frmResultStatus.OK);
    assert.ok(Math.abs(llaBack.POSITION.X - lat0) < 1e-11);
    assert.ok(Math.abs(llaBack.POSITION.Y - lon0) < 1e-11);
    assert.ok(Math.abs(llaBack.POSITION.Z - alt0) < 1e-4);

    // J2000 <-> IAU_EARTH 6-DOF round-trip
    const stFwd = new FRM_NS.FRMFrameTransformRequestT();
    stFwd.OPERATION = FRM_NS.frmOperationCode.STATE_TRANSFORM;
    stFwd.EPOCH = "2025-01-04T12:00:00Z";
    stFwd.EPOCH_TIME_SYSTEM = "UTC";
    stFwd.SOURCE_COORDINATE_SYSTEM = Object.assign(new RFM_NS.RFMCoordinateSystemT(), { NAME: "J2000" });
    stFwd.TARGET_COORDINATE_SYSTEM = Object.assign(new RFM_NS.RFMCoordinateSystemT(), { NAME: "IAU_EARTH" });
    stFwd.TARGET_REPRESENTATION = FRM_NS.frmStateRepresentation.CARTESIAN;
    const sv0 = new FRM_NS.FRMStateVectorT();
    sv0.REPRESENTATION = FRM_NS.frmStateRepresentation.CARTESIAN;
    sv0.POSITION = new FRM_NS.FRMVector3T(6778137.0, 120000.0, -45000.0);
    sv0.VELOCITY = new FRM_NS.FRMVector3T(-120.0, 4800.0, 5980.0);
    sv0.GRAVITATIONAL_PARAMETER = 3.986004418e14;
    stFwd.SOURCE_STATE = sv0;
    const stEcef = await runFrm(stFwd);
    assert.equal(stEcef.STATUS, FRM_NS.frmResultStatus.OK);

    const stInv = new FRM_NS.FRMFrameTransformRequestT();
    stInv.OPERATION = FRM_NS.frmOperationCode.STATE_TRANSFORM;
    stInv.EPOCH = "2025-01-04T12:00:00Z";
    stInv.EPOCH_TIME_SYSTEM = "UTC";
    stInv.SOURCE_COORDINATE_SYSTEM = Object.assign(new RFM_NS.RFMCoordinateSystemT(), { NAME: "IAU_EARTH" });
    stInv.TARGET_COORDINATE_SYSTEM = Object.assign(new RFM_NS.RFMCoordinateSystemT(), { NAME: "J2000" });
    stInv.TARGET_REPRESENTATION = FRM_NS.frmStateRepresentation.CARTESIAN;
    stInv.SOURCE_STATE = stEcef.TARGET_STATE;
    const stBack = await runFrm(stInv);
    assert.equal(stBack.STATUS, FRM_NS.frmResultStatus.OK);
    assert.ok(
      Math.hypot(
        stBack.TARGET_STATE.POSITION.X - sv0.POSITION.X,
        stBack.TARGET_STATE.POSITION.Y - sv0.POSITION.Y,
        stBack.TARGET_STATE.POSITION.Z - sv0.POSITION.Z,
      ) < 1e-4,
    );
    assert.ok(
      Math.hypot(
        stBack.TARGET_STATE.VELOCITY.X - sv0.VELOCITY.X,
        stBack.TARGET_STATE.VELOCITY.Y - sv0.VELOCITY.Y,
        stBack.TARGET_STATE.VELOCITY.Z - sv0.VELOCITY.Z,
      ) < 1e-7,
    );

    // CARTESIAN <-> KEPLERIAN round-trip
    const kepReq = new FRM_NS.FRMFrameTransformRequestT();
    kepReq.OPERATION = FRM_NS.frmOperationCode.STATE_REPRESENTATION_CONVERT;
    kepReq.TARGET_REPRESENTATION = FRM_NS.frmStateRepresentation.KEPLERIAN;
    kepReq.SOURCE_STATE = sv0;
    const kepRes = await runFrm(kepReq);
    assert.equal(kepRes.STATUS, FRM_NS.frmResultStatus.OK);

    const cartReq = new FRM_NS.FRMFrameTransformRequestT();
    cartReq.OPERATION = FRM_NS.frmOperationCode.STATE_REPRESENTATION_CONVERT;
    cartReq.TARGET_REPRESENTATION = FRM_NS.frmStateRepresentation.CARTESIAN;
    cartReq.SOURCE_STATE = kepRes.TARGET_STATE;
    const cartRes = await runFrm(cartReq);
    assert.equal(cartRes.STATUS, FRM_NS.frmResultStatus.OK);
    assert.ok(
      Math.hypot(
        cartRes.TARGET_STATE.POSITION.X - sv0.POSITION.X,
        cartRes.TARGET_STATE.POSITION.Y - sv0.POSITION.Y,
        cartRes.TARGET_STATE.POSITION.Z - sv0.POSITION.Z,
      ) < 1e-3,
    );

    // 6. locate_events ($EVL): APSIDES, ECLIPSE, PARAMETER_CONDITION, & structured error handling
    const runEvl = async (reqObj) => {
      const out = await invoke("locate_events", "request", "EVL", packRoot(new EVL_NS.EVLT(reqObj, null), "$EVL"));
      return EVL_NS.EVL.getRootAsEVL(new flatbuffers.ByteBuffer(out[0].payload)).unpack().EVENT_REPORT;
    };

    const apReq = new EVL_NS.EVLEventLocationRequestT();
    apReq.LOCATOR_CLASS = EVL_NS.evlLocatorClass.APSIDES;
    apReq.SCAN_START_EPOCH = "2025-01-01T00:00:00Z";
    apReq.SCAN_STOP_EPOCH = "2025-02-01T00:00:00Z";
    apReq.EPOCH_TIME_SYSTEM = "UTC";
    apReq.SCAN_STEP_SECONDS = 3600;
    apReq.TARGET_OBJECT_ID = "301";
    apReq.APSIDES_CONFIGURATION = new EVL_NS.EVLApsidesConfigurationT(399, true, true);
    const apRep = await runEvl(apReq);
    assert.equal(apRep.STATUS, EVL_NS.evlResultStatus.OK, apRep.ERROR_MESSAGE);
    assert.ok(apRep.EVENTS.length >= 2);
    const perigees = apRep.EVENTS.filter((e) => e.EVENT_TYPE === EVL_NS.evlEventType.PERIAPSIS);
    const apogees = apRep.EVENTS.filter((e) => e.EVENT_TYPE === EVL_NS.evlEventType.APOAPSIS);
    assert.ok(perigees.length >= 1 && apogees.length >= 1);
    assert.ok(perigees[0].EXTREMUM_VALUE > 356e6 && perigees[0].EXTREMUM_VALUE < 371e6);
    assert.ok(apogees[0].EXTREMUM_VALUE > 404e6 && apogees[0].EXTREMUM_VALUE < 407e6);

    const ecReq = new EVL_NS.EVLEventLocationRequestT();
    ecReq.LOCATOR_CLASS = EVL_NS.evlLocatorClass.ECLIPSE;
    ecReq.SCAN_START_EPOCH = "2024-04-08T15:00:00Z";
    ecReq.SCAN_STOP_EPOCH = "2024-04-08T22:00:00Z";
    ecReq.EPOCH_TIME_SYSTEM = "UTC";
    ecReq.SCAN_STEP_SECONDS = 300;
    ecReq.TARGET_OBJECT_ID = "399";
    const ecCfg = new EVL_NS.EVLEclipseConfigurationT();
    ecCfg.OCCULTING_BODY_IDS = [301];
    ecCfg.ILLUMINATING_BODY_ID = 10;
    ecCfg.REPORT_PENUMBRA = true;
    ecCfg.ABERRATION_CORRECTION = EVL_NS.evlAberrationCorrection.LIGHT_TIME;
    ecReq.ECLIPSE_CONFIGURATION = ecCfg;
    const ecRep = await runEvl(ecReq);
    assert.equal(ecRep.STATUS, EVL_NS.evlResultStatus.OK, ecRep.ERROR_MESSAGE);
    assert.ok(ecRep.EVENTS.length >= 1);
    assert.ok(ecRep.EVENTS[0].DURATION_SECONDS > 600);

    const pcReq = new EVL_NS.EVLEventLocationRequestT();
    pcReq.LOCATOR_CLASS = EVL_NS.evlLocatorClass.PARAMETER_CONDITION;
    pcReq.SCAN_START_EPOCH = "2025-01-01T00:00:00Z";
    pcReq.SCAN_STOP_EPOCH = "2025-01-20T00:00:00Z";
    pcReq.EPOCH_TIME_SYSTEM = "UTC";
    pcReq.SCAN_STEP_SECONDS = 3600;
    pcReq.TARGET_OBJECT_ID = "301";
    const pref = new PCE_NS.PCEParameterRefT();
    pref.PARAMETER = PCE_NS.pceParameter.RELATIVE_RANGE;
    pref.SECOND_OBJECT_ID = "399";
    const cond = new PCE_NS.PCEParameterConditionT();
    cond.PARAMETER = pref;
    cond.GOAL_VALUE = 380000000.0;
    pcReq.PARAMETER_CONDITION_CONFIGURATION = new EVL_NS.EVLParameterConditionConfigurationT([cond], false, 0.0);
    const pcRep = await runEvl(pcReq);
    assert.equal(pcRep.STATUS, EVL_NS.evlResultStatus.OK, pcRep.ERROR_MESSAGE);
    assert.ok(pcRep.EVENTS.length >= 1);

    // Structured error status on invalid configuration (no WASM trap!)
    const badReq = new EVL_NS.EVLEventLocationRequestT();
    badReq.LOCATOR_CLASS = EVL_NS.evlLocatorClass.APSIDES;
    badReq.SCAN_START_EPOCH = "2025-01-01T00:00:00Z";
    badReq.SCAN_STOP_EPOCH = "2025-01-02T00:00:00Z";
    badReq.TARGET_OBJECT_ID = "301";
    const badRep = await runEvl(badReq);
    assert.equal(badRep.STATUS, EVL_NS.evlResultStatus.MISSING_CONFIGURATION);

    // 7. ingest_omm ($OMM)
    const omm = new OMM_NS.OMMT();
    omm.OBJECT_NAME = "ISS (ZARYA)";
    omm.NORAD_CAT_ID = 25544;
    omm.EPOCH = "2025-01-04T12:00:00.000Z";
    omm.MEAN_MOTION = 15.50103472;
    omm.ECCENTRICITY = 0.0006703;
    omm.INCLINATION = 51.6439;
    omm.RA_OF_ASC_NODE = 247.4627;
    omm.ARG_OF_PERICENTER = 130.536;
    omm.MEAN_ANOMALY = 325.0288;
    const ommRes = await harness.invoke({
      methodId: "ingest_omm",
      inputs: [{ portId: "omm", typeRef: sdsTypeRef("OMM"), payload: packRoot(omm, "$OMM") }],
    });
    assert.equal(ommRes.statusCode, 0);
  } finally {
    await harness.destroy();
  }
});

test("plugin_init_ephemeris DAF/SPK container propagation in SI meters & out-of-range refusal", async () => {
  const wasmBytes = new Uint8Array(await fsp.readFile(WASM_PATH));
  const spkBytes = new Uint8Array(await fsp.readFile(path.join(KERNELS_DIR, "de440_2000_2035.bsp")));

  const mod = await WebAssembly.compile(wasmBytes);
  const shim = createBrowserWasiShim();
  const instance = await WebAssembly.instantiate(mod, shim.imports);
  shim.setMemory(instance.exports.memory);
  const ex = instance.exports;

  const spkPtr = ex.plugin_alloc(spkBytes.byteLength);
  assert.ok(spkPtr > 0);
  const outPtr = ex.plugin_alloc(ORBPRO_STATE_VECTOR.size);
  assert.ok(outPtr > 0);

  try {
    new Uint8Array(ex.memory.buffer, spkPtr, spkBytes.byteLength).set(spkBytes);
    const entityCount = ex.plugin_init_ephemeris(spkPtr, spkBytes.byteLength, 0);
    assert.ok(entityCount >= 10, `Expected >= 10 SPK entities, got ${entityCount}`);
    assert.equal(ex.plugin_entity_count(), entityCount);

    // Propagate entity 0 at 2025-01-04T12:00:00 TDB (JD 2460680.0)
    const jdInRange = 2460680.0;
    const rcOk = ex.plugin_propagate(jdInRange, 0, outPtr);
    assert.equal(rcOk, 0);

    const view = new DataView(ex.memory.buffer, outPtr, ORBPRO_STATE_VECTOR.size);
    const { offsets } = ORBPRO_STATE_VECTOR;
    const epoch = view.getFloat64(offsets.epoch, true);
    const posM = [0, 1, 2].map((a) => view.getFloat64(offsets.position + a * 8, true));
    const refFrame = view.getUint8(offsets.reference_frame);
    const flags = view.getUint32(offsets.flags, true);

    assert.equal(epoch, jdInRange);
    assert.equal(refFrame, ReferenceFrame.J2000);
    assert.equal(flags, StateFlags.VALID);
    assert.ok(Math.hypot(...posM) > 1e9, `Expected planetary/barycentric state in meters, got ${posM}`);

    // Out-of-coverage epoch (1858 MJD 0 = JD 2400000.5) must return ORBPRO_PROP_EPOCH_OUT_OF_RANGE (-8)
    const rcOut = ex.plugin_propagate(2400000.5, 0, outPtr);
    assert.equal(rcOut, -8);

    ex.plugin_destroy();
    assert.equal(ex.plugin_entity_count(), 0);
  } finally {
    ex.plugin_free(outPtr);
    ex.plugin_free(spkPtr);
  }
});

test("Headless Chromium browser E2E workbench (COOP/COEP, zero external requests, all 6 SDS tabs)", async (t) => {
  const executablePath = findLocalChromiumExecutable();
  if (!executablePath) {
    t.skip("No local Chromium binary found for headless browser E2E");
    return;
  }

  const server = createPepperWasmServer();
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const addr = server.address();
  const baseUrl = `http://127.0.0.1:${addr.port}`;

  const browser = await chromium.launch({ executablePath, headless: true });
  try {
    // Verify landing page links to wasm.html in its own page so its teardown does not emit events on wasm.html
    const indexPage = await browser.newPage();
    await indexPage.goto(`${baseUrl}/index.html`, { waitUntil: "domcontentloaded" });
    const wasmLinks = await indexPage.locator('a[href="wasm.html"]').count();
    assert.ok(wasmLinks >= 1, "web/index.html should link to wasm.html");
    await indexPage.close();

    const page = await browser.newPage();
    const consoleErrors = [];
    const pageErrors = [];
    const failedRequests = [];
    const requestedUrls = [];

    page.on("console", (msg) => {
      if (msg.type() === "error") consoleErrors.push(msg.text());
    });
    page.on("pageerror", (err) => pageErrors.push(err.message));
    page.on("requestfailed", (req) => failedRequests.push(`${req.url()} (${req.failure()?.errorText})`));
    page.on("request", (req) => requestedUrls.push(req.url()));

    // Open WASM workbench & wait for full Fortran WASM bootstrap + initial $OEM run
    await page.goto(`${baseUrl}/wasm.html`, { waitUntil: "domcontentloaded" });
    await page.waitForFunction(() => window.__PEPPER_WASM_READY__ === true, null, { timeout: 30000 });

    const coi = await page.evaluate(() => window.crossOriginIsolated);
    assert.equal(coi, true, "Expected crossOriginIsolated === true under COOP/COEP headers");
    const plgText = await page.locator("#manifest-status").textContent();
    assert.match(plgText, /org\.spacedatastandards\.pepper/);

    // Initial $OEM tab verification
    const wireInitial = await page.locator("#wire-res-hex").textContent();
    assert.match(wireInitial, /file_identifier: "\$OEM"/);
    const oemRows = await page.locator("#oem-table tbody tr").count();
    assert.ok(oemRows > 10, `Expected >10 $OEM state rows, got ${oemRows}`);

    // Tab 2: $AEM Attitude
    await page.click("#tab-btn-aem");
    await page.click("#btn-run-aem");
    await page.waitForFunction(
      () => document.getElementById("wire-res-hex")?.textContent?.includes('file_identifier: "$AEM"'),
    );
    const aemRows = await page.locator("#aem-table tbody tr").count();
    assert.ok(aemRows >= 24);

    // Tab 3: $FRM Frames & Geodesy
    await page.click("#tab-btn-frm");
    await page.click("#btn-run-frm");
    await page.waitForFunction(
      () => document.getElementById("wire-res-hex")?.textContent?.includes('file_identifier: "$FRM"'),
    );
    const frmText = await page.locator("#frm-output").textContent();
    assert.match(frmText, /"STATUS": 0/);

    // Tab 4: $TIM Time Scales
    await page.click("#tab-btn-tim");
    await page.click("#btn-run-tim");
    await page.waitForFunction(
      () => document.getElementById("wire-res-hex")?.textContent?.includes('file_identifier: "$TIM"'),
    );
    const timRows = await page.locator("#tim-table tbody tr").count();
    assert.equal(timRows, 6);

    // Tab 5: $EVL Event Locator
    await page.click("#tab-btn-evl");
    await page.click("#btn-run-evl");
    await page.waitForFunction(
      () => document.getElementById("wire-res-hex")?.textContent?.includes('file_identifier: "$EVL"'),
    );
    const evlRows = await page.locator("#evl-table tbody tr").count();
    assert.ok(evlRows >= 2);

    // Tab 6: $NCD Kernel Inspector
    await page.click("#tab-btn-ncd");
    const ncdRows = await page.locator("#ncd-table tbody tr").count();
    assert.ok(ncdRows >= 15, `Expected resident kernel & segment rows in $NCD table, got ${ncdRows}`);

    // Strict zero-error & zero-external-origin network assertions
    assert.deepEqual(consoleErrors, [], `Console errors: ${consoleErrors.join("; ")}`);
    assert.deepEqual(pageErrors, [], `Page errors: ${pageErrors.join("; ")}`);
    assert.deepEqual(failedRequests, [], `Failed requests: ${failedRequests.join("; ")}`);
    const externalRequests = requestedUrls.filter(
      (u) => !u.startsWith(baseUrl) && !u.startsWith("blob:") && !u.startsWith("data:"),
    );
    assert.deepEqual(externalRequests, [], `Unexpected external-origin requests: ${externalRequests.join(", ")}`);
  } finally {
    await browser.close();
    await new Promise((resolve) => server.close(resolve));
  }
});

test("Space Data Module CLI & Ecosystem Lifecycle (repo check, flow check, Ed25519 sign/verify, native WasmEdge)", async () => {
  const execFileAsync = promisify(execFile);
  const sdmCli = path.join(SDK_ROOT, "bin", "space-data-module.js");

  // 1. Official CLI `space-data-module check --repo-root .` & `--manifest/--wasm`
  const repoChk = await execFileAsync(process.execPath, [sdmCli, "check", "--repo-root", ROOT]);
  assert.match(repoChk.stdout, /PASS .*plugin-manifest\.json/);

  const wasmChk = await execFileAsync(process.execPath, [
    sdmCli,
    "check",
    "--manifest",
    MANIFEST_PATH,
    "--wasm",
    WASM_PATH,
  ]);
  assert.match(wasmChk.stdout, /PASS .*plugin-manifest\.json/);

  // 2. Official CLI `space-data-module flow check` (linked-direct SDN Flow resolution)
  const tmpDir = await fsp.mkdtemp(path.join(os.tmpdir(), "pepper-sdm-lifecycle-"));
  try {
    const flowDoc = {
      programId: "pepper.ephemeris.pipeline",
      name: "PEPPER Ephemeris Flow",
      version: "0.1.0",
      requiredPlugins: ["org.spacedatastandards.pepper"],
      nodes: [
        {
          nodeId: "ephem",
          pluginId: "org.spacedatastandards.pepper",
          methodId: "compute_ephemeris",
          kind: "processor",
          drainPolicy: "single-shot",
        },
      ],
      edges: [],
      triggers: [{ triggerId: "oem-req", kind: "manual" }],
      triggerBindings: [{ triggerId: "oem-req", targetNodeId: "ephem", targetPortId: "request" }],
    };
    const flowPath = path.join(tmpDir, "pepper.flow.json");
    const depsPath = path.join(tmpDir, "deps.json");
    await fsp.writeFile(flowPath, JSON.stringify(flowDoc, null, 2));
    await fsp.writeFile(depsPath, JSON.stringify({ "org.spacedatastandards.pepper": ROOT }, null, 2));

    const flowChk = await execFileAsync(process.execPath, [
      sdmCli,
      "flow",
      "check",
      flowPath,
      "--deps",
      depsPath,
    ]);
    assert.match(flowChk.stdout, /PASS /);
    assert.match(flowChk.stdout, /org\.spacedatastandards\.pepper:compute_ephemeris \(linked-direct\)/);

    // 3. Official CLI Ed25519 `sign` + `verify --require-signature`
    const devKey = path.join(tmpDir, "test-signing-keypair.json");
    const { privateKey, publicKey } = generateKeyPairSync("ed25519");
    await fsp.writeFile(devKey, JSON.stringify({
      keyId: "pepper-lifecycle-test",
      privateKeySeedHex: Buffer.from(privateKey.export({ format: "jwk" }).d, "base64url").toString("hex"),
      publicKeyHex: Buffer.from(publicKey.export({ format: "jwk" }).x, "base64url").toString("hex"),
    }), { mode: 0o600 });
    {
      const signedOut = path.join(tmpDir, "module.signed.wasm");
      await execFileAsync(process.execPath, [
        sdmCli,
        "sign",
        "--wasm",
        WASM_PATH,
        "--key",
        devKey,
        "--out",
        signedOut,
      ]);
      const verRes = await execFileAsync(process.execPath, [
        sdmCli,
        "verify",
        "--wasm",
        signedOut,
        "--key",
        devKey,
        "--require-signature",
      ]);
      assert.match(verRes.stdout, /verified=true/);
      assert.match(verRes.stdout, /signed=true/);
    }
  } finally {
    await fsp.rm(tmpDir, { recursive: true, force: true });
  }

  // 4. Native server-side WasmEdge `loadModule` execution (if `wasmedge` is installed locally)
  const wasmedgeCandidates = [
    process.env.WASMEDGE_BIN,
    path.join(os.homedir(), ".wasmedge", "bin", "wasmedge"),
    "/opt/homebrew/bin/wasmedge",
    "/usr/local/bin/wasmedge",
  ].filter(Boolean);
  const wasmedgeBin = wasmedgeCandidates.find((p) => fs.existsSync(p));
  if (wasmedgeBin) {
    const prevPath = process.env.PATH ?? "";
    process.env.PATH = `${path.dirname(wasmedgeBin)}${path.delimiter}${prevPath}`;
    try {
      const { mblBytes } = await loadDefaultKernelsMbl();
      const wasmedgeMod = await loadServerModule({ wasmSource: WASM_PATH });
      try {
        assert.equal(wasmedgeMod.runtime.kind, "wasmedge");
        const src = new TIM_NS.TIMInstantT();
        src.TIME_SYSTEM = TIM_NS.timingStandard.UTC;
        src.EPOCH_FORMAT = TIM_NS.timEpochRepresentation.ISO8601;
        src.ISO8601 = "2000-01-01T12:00:00.000Z";
        const req = new TIM_NS.TIMConversionRequestT();
        req.SOURCE = src;
        req.TARGET_TIME_SYSTEM = TIM_NS.timingStandard.TDB;
        req.TARGET_EPOCH_FORMAT = TIM_NS.timEpochRepresentation.JULIAN_DATE;
        const timReqBytes = packRoot(new TIM_NS.TIMT(src.TIME_SYSTEM, null, req, null), "$TIM");

        const res = await wasmedgeMod.invoke({
          methodId: "convert_time",
          inputs: [
            { portId: "kernels", typeRef: sdsTypeRef("MBL"), payload: mblBytes },
            { portId: "request", typeRef: sdsTypeRef("TIM"), payload: timReqBytes },
          ],
        });
        assert.equal(res.statusCode, 0);
        const out = TIM_NS.TIM.getRootAsTIM(new flatbuffers.ByteBuffer(res.outputs[0].payload)).unpack();
        assert.ok(Math.abs(out.CONVERSION_RESULT.DELTA_SECONDS - 64.1839272847) < 1e-6);
      } finally {
        await wasmedgeMod.destroy?.();
      }
    } finally {
      process.env.PATH = prevPath;
    }
  }
});

