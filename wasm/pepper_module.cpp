#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>
#include <vector>

extern "C" {
#ifdef __wasm32__
#include "pepper_wasm.h"
#else
#include "pepper.h"
#endif
}

#include "orbpro/orbpro_propagator_abi.h"
#include "space_data_module_invoke.h"

#include "sds/AEM/main_generated.h"
#include "sds/EVL/main_generated.h"
#include "sds/FRM/main_generated.h"
#include "sds/MBL/main_generated.h"
#include "sds/NCD/main_generated.h"
#include "sds/OEM/main_generated.h"
#include "sds/OMM/main_generated.h"
#include "sds/TIM/main_generated.h"

namespace {

constexpr double kPi = 3.141592653589793238462643383279502884;
constexpr double kDegToRad = kPi / 180.0;
constexpr double kRadToDeg = 180.0 / kPi;
constexpr double kSecPerDay = 86400.0;
constexpr double kJ2000Jd = 2451545.0;
constexpr double kMjdOffset = 2400000.5;
constexpr double kUnixJ2000Sec = 946728000.0;  // 2000-01-01T12:00:00Z - 1970-01-01T00:00:00Z
constexpr double kGpsJ2000Sec = 630763200.0;   // 2000-01-01T12:00:00 GPS - 1980-01-06T00:00:00 GPS
constexpr double kTaiMinusGps = 19.0;
constexpr double kTtMinusTai = 32.184;
constexpr double kA1MinusTai = 0.0343817;
constexpr double kWgs84ReMeters = 6378137.0;
constexpr double kWgs84RpMeters = 6356752.314245179;
constexpr double kWgs84MuMeters3S2 = 3.986004418e14;
constexpr double kWgs72MuMeters3S2 = 398600.8e9;
constexpr double kEarthOmegaRadS = 7.292115146706979e-5;

// ============================================================================
// Minimal FIPS 180-4 SHA-256 (zero external dependencies)
// ============================================================================

struct Sha256 {
  uint32_t state[8];
  uint64_t bitlen;
  uint8_t data[64];
  size_t datalen;

  static inline uint32_t rotr(uint32_t x, uint32_t n) {
    return (x >> n) | (x << (32U - n));
  }

  void init() {
    datalen = 0;
    bitlen = 0;
    state[0] = 0x6a09e667U;
    state[1] = 0xbb67ae85U;
    state[2] = 0x3c6ef372U;
    state[3] = 0xa54ff53aU;
    state[4] = 0x510e527fU;
    state[5] = 0x9b05688cU;
    state[6] = 0x1f83d9abU;
    state[7] = 0x5be0cd19U;
  }

  void transform(const uint8_t block[64]) {
    static constexpr uint32_t k[64] = {
        0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U, 0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
        0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U, 0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
        0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU, 0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
        0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U, 0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
        0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U, 0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
        0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U, 0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
        0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U, 0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
        0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U, 0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U};
    uint32_t m[64];
    for (int i = 0, j = 0; i < 16; ++i, j += 4) {
      m[i] = (static_cast<uint32_t>(block[j]) << 24) |
             (static_cast<uint32_t>(block[j + 1]) << 16) |
             (static_cast<uint32_t>(block[j + 2]) << 8) |
             static_cast<uint32_t>(block[j + 3]);
    }
    for (int i = 16; i < 64; ++i) {
      uint32_t s0 = rotr(m[i - 15], 7) ^ rotr(m[i - 15], 18) ^ (m[i - 15] >> 3);
      uint32_t s1 = rotr(m[i - 2], 17) ^ rotr(m[i - 2], 19) ^ (m[i - 2] >> 10);
      m[i] = m[i - 16] + s0 + m[i - 7] + s1;
    }
    uint32_t a = state[0], b = state[1], c = state[2], d = state[3];
    uint32_t e = state[4], f = state[5], g = state[6], h = state[7];
    for (int i = 0; i < 64; ++i) {
      uint32_t S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
      uint32_t ch = (e & f) ^ ((~e) & g);
      uint32_t temp1 = h + S1 + ch + k[i] + m[i];
      uint32_t S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
      uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
      uint32_t temp2 = S0 + maj;
      h = g;
      g = f;
      f = e;
      e = d + temp1;
      d = c;
      c = b;
      b = a;
      a = temp1 + temp2;
    }
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
  }

  void update(const uint8_t *in, size_t len) {
    for (size_t i = 0; i < len; ++i) {
      data[datalen++] = in[i];
      if (datalen == 64) {
        transform(data);
        bitlen += 512;
        datalen = 0;
      }
    }
  }

  void final_bytes(uint8_t hash[32]) {
    size_t i = datalen;
    if (datalen < 56) {
      data[i++] = 0x80;
      while (i < 56) data[i++] = 0x00;
    } else {
      data[i++] = 0x80;
      while (i < 64) data[i++] = 0x00;
      transform(data);
      std::memset(data, 0, 56);
    }
    bitlen += static_cast<uint64_t>(datalen) * 8ULL;
    data[63] = static_cast<uint8_t>(bitlen);
    data[62] = static_cast<uint8_t>(bitlen >> 8);
    data[61] = static_cast<uint8_t>(bitlen >> 16);
    data[60] = static_cast<uint8_t>(bitlen >> 24);
    data[59] = static_cast<uint8_t>(bitlen >> 32);
    data[58] = static_cast<uint8_t>(bitlen >> 40);
    data[57] = static_cast<uint8_t>(bitlen >> 48);
    data[56] = static_cast<uint8_t>(bitlen >> 56);
    transform(data);
    for (int j = 0; j < 4; ++j) {
      for (int k = 0; k < 8; ++k) {
        hash[j + k * 4] = static_cast<uint8_t>((state[k] >> (24 - j * 8)) & 0xffU);
      }
    }
  }
};

std::string sha256_hex(const uint8_t *bytes, size_t len) {
  Sha256 ctx{};
  ctx.init();
  if (bytes && len > 0) ctx.update(bytes, len);
  uint8_t digest[32];
  ctx.final_bytes(digest);
  static constexpr char hex[] = "0123456789abcdef";
  std::string out(64, '0');
  for (int i = 0; i < 32; ++i) {
    out[2 * i] = hex[(digest[i] >> 4) & 0x0f];
    out[2 * i + 1] = hex[digest[i] & 0x0f];
  }
  return out;
}

std::string trim_copy(const std::string &s) {
  size_t b = 0;
  while (b < s.size() && (s[b] == ' ' || s[b] == '\t' || s[b] == '\r' || s[b] == '\n')) ++b;
  size_t e = s.size();
  while (e > b && (s[e - 1] == ' ' || s[e - 1] == '\t' || s[e - 1] == '\r' || s[e - 1] == '\n')) --e;
  return s.substr(b, e - b);
}

std::string upper_copy(std::string s) {
  for (char &c : s) {
    if (c >= 'a' && c <= 'z') c = static_cast<char>(c - 'a' + 'A');
  }
  return s;
}

// ============================================================================
// Resident PEPPER Runtime & Kernel State
// ============================================================================

bool g_runtime_initialized = false;
void *g_spice = nullptr;
void *g_ctx = nullptr;
std::string g_loaded_kernels_digest;

void ensure_pepper_ready() {
  if (!g_runtime_initialized) {
    pepper_runtime_init();
    g_runtime_initialized = true;
  }
  if (!g_spice) {
    g_spice = pepper_spice_new();
  }
  if (!g_ctx) {
    g_ctx = pepper_context_new(g_spice);
  }
  if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
  if (pepper_spice_failed(g_spice)) pepper_spice_reset(g_spice);
}

std::string capture_spice_error(void *spice) {
  char short_msg[128] = {0};
  char long_msg[1024] = {0};
  pepper_spice_getmsg(spice, "SHORT", sizeof(short_msg), short_msg);
  pepper_spice_getmsg(spice, "LONG", sizeof(long_msg), long_msg);
  pepper_spice_reset(spice);
  std::string s = trim_copy(short_msg);
  std::string l = trim_copy(long_msg);
  return l.empty() ? (s.empty() ? "SPICE kernel error" : s) : (s + ": " + l);
}

std::string capture_ctx_error(void *ctx) {
  char short_msg[128] = {0};
  char long_msg[1024] = {0};
  pepper_getmsg(ctx, "SHORT", sizeof(short_msg), short_msg);
  pepper_getmsg(ctx, "LONG", sizeof(long_msg), long_msg);
  pepper_reset(ctx);
  std::string s = trim_copy(short_msg);
  std::string l = trim_copy(long_msg);
  return l.empty() ? (s.empty() ? "SPICE context error" : s) : (s + ": " + l);
}

int fail_method(int code, const std::string &msg) {
  const char *err_code = (code == 422) ? "SPICE_EVALUATION_ERROR" : "INVALID_INPUT";
  plugin_set_error(err_code, msg.c_str());
  return code;
}

struct PluginInvokeSlice {
  const uint8_t *ptr;
  size_t size;
};

uint32_t plugin_input_stream_count(const char *port_id) {
  uint32_t count = 0;
  while (plugin_find_input_index(port_id, count) >= 0) {
    ++count;
  }
  return count;
}

PluginInvokeSlice plugin_get_input_stream(const char *port_id, uint32_t ordinal) {
  int32_t idx = plugin_find_input_index(port_id, ordinal);
  if (idx < 0) return PluginInvokeSlice{nullptr, 0};
  const plugin_input_frame_t *frame = plugin_get_input_frame(static_cast<uint32_t>(idx));
  if (!frame || !frame->payload) return PluginInvokeSlice{nullptr, 0};
  return PluginInvokeSlice{frame->payload, static_cast<size_t>(frame->payload_length)};
}

int emit_flatbuffer(const char *port_id, const char *schema_code,
                    const flatbuffers::FlatBufferBuilder &fbb) {
  std::string schema_name = std::string(schema_code) + ".fbs";
  std::string file_id = std::string("$") + schema_code;
  int32_t out_idx = plugin_push_output_ex(
      port_id,
      schema_name.c_str(),
      file_id.c_str(),
      PLUGIN_PAYLOAD_WIRE_FORMAT_FLATBUFFER,
      schema_code,
      0,
      0,
      fbb.GetBufferPointer(),
      static_cast<uint32_t>(fbb.GetSize()));
  return out_idx < 0 ? -1 : 0;
}

struct KernelBlobRef {
  std::string name;
  const uint8_t *bytes;
  size_t size;
};

bool collect_mbl_entries(const char *port_id, std::vector<KernelBlobRef> &out, std::string &err) {
  out.clear();
  uint32_t n_frames = plugin_input_stream_count(port_id);
  for (uint32_t i = 0; i < n_frames; ++i) {
    PluginInvokeSlice slice = plugin_get_input_stream(port_id, i);
    if (!slice.ptr || slice.size < 8) {
      err = "Empty or truncated $MBL frame";
      return false;
    }
    flatbuffers::Verifier verifier(slice.ptr, slice.size);
    if (!MBLBufferHasIdentifier(slice.ptr) || !VerifyMBLBuffer(verifier)) {
      err = "Invalid $MBL FlatBuffer payload on kernel port";
      return false;
    }
    const MBL *mbl = GetMBL(slice.ptr);
    if (!mbl || !mbl->entries()) continue;
    for (const ModuleBundleEntry *entry : *mbl->entries()) {
      if (!entry || !entry->payload() || entry->payload()->size() == 0) {
        err = "Empty kernel payload in $MBL ModuleBundleEntry";
        return false;
      }
      std::string name = entry->entry_id() ? entry->entry_id()->str() : "kernel.dat";
      out.push_back(KernelBlobRef{
          name,
          entry->payload()->data(),
          static_cast<size_t>(entry->payload()->size())});
    }
  }
  return true;
}

std::string compute_kernel_set_digest(const std::vector<KernelBlobRef> &blobs) {
  Sha256 h{};
  h.init();
  for (const auto &b : blobs) {
    std::string entry_hash = sha256_hex(b.bytes, b.size);
    h.update(reinterpret_cast<const uint8_t *>(b.name.data()), b.name.size());
    uint8_t sep = 0;
    h.update(&sep, 1);
    h.update(reinterpret_cast<const uint8_t *>(entry_hash.data()), entry_hash.size());
  }
  uint8_t raw[32];
  h.final_bytes(raw);
  static constexpr char hex[] = "0123456789abcdef";
  std::string out(64, '0');
  for (int i = 0; i < 32; ++i) {
    out[2 * i] = hex[(raw[i] >> 4) & 0x0f];
    out[2 * i + 1] = hex[raw[i] & 0x0f];
  }
  return out;
}

bool furnish_mbl_if_provided(const char *port_id, bool require_non_empty, std::string &err) {
  ensure_pepper_ready();
  uint32_t n_frames = plugin_input_stream_count(port_id);
  if (n_frames == 0) {
    if (require_non_empty) {
      err = "Expected at least one $MBL kernel stream";
      return false;
    }
    return true;
  }
  std::vector<KernelBlobRef> blobs;
  if (!collect_mbl_entries(port_id, blobs, err)) return false;
  if (blobs.empty()) {
    if (require_non_empty) {
      err = "No kernel entries present in $MBL bundle";
      return false;
    }
    return true;
  }
  std::string digest = compute_kernel_set_digest(blobs);
  if (!require_non_empty && digest == g_loaded_kernels_digest) {
    return true;
  }
  pepper_spice_kclear(g_spice);
  if (pepper_spice_failed(g_spice)) {
    err = capture_spice_error(g_spice);
    g_loaded_kernels_digest.clear();
    return false;
  }
  for (const auto &b : blobs) {
    if (pepper_spice_furnsh_memory(
            g_spice, b.name.c_str(), b.bytes, static_cast<int64_t>(b.size))) {
      err = capture_spice_error(g_spice);
      pepper_spice_kclear(g_spice);
      g_loaded_kernels_digest.clear();
      return false;
    }
  }
  g_loaded_kernels_digest = digest;
  return true;
}

// ============================================================================
// Time & Calendar Helpers (Leap-second aware via PEPPER + exact fallback)
// ============================================================================

bool has_leapseconds_loaded(void *ctx) {
  const double *vals = nullptr;
  int n = 0, found = 0;
  if (pepper_pool_values(ctx, "DELTET/DELTA_AT", &vals, &n, &found)) {
    pepper_reset(ctx);
    return false;
  }
  return found != 0 && n > 0;
}

// Proleptic Gregorian ISO-8601 parser -> days and seconds relative to 2000-01-01T12:00:00
bool parse_iso8601_j2000_seconds(const std::string &raw, double &sec_past_j2000) {
  std::string s = trim_copy(raw);
  if (s.empty()) return false;
  // Strip trailing scale suffix like "Z", " UTC", " TDB", " TT", " TAI", " GPS"
  while (!s.empty() && (s.back() == 'Z' || s.back() == 'z')) s.pop_back();
  size_t sp = s.find(' ');
  // Keep date-time separator if space is at index 10 ("YYYY-MM-DD HH:MM:SS")
  if (sp != std::string::npos && sp > 12) {
    s = trim_copy(s.substr(0, sp));
  }
  int year = 0, month = 0, day = 0, hour = 0, minute = 0;
  double second = 0.0;
  char sep = 'T';
  int matched = std::sscanf(
      s.c_str(), "%d-%d-%d%c%d:%d:%lf", &year, &month, &day, &sep, &hour, &minute, &second);
  if (matched < 3) return false;
  if (matched == 3) {
    hour = 0;
    minute = 0;
    second = 0.0;
  } else if (matched < 6) {
    return false;
  }
  if (month < 1 || month > 12 || day < 1 || day > 31 || hour < 0 || hour > 23 ||
      minute < 0 || minute > 59 || second < 0.0 || second >= 61.0) {
    return false;
  }
  // Howard Hinnant civil-from-days / days-from-civil relative to 1970-01-01
  int y = year - (month <= 2 ? 1 : 0);
  int era = (y >= 0 ? y : y - 399) / 400;
  unsigned yoe = static_cast<unsigned>(y - era * 400);
  unsigned doy = (153U * static_cast<unsigned>(month + (month > 2 ? -3 : 9)) + 2U) / 5U +
                 static_cast<unsigned>(day) - 1U;
  unsigned doe = yoe * 365U + yoe / 4U - yoe / 100U + doy;
  int64_t days_since_1970 = static_cast<int64_t>(era) * 146097LL + static_cast<int64_t>(doe) - 719468LL;
  int64_t days_since_2000 = days_since_1970 - 10957LL;  // 2000-01-01 is 10957 days after 1970-01-01
  sec_past_j2000 = (static_cast<double>(days_since_2000) - 0.5) * kSecPerDay +
                   static_cast<double>(hour * 3600 + minute * 60) + second;
  return true;
}

std::string format_j2000_seconds_iso8601(double sec_past_j2000, bool append_z) {
  double shifted = sec_past_j2000 + 43200.0;  // seconds since 2000-01-01T00:00:00
  double day_floor = std::floor(shifted / kSecPerDay);
  double sod = shifted - day_floor * kSecPerDay;
  // Round to microsecond precision cleanly
  double rounded_us = std::round(sod * 1e6);
  if (rounded_us >= 86400.0 * 1e6) {
    rounded_us -= 86400.0 * 1e6;
    day_floor += 1.0;
  }
  int64_t total_us = static_cast<int64_t>(rounded_us);
  int64_t total_s = total_us / 1000000LL;
  int64_t frac_us = total_us % 1000000LL;
  int hour = static_cast<int>(total_s / 3600LL);
  int minute = static_cast<int>((total_s % 3600LL) / 60LL);
  int sec = static_cast<int>(total_s % 60LL);

  int64_t z = static_cast<int64_t>(day_floor) + 10957LL + 719468LL;
  int64_t era = (z >= 0 ? z : z - 146096LL) / 146097LL;
  uint64_t doe = static_cast<uint64_t>(z - era * 146097LL);
  uint64_t yoe = (doe - doe / 1460ULL + doe / 36524ULL - doe / 146096ULL) / 365ULL;
  int64_t y = static_cast<int64_t>(yoe) + era * 400LL;
  uint64_t doy = doe - (365ULL * yoe + yoe / 4ULL - yoe / 100ULL);
  uint64_t mp = (5ULL * doy + 2ULL) / 153ULL;
  unsigned d = static_cast<unsigned>(doy - (153ULL * mp + 2ULL) / 5ULL + 1ULL);
  unsigned m = static_cast<unsigned>(mp + (mp < 10ULL ? 3ULL : -9ULL));
  y += (m <= 2U ? 1 : 0);

  char buf[64];
  std::snprintf(buf, sizeof(buf), "%04lld-%02u-%02uT%02d:%02d:%02d.%06lld%s",
                static_cast<long long>(y), m, d, hour, minute, sec,
                static_cast<long long>(frac_us), append_z ? "Z" : "");
  return std::string(buf);
}

// Periodic TDB - TT difference (Fairhead & Bretagnon / SPICE DELTET M0, M1, EB, K)
double tdb_minus_tt_from_et(void *ctx, double approx_et) {
  double delta = 0.0;
  if (ctx && has_leapseconds_loaded(ctx)) {
    // pepper_unitim works directly on loaded LSK constants!
    double tdb = 0.0;
    if (!pepper_unitim(ctx, approx_et, "TDT", "TDB", &tdb)) {
      return tdb - approx_et;
    }
    pepper_reset(ctx);
  }
  // Exact SPICE/NAIF LSK analytical formula: K * sin(E), E = M + EB * sin(M)
  constexpr double kK = 1.657e-3;
  constexpr double kEb = 1.671e-2;
  constexpr double kM0 = 6.239996;
  constexpr double kM1 = 1.99096871e-7;
  double M = kM0 + kM1 * approx_et;
  double E = M + kEb * std::sin(M);
  delta = kK * std::sin(E);
  return delta;
}

bool parse_epoch_to_et(void *ctx, const std::string &epoch_str,
                       const std::string &time_system_raw, double &et, std::string &err) {
  std::string ts = upper_copy(trim_copy(time_system_raw));
  if (ts.empty()) ts = "UTC";
  std::string clean = trim_copy(epoch_str);
  if (clean.empty()) {
    err = "Empty epoch string";
    return false;
  }

  if (ts == "TDB" || ts == "ET") {
    if (has_leapseconds_loaded(ctx)) {
      std::string q = clean;
      if (!q.empty() && (q.back() == 'Z' || q.back() == 'z')) q.pop_back();
      q += " TDB";
      if (!pepper_str2et(ctx, q.c_str(), &et)) return true;
      pepper_reset(ctx);
    }
    if (parse_iso8601_j2000_seconds(clean, et)) return true;
    err = "Invalid TDB ISO-8601 epoch: " + epoch_str;
    return false;
  }

  if (ts == "TT" || ts == "TDT") {
    double tt_sec = 0.0;
    if (!parse_iso8601_j2000_seconds(clean, tt_sec)) {
      err = "Invalid TT ISO-8601 epoch: " + epoch_str;
      return false;
    }
    if (has_leapseconds_loaded(ctx) && !pepper_unitim(ctx, tt_sec, "TDT", "TDB", &et)) {
      return true;
    }
    if (pepper_failed(ctx)) pepper_reset(ctx);
    et = tt_sec + tdb_minus_tt_from_et(ctx, tt_sec);
    return true;
  }

  if (ts == "TAI") {
    double tai_sec = 0.0;
    if (!parse_iso8601_j2000_seconds(clean, tai_sec)) {
      err = "Invalid TAI ISO-8601 epoch: " + epoch_str;
      return false;
    }
    if (has_leapseconds_loaded(ctx) && !pepper_unitim(ctx, tai_sec, "TAI", "TDB", &et)) {
      return true;
    }
    if (pepper_failed(ctx)) pepper_reset(ctx);
    double tt_sec = tai_sec + kTtMinusTai;
    et = tt_sec + tdb_minus_tt_from_et(ctx, tt_sec);
    return true;
  }

  if (ts == "GPS") {
    double gps_sec = 0.0;
    if (!parse_iso8601_j2000_seconds(clean, gps_sec)) {
      err = "Invalid GPS ISO-8601 epoch: " + epoch_str;
      return false;
    }
    double tai_sec = gps_sec + kTaiMinusGps;
    double tt_sec = tai_sec + kTtMinusTai;
    et = tt_sec + tdb_minus_tt_from_et(ctx, tt_sec);
    return true;
  }

  // Default: UTC (uses pepper_str2et when LSK is loaded)
  if (!has_leapseconds_loaded(ctx)) {
    err = "Leap-second kernel (LSK) required for UTC epoch conversion";
    return false;
  }
  if (pepper_str2et(ctx, clean.c_str(), &et)) {
    err = capture_ctx_error(ctx);
    return false;
  }
  return true;
}

std::string format_et_in_system(void *ctx, double et, const std::string &time_system_raw) {
  std::string ts = upper_copy(trim_copy(time_system_raw));
  if (ts.empty()) ts = "UTC";
  if (ts == "TDB" || ts == "ET") {
    return format_j2000_seconds_iso8601(et, false);
  }
  if (ts == "TT" || ts == "TDT") {
    double tt = 0.0;
    if (has_leapseconds_loaded(ctx) && !pepper_unitim(ctx, et, "TDB", "TDT", &tt)) {
      return format_j2000_seconds_iso8601(tt, false);
    }
    if (pepper_failed(ctx)) pepper_reset(ctx);
    return format_j2000_seconds_iso8601(et - tdb_minus_tt_from_et(ctx, et), false);
  }
  if (ts == "TAI") {
    double tai = 0.0;
    if (has_leapseconds_loaded(ctx) && !pepper_unitim(ctx, et, "TDB", "TAI", &tai)) {
      return format_j2000_seconds_iso8601(tai, false);
    }
    if (pepper_failed(ctx)) pepper_reset(ctx);
    return format_j2000_seconds_iso8601(et - tdb_minus_tt_from_et(ctx, et) - kTtMinusTai, false);
  }
  if (ts == "GPS") {
    double tai = et - tdb_minus_tt_from_et(ctx, et) - kTtMinusTai;
    if (has_leapseconds_loaded(ctx) && !pepper_unitim(ctx, et, "TDB", "TAI", &tai)) {
      // ok
    } else if (pepper_failed(ctx)) {
      pepper_reset(ctx);
    }
    return format_j2000_seconds_iso8601(tai - kTaiMinusGps, false);
  }
  if (has_leapseconds_loaded(ctx)) {
    char out[64] = {0};
    if (!pepper_timout(ctx, et, "YYYY-MM-DDTHR:MN:SC.######Z ::UTC ::RND", sizeof(out), out)) {
      return trim_copy(out);
    }
    pepper_reset(ctx);
  }
  return format_j2000_seconds_iso8601(et, true);
}

const char *timing_standard_to_str(timingStandard ts) {
  switch (ts) {
    case timingStandard_UTC:
      return "UTC";
    case timingStandard_TDB:
      return "TDB";
    case timingStandard_TT:
      return "TT";
    case timingStandard_TAI:
      return "TAI";
    case timingStandard_GPS:
      return "GPS";
    case timingStandard_A1:
      return "A1";
    default:
      return "UTC";
  }
}

// ============================================================================
// Reference Frame Resolution ($RFM / RFMCoordinateSystem -> SPICE frame name)
// ============================================================================

std::string body_fixed_frame_for_id(void *ctx, int body_id) {
  if (body_id == 399) return "IAU_EARTH";
  if (body_id == 301) return "IAU_MOON";
  if (body_id == 10) return "IAU_SUN";
  if (body_id == 499) return "IAU_MARS";
  if (body_id == 599) return "IAU_JUPITER";
  if (body_id == 699) return "IAU_SATURN";
  if (body_id == 299) return "IAU_VENUS";
  if (body_id == 199) return "IAU_MERCURY";
  const char *bname = nullptr;
  int bname_len = 0, found = 0;
  if (ctx && !pepper_body_name(ctx, body_id, &bname, &bname_len, &found) && found && bname) {
    return "IAU_" + upper_copy(std::string(bname, static_cast<size_t>(bname_len)));
  }
  if (ctx && pepper_failed(ctx)) pepper_reset(ctx);
  return "IAU_EARTH";
}

std::string resolve_rfm_coord_sys_name(void *ctx, const RFMCoordinateSystem *cs) {
  if (!cs) return "J2000";
  if (cs->KERNEL_FRAME_NAME() && cs->KERNEL_FRAME_NAME()->size() > 0) {
    return cs->KERNEL_FRAME_NAME()->str();
  }
  if (cs->KERNEL_FRAME_ID() != 0 && ctx) {
    const char *fname = nullptr;
    int flen = 0, found = 0;
    if (!pepper_frame_name(ctx, cs->KERNEL_FRAME_ID(), &fname, &flen, &found) && found && fname) {
      return std::string(fname, static_cast<size_t>(flen));
    }
    if (pepper_failed(ctx)) pepper_reset(ctx);
  }
  switch (cs->AXIS_TYPE()) {
    case rfmAxisType_MEAN_EQUATOR_EQUINOX_J2000:
    case rfmAxisType_ICRF:
      return "J2000";
    case rfmAxisType_MEAN_ECLIPTIC_EQUINOX_J2000:
      return "ECLIPJ2000";
    default:
      break;
  }
  if (cs->NAME() && cs->NAME()->size() > 0) {
    std::string n = upper_copy(trim_copy(cs->NAME()->str()));
    if (n == "ICRF" || n == "GCRF" || n == "EME2000" || n == "J2000") return "J2000";
    if (n == "ECEF" || n == "WGS84" || n == "FIXED_EARTH") return "IAU_EARTH";
    int frcode = 0;
    if (ctx && !pepper_namfrm(ctx, n.c_str(), &frcode) && frcode != 0) {
      return n;
    }
    if (ctx && pepper_failed(ctx)) pepper_reset(ctx);
  }
  if (cs->AXIS_REFERENCE_BODY_ID() != 0) {
    return body_fixed_frame_for_id(ctx, cs->AXIS_REFERENCE_BODY_ID());
  }
  if (cs->ORIGIN() && cs->ORIGIN()->CELESTIAL_BODY_ID() != 0 &&
      cs->AXIS_TYPE() != rfmAxisType_UNSPECIFIED) {
    return body_fixed_frame_for_id(ctx, cs->ORIGIN()->CELESTIAL_BODY_ID());
  }
  return "J2000";
}

std::string resolve_rfm_name(void *ctx, const RFM *rfm) {
  if (!rfm) return "J2000";
  if (rfm->NAME() && rfm->NAME()->size() > 0) {
    std::string n = upper_copy(trim_copy(rfm->NAME()->str()));
    if (n == "ICRF" || n == "GCRF" || n == "EME2000") return "J2000";
    if (n == "ECEF" || n == "WGS84" || n == "FIXED_EARTH") return "IAU_EARTH";
    return n;
  }
  if (rfm->REFERENCE_FRAME_type() == RFMUnion_CelestialFrameWrapper) {
    const auto *w = rfm->REFERENCE_FRAME_as_CelestialFrameWrapper();
    if (w) {
      switch (w->frame()) {
        case CelestialFrame_GCRF:
        case CelestialFrame_ICRF:
        case CelestialFrame_J2000:
        case CelestialFrame_J2000A:
        case CelestialFrame_EME2000:
          return "J2000";
        case CelestialFrame_B1950:
          return "B1950";
        case CelestialFrame_ITRF93:
          return "ITRF93";
        case CelestialFrame_FIXED_EARTH:
        case CelestialFrame_WGS84:
        case CelestialFrame_EFG:
        case CelestialFrame_ITRF2000:
        case CelestialFrame_ITRF97:
          return "IAU_EARTH";
        default:
          break;
      }
    }
  } else if (rfm->REFERENCE_FRAME_type() == RFMUnion_CustomFrameWrapper) {
    const auto *w = rfm->REFERENCE_FRAME_as_CustomFrameWrapper();
    if (w && w->frame() == CustomFrame_ECEF) return "IAU_EARTH";
  } else if (rfm->REFERENCE_FRAME_type() == RFMUnion_RFMCoordinateSystemWrapper) {
    const auto *w = rfm->REFERENCE_FRAME_as_RFMCoordinateSystemWrapper();
    if (w) return resolve_rfm_coord_sys_name(ctx, w->COORDINATE_SYSTEM());
  }
  return "J2000";
}

// ============================================================================
// $NCD Kernel Inspection Helper
// ============================================================================

bool build_ncd_buffer_for_blob(const KernelBlobRef &blob,
                               flatbuffers::FlatBufferBuilder &fbb,
                               std::string &err) {
  ensure_pepper_ready();
  void *tmp_spice = pepper_spice_new();
  void *tmp_ctx = pepper_context_new(tmp_spice);

  auto cleanup = [&]() {
    if (tmp_ctx) pepper_context_free(tmp_ctx);
    if (tmp_spice) pepper_spice_free(tmp_spice);
  };

  std::string sha = sha256_hex(blob.bytes, blob.size);
  std::string idword;
  if (blob.size >= 8) {
    idword.assign(reinterpret_cast<const char *>(blob.bytes), 8);
    idword = upper_copy(trim_copy(idword));
  }

  if (pepper_spice_furnsh_memory(
          tmp_spice, blob.name.c_str(), blob.bytes, static_cast<int64_t>(blob.size))) {
    err = capture_spice_error(tmp_spice);
    cleanup();
    return false;
  }

  ncdContainerFormat fmt = ncdContainerFormat_PROVIDER_DEFINED;
  std::string provider_fmt;
  std::string ifname;
  std::vector<std::string> comments;
  std::vector<flatbuffers::Offset<NCDSegmentDescriptor>> seg_offsets;
  double min_et = std::numeric_limits<double>::infinity();
  double max_et = -std::numeric_limits<double>::infinity();

  bool is_daf = (idword.rfind("DAF/", 0) == 0);
  if (is_daf) {
    if (idword == "DAF/SPK") {
      fmt = ncdContainerFormat_SPK_DAF;
    } else if (idword == "DAF/CK") {
      fmt = ncdContainerFormat_PROVIDER_DEFINED;
      provider_fmt = "NAIF_CK_DAF";
    } else if (idword == "DAF/PCK") {
      fmt = ncdContainerFormat_PROVIDER_DEFINED;
      provider_fmt = "NAIF_PCK_DAF";
    } else {
      provider_fmt = idword;
    }

    int handle = 0;
    if (!pepper_dafopr(tmp_ctx, blob.name.c_str(), &handle)) {
      int nd = 0, ni = 0, fward = 0, bward = 0, free_addr = 0;
      char ifname_buf[128] = {0};
      if (!pepper_dafrfr(tmp_ctx, handle, &nd, &ni, sizeof(ifname_buf), ifname_buf,
                         &fward, &bward, &free_addr)) {
        ifname = trim_copy(ifname_buf);
      } else {
        pepper_reset(tmp_ctx);
      }

      const char *cline_ptr = nullptr;
      int cline_n = 0;
      if (!pepper_daf_comments(tmp_ctx, handle, &cline_ptr, &cline_n) && cline_ptr && cline_n > 0) {
        const char *p = cline_ptr;
        for (int i = 0; i < cline_n; ++i) {
          size_t slen = std::strlen(p);
          comments.emplace_back(p, slen);
          p += slen + 1;
        }
      } else if (pepper_failed(tmp_ctx)) {
        pepper_reset(tmp_ctx);
      }

      if (nd > 0 && nd <= 8 && ni > 0 && ni <= 16 && !pepper_dafbfs(tmp_ctx, handle)) {
        int found = 0;
        while (!pepper_daffna(tmp_ctx, &found) && found) {
          double sum[32] = {0.0};
          double dc[8] = {0.0};
          int ic[16] = {0};
          char seg_name_buf[128] = {0};
          if (pepper_dafgs(tmp_ctx, sum) ||
              pepper_dafgn(tmp_ctx, sizeof(seg_name_buf), seg_name_buf) ||
              pepper_dafus(tmp_ctx, sum, nd, ni, dc, ic)) {
            pepper_reset(tmp_ctx);
            break;
          }
          std::string seg_name = trim_copy(seg_name_buf);
          int target_id = 0, center_id = 0, frame_id = 0, seg_type = 0;
          uint64_t init_addr = 0, final_addr = 0;
          if (idword == "DAF/SPK" && ni >= 6) {
            target_id = ic[0];
            center_id = ic[1];
            frame_id = ic[2];
            seg_type = ic[3];
            init_addr = static_cast<uint64_t>(ic[4]);
            final_addr = static_cast<uint64_t>(ic[5]);
          } else if (idword == "DAF/PCK" && ni >= 5) {
            frame_id = ic[0];
            seg_type = ic[1];
            init_addr = static_cast<uint64_t>(ic[2]);
            final_addr = static_cast<uint64_t>(ic[3]);
          } else if (idword == "DAF/CK" && ni >= 6) {
            target_id = ic[0];
            frame_id = ic[1];
            seg_type = ic[2];
            init_addr = static_cast<uint64_t>(ic[4]);
            final_addr = static_cast<uint64_t>(ic[5]);
          }
          std::string frame_name;
          if (frame_id != 0) {
            const char *fn = nullptr;
            int fn_len = 0, fn_found = 0;
            if (!pepper_frame_name(tmp_ctx, frame_id, &fn, &fn_len, &fn_found) && fn_found && fn) {
              frame_name.assign(fn, static_cast<size_t>(fn_len));
            } else if (pepper_failed(tmp_ctx)) {
              pepper_reset(tmp_ctx);
            }
          }
          double s_et = dc[0];
          double e_et = dc[1];
          if (s_et < min_et) min_et = s_et;
          if (e_et > max_et) max_et = e_et;
          std::string s_iso = format_j2000_seconds_iso8601(s_et, false);
          std::string e_iso = format_j2000_seconds_iso8601(e_et, false);
          seg_offsets.push_back(CreateNCDSegmentDescriptorDirect(
              fbb,
              seg_name.c_str(),
              target_id,
              center_id,
              frame_id,
              frame_name.empty() ? nullptr : frame_name.c_str(),
              seg_type,
              s_iso.c_str(),
              e_iso.c_str(),
              s_et,
              e_et,
              init_addr,
              final_addr,
              0));
        }
      }
      pepper_dafcls(tmp_ctx, handle);
    } else {
      pepper_reset(tmp_ctx);
    }
  } else {
    ifname = blob.name;
    if (blob.name.size() >= 5 &&
        upper_copy(blob.name.substr(blob.name.size() - 5)) == ".JSON") {
      provider_fmt = "SPICE_JSON_KERNEL";
    } else if (!idword.empty() && idword.rfind("KPL/", 0) == 0) {
      provider_fmt = idword;
    } else {
      provider_fmt = "SPICE_TEXT_KERNEL";
    }
    // Extract leading non-data lines (up to 32 lines) as comment preview
    std::string text(reinterpret_cast<const char *>(blob.bytes),
                     std::min<size_t>(blob.size, 8192));
    size_t pos = 0;
    while (pos < text.size() && comments.size() < 32) {
      size_t nl = text.find('\n', pos);
      std::string line = (nl == std::string::npos) ? text.substr(pos) : text.substr(pos, nl - pos);
      if (!line.empty() && line.back() == '\r') line.pop_back();
      if (line.find("\\begindata") != std::string::npos) break;
      if (!trim_copy(line).empty()) comments.push_back(line);
      if (nl == std::string::npos) break;
      pos = nl + 1;
    }
  }

  cleanup();

  std::vector<flatbuffers::Offset<flatbuffers::String>> comment_offs;
  comment_offs.reserve(comments.size());
  for (const auto &c : comments) {
    comment_offs.push_back(fbb.CreateString(c));
  }
  auto comments_vec = comment_offs.empty() ? 0 : fbb.CreateVector(comment_offs);
  auto segs_vec = seg_offsets.empty() ? 0 : fbb.CreateVector(seg_offsets);

  std::string start_iso = std::isfinite(min_et) ? format_j2000_seconds_iso8601(min_et, false) : "";
  std::string stop_iso = std::isfinite(max_et) ? format_j2000_seconds_iso8601(max_et, false) : "";

  auto ncd = CreateNCD(
      fbb,
      fmt,
      provider_fmt.empty() ? 0 : fbb.CreateString(provider_fmt),
      0,
      fbb.CreateString("PEPPER-WASM"),
      0,
      0,
      ifname.empty() ? 0 : fbb.CreateString(ifname),
      comments_vec,
      0,
      0,
      is_daf ? fbb.CreateString("TDB") : 0,
      segs_vec,
      0,
      0,
      0,
      start_iso.empty() ? 0 : fbb.CreateString(start_iso),
      stop_iso.empty() ? 0 : fbb.CreateString(stop_iso),
      static_cast<uint64_t>(blob.size),
      fbb.CreateString(sha),
      0);
  fbb.Finish(ncd, NCDIdentifier());
  return true;
}

}  // namespace

// ============================================================================
// Space Data Module SDK Invoke Method Exports
// ============================================================================

extern "C" int furnish_kernels(void) {
  std::string err;
  std::vector<KernelBlobRef> blobs;
  if (!collect_mbl_entries("kernels", blobs, err) || blobs.empty()) {
    return fail_method(400, err.empty() ? "No kernels provided in $MBL stream" : err);
  }
  if (!furnish_mbl_if_provided("kernels", true, err)) {
    return fail_method(400, err);
  }
  for (const auto &b : blobs) {
    flatbuffers::FlatBufferBuilder fbb(4096);
    if (!build_ncd_buffer_for_blob(b, fbb, err)) {
      return fail_method(400, err);
    }
    int rc = emit_flatbuffer("descriptors", "NCD", fbb);
    if (rc != 0) return rc;
  }
  return 0;
}

extern "C" int describe_kernel(void) {
  std::string err;
  std::vector<KernelBlobRef> blobs;
  if (!collect_mbl_entries("kernel", blobs, err) || blobs.empty()) {
    return fail_method(400, err.empty() ? "Expected kernel entry in $MBL input" : err);
  }
  flatbuffers::FlatBufferBuilder fbb(4096);
  if (!build_ncd_buffer_for_blob(blobs[0], fbb, err)) {
    return fail_method(400, err);
  }
  return emit_flatbuffer("descriptor", "NCD", fbb);
}

extern "C" int compute_ephemeris(void) {
  std::string err;
  if (!furnish_mbl_if_provided("kernels", false, err)) {
    return fail_method(400, err);
  }
  PluginInvokeSlice req_slice = plugin_get_input_stream("request", 0);
  if (!req_slice.ptr || req_slice.size < 8) {
    return fail_method(400, "Missing $OEM request payload");
  }
  flatbuffers::Verifier verifier(req_slice.ptr, req_slice.size);
  if (!OEMBufferHasIdentifier(req_slice.ptr) || !VerifyOEMBuffer(verifier)) {
    return fail_method(400, "Invalid $OEM FlatBuffer request");
  }
  const OEM *req = GetOEM(req_slice.ptr);
  if (!req || !req->EPHEMERIS_DATA_BLOCK() || req->EPHEMERIS_DATA_BLOCK()->size() == 0) {
    return fail_method(400, "$OEM request must include at least one EPHEMERIS_DATA_BLOCK");
  }

  flatbuffers::FlatBufferBuilder fbb(8192);
  std::vector<flatbuffers::Offset<ephemerisDataBlock>> out_blocks;

  for (const ephemerisDataBlock *blk : *req->EPHEMERIS_DATA_BLOCK()) {
    if (!blk) continue;
    std::string target_spec;
    if (blk->OBJECT_NAIF_ID() != 0) {
      target_spec = std::to_string(blk->OBJECT_NAIF_ID());
    } else if (blk->OBJECT() && blk->OBJECT()->OBJECT_NAME() && blk->OBJECT()->OBJECT_NAME()->size() > 0) {
      target_spec = blk->OBJECT()->OBJECT_NAME()->str();
    } else if (blk->OBJECT() && blk->OBJECT()->OBJECT_ID() && blk->OBJECT()->OBJECT_ID()->size() > 0) {
      target_spec = blk->OBJECT()->OBJECT_ID()->str();
    } else {
      return fail_method(400, "OEM block missing OBJECT_NAIF_ID and OBJECT.OBJECT_NAME");
    }

    std::string center_spec;
    if (blk->CENTER_NAIF_ID() != 0) {
      center_spec = std::to_string(blk->CENTER_NAIF_ID());
    } else if (blk->CENTER_NAME() && blk->CENTER_NAME()->size() > 0) {
      center_spec = blk->CENTER_NAME()->str();
    } else {
      center_spec = "EARTH";
    }

    std::string ref_frame = resolve_rfm_name(g_ctx, blk->REFERENCE_FRAME());
    std::string abcorr = "NONE";
    if (blk->COMMENT() && blk->COMMENT()->size() > 0) {
      std::string cmt = upper_copy(blk->COMMENT()->str());
      size_t p = cmt.find("ABCORR=");
      if (p != std::string::npos) {
        size_t s = p + 7;
        size_t e = s;
        while (e < cmt.size() && cmt[e] != ' ' && cmt[e] != ';' && cmt[e] != '\n' && cmt[e] != ',') ++e;
        if (e > s) abcorr = cmt.substr(s, e - s);
      }
    }

    std::string ts_str = timing_standard_to_str(blk->TIME_SYSTEM());
    int obj_id = blk->OBJECT_NAIF_ID();
    int ctr_id = blk->CENTER_NAIF_ID();
    int found = 0;
    if (obj_id == 0 && !pepper_bods2c(g_ctx, target_spec.c_str(), &obj_id, &found) && !found) obj_id = 0;
    if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
    if (ctr_id == 0 && !pepper_bods2c(g_ctx, center_spec.c_str(), &ctr_id, &found) && !found) ctr_id = 0;
    if (pepper_failed(g_ctx)) pepper_reset(g_ctx);

    std::string obj_name_out = target_spec;
    if (obj_id != 0) {
      const char *bn = nullptr;
      int bn_len = 0;
      if (!pepper_body_name(g_ctx, obj_id, &bn, &bn_len, &found) && found && bn) {
        obj_name_out.assign(bn, static_cast<size_t>(bn_len));
      }
      if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
    }
    std::string ctr_name_out = center_spec;
    if (ctr_id != 0) {
      const char *bn = nullptr;
      int bn_len = 0;
      if (!pepper_body_name(g_ctx, ctr_id, &bn, &bn_len, &found) && found && bn) {
        ctr_name_out.assign(bn, static_cast<size_t>(bn_len));
      }
      if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
    }

    double step = blk->STEP_SIZE();
    flatbuffers::Offset<flatbuffers::Vector<double>> compact_vec = 0;
    flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<ephemerisDataLine>>> lines_vec = 0;
    std::string start_str = blk->START_TIME() ? blk->START_TIME()->str() : "";
    std::string stop_str = blk->STOP_TIME() ? blk->STOP_TIME()->str() : "";

    if (step > 0.0) {
      double et_start = 0.0, et_stop = 0.0;
      if (!parse_epoch_to_et(g_ctx, start_str, ts_str, et_start, err)) return fail_method(400, err);
      if (stop_str.empty()) {
        et_stop = et_start;
      } else if (!parse_epoch_to_et(g_ctx, stop_str, ts_str, et_stop, err)) {
        return fail_method(400, err);
      }
      if (et_stop < et_start - 1e-9) {
        return fail_method(400, "OEM STOP_TIME precedes START_TIME");
      }
      size_t n_steps = static_cast<size_t>(std::floor((et_stop - et_start) / step + 1e-9)) + 1;
      if (n_steps > 200000) {
        return fail_method(400, "OEM step count exceeds safety limit of 200000");
      }
      std::vector<double> packed;
      packed.reserve(n_steps * 6);
      for (size_t i = 0; i < n_steps; ++i) {
        double et = et_start + static_cast<double>(i) * step;
        double st[6] = {0.0};
        double lt = 0.0;
        if (pepper_spkezr(g_ctx, target_spec.c_str(), et, ref_frame.c_str(),
                          abcorr.c_str(), center_spec.c_str(), st, &lt)) {
          return fail_method(422, capture_ctx_error(g_ctx));
        }
        for (int k = 0; k < 6; ++k) packed.push_back(st[k]);
      }
      compact_vec = fbb.CreateVector(packed);
      start_str = format_et_in_system(g_ctx, et_start, ts_str);
      stop_str = format_et_in_system(
          g_ctx, et_start + static_cast<double>(n_steps - 1) * step, ts_str);
    } else if (blk->EPHEMERIS_DATA_LINES() && blk->EPHEMERIS_DATA_LINES()->size() > 0) {
      std::vector<flatbuffers::Offset<ephemerisDataLine>> lines;
      lines.reserve(blk->EPHEMERIS_DATA_LINES()->size());
      for (const ephemerisDataLine *in_line : *blk->EPHEMERIS_DATA_LINES()) {
        if (!in_line || !in_line->EPOCH()) continue;
        std::string ep_str = in_line->EPOCH()->str();
        double et = 0.0;
        if (!parse_epoch_to_et(g_ctx, ep_str, ts_str, et, err)) return fail_method(400, err);
        double st[6] = {0.0};
        double lt = 0.0;
        if (pepper_spkezr(g_ctx, target_spec.c_str(), et, ref_frame.c_str(),
                          abcorr.c_str(), center_spec.c_str(), st, &lt)) {
          return fail_method(422, capture_ctx_error(g_ctx));
        }
        if (start_str.empty()) start_str = ep_str;
        stop_str = ep_str;
        lines.push_back(CreateephemerisDataLineDirect(
            fbb, ep_str.c_str(), st[0], st[1], st[2], st[3], st[4], st[5]));
      }
      lines_vec = fbb.CreateVector(lines);
    } else if (!start_str.empty()) {
      double et = 0.0;
      if (!parse_epoch_to_et(g_ctx, start_str, ts_str, et, err)) return fail_method(400, err);
      double st[6] = {0.0};
      double lt = 0.0;
      if (pepper_spkezr(g_ctx, target_spec.c_str(), et, ref_frame.c_str(),
                        abcorr.c_str(), center_spec.c_str(), st, &lt)) {
        return fail_method(422, capture_ctx_error(g_ctx));
      }
      std::vector<flatbuffers::Offset<ephemerisDataLine>> lines;
      lines.push_back(CreateephemerisDataLineDirect(
          fbb, start_str.c_str(), st[0], st[1], st[2], st[3], st[4], st[5]));
      lines_vec = fbb.CreateVector(lines);
      stop_str = start_str;
    } else {
      return fail_method(400, "OEM block requires START_TIME + STEP_SIZE or EPHEMERIS_DATA_LINES");
    }

    auto cat_off = CreateCATDirect(fbb, obj_name_out.c_str(),
                                   blk->OBJECT() && blk->OBJECT()->OBJECT_ID()
                                       ? blk->OBJECT()->OBJECT_ID()->c_str()
                                       : nullptr);
    auto rfm_off = CreateRFMDirect(fbb, RFMUnion_NONE, 0, 0, ref_frame.c_str());
    std::string out_comment = "ABCORR=" + abcorr;
    out_blocks.push_back(CreateephemerisDataBlock(
        fbb,
        fbb.CreateString(out_comment),
        cat_off,
        fbb.CreateString(ctr_name_out),
        rfm_off,
        0,
        0,
        blk->TIME_SYSTEM(),
        fbb.CreateString(start_str),
        0,
        0,
        fbb.CreateString(stop_str),
        fbb.CreateString("HERMITE"),
        7,
        step > 0.0 ? step : 0.0,
        6,
        compact_vec,
        lines_vec,
        0,
        0,
        obj_id,
        ctr_id));
  }

  auto blocks_vec = fbb.CreateVector(out_blocks);
  auto oem_off = CreateOEM(
      fbb,
      0,
      2.0,
      req->CREATION_DATE() ? fbb.CreateString(req->CREATION_DATE()->str()) : 0,
      fbb.CreateString("PEPPER-WASM"),
      blocks_vec);
  fbb.Finish(oem_off, OEMIdentifier());
  return emit_flatbuffer("ephemeris", "OEM", fbb);
}

extern "C" int compute_attitude(void) {
  std::string err;
  if (!furnish_mbl_if_provided("kernels", false, err)) {
    return fail_method(400, err);
  }
  PluginInvokeSlice req_slice = plugin_get_input_stream("request", 0);
  if (!req_slice.ptr || req_slice.size < 8) {
    return fail_method(400, "Missing $AEM request payload");
  }
  flatbuffers::Verifier verifier(req_slice.ptr, req_slice.size);
  if (!AEMBufferHasIdentifier(req_slice.ptr) || !VerifyAEMBuffer(verifier)) {
    return fail_method(400, "Invalid $AEM FlatBuffer request");
  }
  const AEM *req = GetAEM(req_slice.ptr);
  if (!req || !req->SEGMENTS() || req->SEGMENTS()->size() == 0) {
    return fail_method(400, "$AEM request must include at least one AEMSegment");
  }

  flatbuffers::FlatBufferBuilder fbb(8192);
  std::vector<flatbuffers::Offset<AEMSegment>> out_segs;

  for (const AEMSegment *seg : *req->SEGMENTS()) {
    if (!seg) continue;
    std::string frame_a = seg->REF_FRAME_A() ? trim_copy(seg->REF_FRAME_A()->str()) : "J2000";
    std::string frame_b = seg->REF_FRAME_B() ? trim_copy(seg->REF_FRAME_B()->str()) : "IAU_EARTH";
    std::string att_dir = seg->ATTITUDE_DIR() ? upper_copy(trim_copy(seg->ATTITUDE_DIR()->str())) : "A2B";
    std::string from_frame = (att_dir == "B2A") ? frame_b : frame_a;
    std::string to_frame = (att_dir == "B2A") ? frame_a : frame_b;
    std::string ts_str = seg->TIME_SYSTEM() ? trim_copy(seg->TIME_SYSTEM()->str()) : "UTC";
    if (ts_str.empty()) ts_str = "UTC";
    std::string angvel_frame =
        seg->ANGVEL_FRAME() ? upper_copy(trim_copy(seg->ANGVEL_FRAME()->str())) : "REF_FRAME_B";
    if (angvel_frame.empty()) angvel_frame = "REF_FRAME_B";
    uint8_t comps = seg->ATTITUDE_COMPONENTS() == 4 ? 4 : 7;

    auto eval_attitude = [&](double et, double q_ccsds[4], double av_deg_s[3]) -> bool {
      double xform[36] = {0.0};
      if (pepper_sxform(g_ctx, from_frame.c_str(), to_frame.c_str(), et, xform)) {
        err = capture_ctx_error(g_ctx);
        return false;
      }
      double rot[9] = {0.0};
      double av_from[3] = {0.0};
      pepper_xf2rav(xform, rot, av_from);
      double q_spice[4] = {0.0};
      if (pepper_m2q(g_ctx, rot, q_spice)) {
        err = capture_ctx_error(g_ctx);
        return false;
      }
      // Convert SPICE [q0, q1, q2, q3] to CCSDS 504.0-B-2 passive [Q1, Q2, Q3, QC]
      double sgn = q_spice[0] < 0.0 ? -1.0 : 1.0;
      q_ccsds[0] = -sgn * q_spice[1];
      q_ccsds[1] = -sgn * q_spice[2];
      q_ccsds[2] = -sgn * q_spice[3];
      q_ccsds[3] = sgn * q_spice[0];

      // pepper_xf2rav returns angular velocity of `to_frame` wrt `from_frame` in `from_frame`
      bool want_in_to =
          (att_dir != "B2A" && (angvel_frame == "REF_FRAME_B" || angvel_frame == upper_copy(frame_b))) ||
          (att_dir == "B2A" && (angvel_frame == "REF_FRAME_A" || angvel_frame == upper_copy(frame_a)));
      double av_out[3] = {av_from[0], av_from[1], av_from[2]};
      if (want_in_to) {
        for (int r = 0; r < 3; ++r) {
          av_out[r] = rot[r + 0] * av_from[0] + rot[r + 3] * av_from[1] + rot[r + 6] * av_from[2];
        }
      }
      av_deg_s[0] = av_out[0] * kRadToDeg;
      av_deg_s[1] = av_out[1] * kRadToDeg;
      av_deg_s[2] = av_out[2] * kRadToDeg;
      return true;
    };

    double step = seg->STEP_SIZE();
    std::string start_str = seg->START_TIME() ? seg->START_TIME()->str() : "";
    std::string stop_str = seg->STOP_TIME() ? seg->STOP_TIME()->str() : "";
    flatbuffers::Offset<flatbuffers::Vector<double>> compact_vec = 0;
    flatbuffers::Offset<flatbuffers::Vector<flatbuffers::Offset<attitudeDataLine>>> lines_vec = 0;

    if (step > 0.0) {
      double et_start = 0.0, et_stop = 0.0;
      if (!parse_epoch_to_et(g_ctx, start_str, ts_str, et_start, err)) return fail_method(400, err);
      if (stop_str.empty()) {
        et_stop = et_start;
      } else if (!parse_epoch_to_et(g_ctx, stop_str, ts_str, et_stop, err)) {
        return fail_method(400, err);
      }
      if (et_stop < et_start - 1e-9) return fail_method(400, "AEM STOP_TIME precedes START_TIME");
      size_t n_steps = static_cast<size_t>(std::floor((et_stop - et_start) / step + 1e-9)) + 1;
      if (n_steps > 200000) return fail_method(400, "AEM step count exceeds safety limit");
      std::vector<double> packed;
      packed.reserve(n_steps * comps);
      for (size_t i = 0; i < n_steps; ++i) {
        double et = et_start + static_cast<double>(i) * step;
        double q[4], av[3];
        if (!eval_attitude(et, q, av)) return fail_method(422, err);
        packed.push_back(q[0]);
        packed.push_back(q[1]);
        packed.push_back(q[2]);
        packed.push_back(q[3]);
        if (comps == 7) {
          packed.push_back(av[0]);
          packed.push_back(av[1]);
          packed.push_back(av[2]);
        }
      }
      compact_vec = fbb.CreateVector(packed);
      start_str = format_et_in_system(g_ctx, et_start, ts_str);
      stop_str = format_et_in_system(
          g_ctx, et_start + static_cast<double>(n_steps - 1) * step, ts_str);
    } else if (seg->ATTITUDE_DATA_LINES() && seg->ATTITUDE_DATA_LINES()->size() > 0) {
      std::vector<flatbuffers::Offset<attitudeDataLine>> lines;
      lines.reserve(seg->ATTITUDE_DATA_LINES()->size());
      for (const attitudeDataLine *in_l : *seg->ATTITUDE_DATA_LINES()) {
        if (!in_l || !in_l->EPOCH()) continue;
        std::string ep_str = in_l->EPOCH()->str();
        double et = 0.0;
        if (!parse_epoch_to_et(g_ctx, ep_str, ts_str, et, err)) return fail_method(400, err);
        double q[4], av[3];
        if (!eval_attitude(et, q, av)) return fail_method(422, err);
        if (start_str.empty()) start_str = ep_str;
        stop_str = ep_str;
        lines.push_back(CreateattitudeDataLineDirect(
            fbb, ep_str.c_str(), q[0], q[1], q[2], q[3], 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, av[0], av[1], av[2]));
      }
      lines_vec = fbb.CreateVector(lines);
    } else {
      return fail_method(400, "AEMSegment requires START_TIME + STEP_SIZE or ATTITUDE_DATA_LINES");
    }

    std::string att_type = comps == 7 ? "QUATERNION/ANGVEL" : "QUATERNION";
    out_segs.push_back(CreateAEMSegment(
        fbb,
        seg->OBJECT_NAME() ? fbb.CreateString(seg->OBJECT_NAME()->str()) : fbb.CreateString(to_frame),
        seg->OBJECT_ID() ? fbb.CreateString(seg->OBJECT_ID()->str()) : 0,
        fbb.CreateString(frame_a),
        fbb.CreateString(frame_b),
        fbb.CreateString(att_dir),
        fbb.CreateString(ts_str),
        fbb.CreateString(att_type),
        fbb.CreateString(start_str),
        fbb.CreateString(stop_str),
        step > 0.0 ? step : 0.0,
        comps,
        compact_vec,
        0,
        seg->CENTER_NAME() ? fbb.CreateString(seg->CENTER_NAME()->str()) : 0,
        0,
        0,
        0,
        0,
        fbb.CreateString(angvel_frame),
        fbb.CreateString("HERMITE"),
        7,
        lines_vec));
  }

  auto segs_vec = fbb.CreateVector(out_segs);
  auto aem_off = CreateAEM(
      fbb,
      fbb.CreateString("2.0"),
      req->CREATION_DATE() ? fbb.CreateString(req->CREATION_DATE()->str()) : 0,
      fbb.CreateString("PEPPER-WASM"),
      segs_vec);
  fbb.Finish(aem_off, AEMIdentifier());
  return emit_flatbuffer("attitude", "AEM", fbb);
}

namespace {

bool extract_cartesian_state_meters(void *ctx, const FRMStateVector *sv, double state_m[6],
                                    double &mu_m3_s2, frmResultStatus &status, std::string &err) {
  if (!sv) {
    status = frmResultStatus_INVALID_INPUT;
    err = "Missing SOURCE_STATE";
    return false;
  }
  mu_m3_s2 = sv->GRAVITATIONAL_PARAMETER() > 0.0 ? sv->GRAVITATIONAL_PARAMETER() : kWgs84MuMeters3S2;
  frmStateRepresentation rep = sv->REPRESENTATION();
  if (rep == frmStateRepresentation_UNSPECIFIED || rep == frmStateRepresentation_CARTESIAN) {
    if (sv->POSITION() && sv->VELOCITY()) {
      state_m[0] = sv->POSITION()->X();
      state_m[1] = sv->POSITION()->Y();
      state_m[2] = sv->POSITION()->Z();
      state_m[3] = sv->VELOCITY()->X();
      state_m[4] = sv->VELOCITY()->Y();
      state_m[5] = sv->VELOCITY()->Z();
      return true;
    }
    if (sv->ELEMENTS() && sv->ELEMENTS()->size() >= 6) {
      for (int i = 0; i < 6; ++i) state_m[i] = sv->ELEMENTS()->Get(i);
      return true;
    }
    status = frmResultStatus_INVALID_INPUT;
    err = "CARTESIAN state requires POSITION+VELOCITY or 6 ELEMENTS";
    return false;
  }
  if (rep == frmStateRepresentation_KEPLERIAN || rep == frmStateRepresentation_MODIFIED_KEPLERIAN) {
    if (!sv->ELEMENTS() || sv->ELEMENTS()->size() < 6) {
      status = frmResultStatus_INVALID_INPUT;
      err = "KEPLERIAN state requires 6 ELEMENTS";
      return false;
    }
    double e0 = sv->ELEMENTS()->Get(0);
    double e1 = sv->ELEMENTS()->Get(1);
    double inc = sv->ELEMENTS()->Get(2);
    double raan = sv->ELEMENTS()->Get(3);
    double aop = sv->ELEMENTS()->Get(4);
    double ta = sv->ELEMENTS()->Get(5);
    double rp_m = 0.0, ecc = 0.0;
    if (rep == frmStateRepresentation_KEPLERIAN) {
      ecc = e1;
      double sma_m = e0;
      if (ecc < 0.0 || (ecc < 1.0 && sma_m <= 0.0)) {
        status = frmResultStatus_INVALID_INPUT;
        err = "Invalid Keplerian SMA/ECC";
        return false;
      }
      rp_m = std::abs(sma_m) * std::abs(1.0 - ecc);
    } else {
      rp_m = e0;
      double ra_m = e1;
      if (rp_m <= 0.0 || ra_m < rp_m) {
        status = frmResultStatus_INVALID_INPUT;
        err = "Invalid Modified Keplerian RadPer/RadApo";
        return false;
      }
      ecc = (ra_m - rp_m) / (ra_m + rp_m);
    }
    // Convert true anomaly -> mean anomaly at epoch 0 for pepper_conics
    double m0 = 0.0;
    if (ecc < 1.0 - 1e-12) {
      double sin_E = std::sqrt(1.0 - ecc * ecc) * std::sin(ta) / (1.0 + ecc * std::cos(ta));
      double cos_E = (ecc + std::cos(ta)) / (1.0 + ecc * std::cos(ta));
      double E = std::atan2(sin_E, cos_E);
      m0 = E - ecc * std::sin(E);
    } else if (ecc > 1.0 + 1e-12) {
      double tanh_half_H = std::sqrt((ecc - 1.0) / (ecc + 1.0)) * std::tan(0.5 * ta);
      if (std::abs(tanh_half_H) >= 1.0) {
        status = frmResultStatus_INVALID_INPUT;
        err = "Hyperbolic true anomaly beyond asymptote";
        return false;
      }
      double H = 2.0 * std::atanh(tanh_half_H);
      m0 = ecc * std::sinh(H) - H;
    } else {
      double tan_half = std::tan(0.5 * ta);
      m0 = 0.5 * tan_half + (tan_half * tan_half * tan_half) / 6.0;
    }
    double elts[8] = {rp_m / 1000.0, ecc, inc, raan, aop, m0, 0.0, mu_m3_s2 / 1e9};
    double st_km[6] = {0.0};
    if (pepper_conics(ctx, elts, 0.0, st_km)) {
      err = capture_ctx_error(ctx);
      status = frmResultStatus_INVALID_INPUT;
      return false;
    }
    for (int i = 0; i < 6; ++i) state_m[i] = st_km[i] * 1000.0;
    return true;
  }
  status = frmResultStatus_UNSUPPORTED_STATE_REPRESENTATION;
  err = "Unsupported source state representation";
  return false;
}

bool build_frm_state_vector(void *ctx, flatbuffers::FlatBufferBuilder &fbb,
                            const double state_m[6], frmStateRepresentation rep,
                            double mu_m3_s2, const std::string &cs_name,
                            const std::string &epoch_str, const std::string &ts_str,
                            flatbuffers::Offset<FRMStateVector> &out_sv,
                            frmResultStatus &status, std::string &err) {
  if (rep == frmStateRepresentation_UNSPECIFIED) rep = frmStateRepresentation_CARTESIAN;
  std::vector<double> elems(6, 0.0);
  if (rep == frmStateRepresentation_CARTESIAN) {
    for (int i = 0; i < 6; ++i) elems[i] = state_m[i];
  } else if (rep == frmStateRepresentation_KEPLERIAN ||
             rep == frmStateRepresentation_MODIFIED_KEPLERIAN) {
    double st_km[6];
    for (int i = 0; i < 6; ++i) st_km[i] = state_m[i] / 1000.0;
    double elts[8] = {0.0};
    if (pepper_oscelt(ctx, st_km, 0.0, mu_m3_s2 / 1e9, elts)) {
      err = capture_ctx_error(ctx);
      status = frmResultStatus_INVALID_INPUT;
      return false;
    }
    double rp_m = elts[0] * 1000.0;
    double ecc = elts[1];
    double inc = elts[2];
    double raan = elts[3];
    double aop = elts[4];
    double m0 = elts[5];
    if (rep == frmStateRepresentation_KEPLERIAN &&
        (ecc < 1e-11 || inc < 1e-11 || std::abs(kPi - inc) < 1e-11 || std::abs(1.0 - ecc) < 1e-11)) {
      status = frmResultStatus_SINGULAR_ELEMENT_SET;
      err = "Singular orbit geometry for classical Keplerian elements";
      return false;
    }
    // Recover true anomaly from mean anomaly
    double ta = 0.0;
    if (ecc < 1.0 - 1e-11) {
      double E = m0;
      for (int it = 0; it < 60; ++it) {
        double d = (E - ecc * std::sin(E) - m0) / (1.0 - ecc * std::cos(E));
        E -= d;
        if (std::abs(d) < 1e-15) break;
      }
      ta = 2.0 * std::atan2(std::sqrt(1.0 + ecc) * std::sin(0.5 * E),
                            std::sqrt(1.0 - ecc) * std::cos(0.5 * E));
      if (ta < 0.0) ta += 2.0 * kPi;
      double sma_m = rp_m / (1.0 - ecc);
      if (rep == frmStateRepresentation_KEPLERIAN) {
        elems = {sma_m, ecc, inc, raan, aop, ta};
      } else {
        elems = {rp_m, sma_m * (1.0 + ecc), inc, raan, aop, ta};
      }
    } else {
      double H = m0;
      for (int it = 0; it < 60; ++it) {
        double d = (ecc * std::sinh(H) - H - m0) / (ecc * std::cosh(H) - 1.0);
        H -= d;
        if (std::abs(d) < 1e-15) break;
      }
      ta = 2.0 * std::atan(std::sqrt((ecc + 1.0) / (ecc - 1.0)) * std::tanh(0.5 * H));
      double sma_m = -rp_m / (ecc - 1.0);
      elems = {sma_m, ecc, inc, raan, aop, ta};
    }
  } else {
    status = frmResultStatus_UNSUPPORTED_STATE_REPRESENTATION;
    err = "Unsupported TARGET_REPRESENTATION";
    return false;
  }

  auto pos_off = CreateFRMVector3(fbb, state_m[0], state_m[1], state_m[2]);
  auto vel_off = CreateFRMVector3(fbb, state_m[3], state_m[4], state_m[5]);
  auto elems_off = fbb.CreateVector(elems);
  out_sv = CreateFRMStateVector(
      fbb,
      rep,
      elems_off,
      pos_off,
      vel_off,
      cs_name.empty() ? 0 : fbb.CreateString(cs_name),
      epoch_str.empty() ? 0 : fbb.CreateString(epoch_str),
      ts_str.empty() ? 0 : fbb.CreateString(ts_str),
      mu_m3_s2);
  return true;
}

}  // namespace

extern "C" int transform_frame(void) {
  std::string err;
  if (!furnish_mbl_if_provided("kernels", false, err)) {
    return fail_method(400, err);
  }
  PluginInvokeSlice req_slice = plugin_get_input_stream("request", 0);
  if (!req_slice.ptr || req_slice.size < 8) {
    return fail_method(400, "Missing $FRM request payload");
  }
  flatbuffers::Verifier verifier(req_slice.ptr, req_slice.size);
  if (!FRMBufferHasIdentifier(req_slice.ptr) || !VerifyFRMBuffer(verifier)) {
    return fail_method(400, "Invalid $FRM FlatBuffer request");
  }
  const FRM *frm_env = GetFRM(req_slice.ptr);
  const FRMFrameTransformRequest *req = frm_env ? frm_env->FRAME_TRANSFORM_REQUEST() : nullptr;
  if (!req) {
    return fail_method(400, "$FRM envelope missing FRAME_TRANSFORM_REQUEST");
  }

  flatbuffers::FlatBufferBuilder fbb(2048);
  std::string trace_id = req->TRACE_ID() ? req->TRACE_ID()->str() : "";

  auto emit_frm_error = [&](frmResultStatus st, const std::string &msg) -> int {
    auto res = CreateFRMFrameTransformResult(
        fbb, st, fbb.CreateString(msg), 0,
        trace_id.empty() ? 0 : fbb.CreateString(trace_id));
    auto env = CreateFRM(fbb, 0, res);
    fbb.Finish(env, FRMIdentifier());
    return emit_flatbuffer("result", "FRM", fbb);
  };

  frmOperationCode op = req->OPERATION();
  if (op == frmOperationCode_LLA_TO_PCPF || op == frmOperationCode_PCPF_TO_LLA) {
    if (!req->POSITION()) return emit_frm_error(frmResultStatus_INVALID_INPUT, "Missing POSITION");
    double re_m = req->EQUATORIAL_RADIUS_M() > 0.0 ? req->EQUATORIAL_RADIUS_M() : kWgs84ReMeters;
    double rp_m = req->POLAR_RADIUS_M() > 0.0 ? req->POLAR_RADIUS_M() : kWgs84RpMeters;
    if (re_m <= 0.0 || rp_m <= 0.0 || rp_m > re_m * 1.1) {
      return emit_frm_error(frmResultStatus_INVALID_INPUT, "Invalid ellipsoid radii");
    }
    double f = (re_m - rp_m) / re_m;
    if (op == frmOperationCode_LLA_TO_PCPF) {
      double lat = req->POSITION()->X();
      double lon = req->POSITION()->Y();
      double alt_m = req->POSITION()->Z();
      if (!std::isfinite(lat) || !std::isfinite(lon) || !std::isfinite(alt_m) ||
          std::abs(lat) > 0.5 * kPi + 1e-12) {
        return emit_frm_error(frmResultStatus_INVALID_INPUT, "Latitude out of [-pi/2, pi/2] radians");
      }
      double rect_km[3] = {0.0};
      if (pepper_georec(g_ctx, lon, lat, alt_m / 1000.0, re_m / 1000.0, f, rect_km)) {
        return emit_frm_error(frmResultStatus_INVALID_INPUT, capture_ctx_error(g_ctx));
      }
      auto pos_off = CreateFRMVector3(
          fbb, rect_km[0] * 1000.0, rect_km[1] * 1000.0, rect_km[2] * 1000.0);
      auto res = CreateFRMFrameTransformResult(
          fbb, frmResultStatus_OK, 0, pos_off,
          trace_id.empty() ? 0 : fbb.CreateString(trace_id));
      fbb.Finish(CreateFRM(fbb, 0, res), FRMIdentifier());
      return emit_flatbuffer("result", "FRM", fbb);
    } else {
      double rect_km[3] = {
          req->POSITION()->X() / 1000.0,
          req->POSITION()->Y() / 1000.0,
          req->POSITION()->Z() / 1000.0};
      double lon = 0.0, lat = 0.0, alt_km = 0.0;
      if (pepper_recgeo(g_ctx, rect_km, re_m / 1000.0, f, &lon, &lat, &alt_km)) {
        return emit_frm_error(frmResultStatus_INVALID_INPUT, capture_ctx_error(g_ctx));
      }
      auto pos_off = CreateFRMVector3(fbb, lat, lon, alt_km * 1000.0);
      auto res = CreateFRMFrameTransformResult(
          fbb, frmResultStatus_OK, 0, pos_off,
          trace_id.empty() ? 0 : fbb.CreateString(trace_id));
      fbb.Finish(CreateFRM(fbb, 0, res), FRMIdentifier());
      return emit_flatbuffer("result", "FRM", fbb);
    }
  }

  if (op == frmOperationCode_PCI_TO_PCPF || op == frmOperationCode_PCPF_TO_PCI) {
    if (!req->POSITION()) return emit_frm_error(frmResultStatus_INVALID_INPUT, "Missing POSITION");
    double vx = req->POSITION()->X();
    double vy = req->POSITION()->Y();
    double vz = req->POSITION()->Z();
    double m[9] = {0.0};  // row-major [m11,m12,m13, m21,m22,m23, m31,m32,m33]
    if (req->TRANSFORM_DCM()) {
      const FRMMatrix3 *d = req->TRANSFORM_DCM();
      if (op == frmOperationCode_PCI_TO_PCPF) {
        m[0] = d->M11(); m[1] = d->M12(); m[2] = d->M13();
        m[3] = d->M21(); m[4] = d->M22(); m[5] = d->M23();
        m[6] = d->M31(); m[7] = d->M32(); m[8] = d->M33();
      } else {
        // Transpose of PCI->PCPF DCM (or direct if caller passed PCPF->PCI? Wait: check!)
        // In FRM, TRANSFORM_DCM is the rotation matrix of the requested direction when supplied directly.
        m[0] = d->M11(); m[1] = d->M12(); m[2] = d->M13();
        m[3] = d->M21(); m[4] = d->M22(); m[5] = d->M23();
        m[6] = d->M31(); m[7] = d->M32(); m[8] = d->M33();
      }
    } else {
      std::string ep = req->EPOCH() ? req->EPOCH()->str() : "";
      std::string ts = req->EPOCH_TIME_SYSTEM() ? req->EPOCH_TIME_SYSTEM()->str() : "UTC";
      double et = 0.0;
      if (ep.empty() || !parse_epoch_to_et(g_ctx, ep, ts, et, err)) {
        return emit_frm_error(frmResultStatus_INVALID_INPUT,
                              err.empty() ? "EPOCH or TRANSFORM_DCM required" : err);
      }
      std::string src = req->SOURCE_COORDINATE_SYSTEM()
                            ? resolve_rfm_coord_sys_name(g_ctx, req->SOURCE_COORDINATE_SYSTEM())
                            : (op == frmOperationCode_PCI_TO_PCPF ? "J2000" : "IAU_EARTH");
      std::string dst = req->TARGET_COORDINATE_SYSTEM()
                            ? resolve_rfm_coord_sys_name(g_ctx, req->TARGET_COORDINATE_SYSTEM())
                            : (op == frmOperationCode_PCI_TO_PCPF ? "IAU_EARTH" : "J2000");
      double rot_col[9] = {0.0};
      if (pepper_pxform(g_ctx, src.c_str(), dst.c_str(), et, rot_col)) {
        return emit_frm_error(frmResultStatus_MISSING_EOP_DATA, capture_ctx_error(g_ctx));
      }
      m[0] = rot_col[0]; m[1] = rot_col[3]; m[2] = rot_col[6];
      m[3] = rot_col[1]; m[4] = rot_col[4]; m[5] = rot_col[7];
      m[6] = rot_col[2]; m[7] = rot_col[5]; m[8] = rot_col[8];
    }
    auto pos_off = CreateFRMVector3(
        fbb,
        m[0] * vx + m[1] * vy + m[2] * vz,
        m[3] * vx + m[4] * vy + m[5] * vz,
        m[6] * vx + m[7] * vy + m[8] * vz);
    auto dcm_off = CreateFRMMatrix3(
        fbb, m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8]);
    auto res = CreateFRMFrameTransformResult(
        fbb, frmResultStatus_OK, 0, pos_off,
        trace_id.empty() ? 0 : fbb.CreateString(trace_id),
        0, dcm_off);
    fbb.Finish(CreateFRM(fbb, 0, res), FRMIdentifier());
    return emit_flatbuffer("result", "FRM", fbb);
  }

  if (op == frmOperationCode_STATE_REPRESENTATION_CONVERT) {
    double state_m[6] = {0.0};
    double mu_m3_s2 = kWgs84MuMeters3S2;
    frmResultStatus st = frmResultStatus_OK;
    if (!extract_cartesian_state_meters(g_ctx, req->SOURCE_STATE(), state_m, mu_m3_s2, st, err)) {
      return emit_frm_error(st, err);
    }
    std::string cs_name =
        (req->SOURCE_STATE() && req->SOURCE_STATE()->COORDINATE_SYSTEM_NAME())
            ? req->SOURCE_STATE()->COORDINATE_SYSTEM_NAME()->str()
            : "J2000";
    std::string ep = req->EPOCH() ? req->EPOCH()->str() : "";
    std::string ts = req->EPOCH_TIME_SYSTEM() ? req->EPOCH_TIME_SYSTEM()->str() : "UTC";
    flatbuffers::Offset<FRMStateVector> target_sv = 0;
    if (!build_frm_state_vector(g_ctx, fbb, state_m, req->TARGET_REPRESENTATION(),
                                mu_m3_s2, cs_name, ep, ts, target_sv, st, err)) {
      return emit_frm_error(st, err);
    }
    auto res = CreateFRMFrameTransformResult(
        fbb, frmResultStatus_OK, 0, 0,
        trace_id.empty() ? 0 : fbb.CreateString(trace_id), target_sv);
    fbb.Finish(CreateFRM(fbb, 0, res), FRMIdentifier());
    return emit_flatbuffer("result", "FRM", fbb);
  }

  if (op == frmOperationCode_FRAME_ROTATION || op == frmOperationCode_STATE_TRANSFORM) {
    std::string ep = req->EPOCH() ? req->EPOCH()->str() : "";
    if (ep.empty() && req->SOURCE_STATE() && req->SOURCE_STATE()->EPOCH()) {
      ep = req->SOURCE_STATE()->EPOCH()->str();
    }
    std::string ts = req->EPOCH_TIME_SYSTEM() ? req->EPOCH_TIME_SYSTEM()->str() : "";
    if (ts.empty() && req->SOURCE_STATE() && req->SOURCE_STATE()->EPOCH_TIME_SYSTEM()) {
      ts = req->SOURCE_STATE()->EPOCH_TIME_SYSTEM()->str();
    }
    if (ts.empty()) ts = "UTC";
    double et = 0.0;
    if (!parse_epoch_to_et(g_ctx, ep, ts, et, err)) {
      return emit_frm_error(frmResultStatus_INVALID_INPUT, err);
    }
    std::string src_frame = resolve_rfm_coord_sys_name(g_ctx, req->SOURCE_COORDINATE_SYSTEM());
    std::string dst_frame = resolve_rfm_coord_sys_name(g_ctx, req->TARGET_COORDINATE_SYSTEM());
    double xform[36] = {0.0};
    if (pepper_sxform(g_ctx, src_frame.c_str(), dst_frame.c_str(), et, xform)) {
      return emit_frm_error(frmResultStatus_MISSING_EOP_DATA, capture_ctx_error(g_ctx));
    }
    double rot[9] = {0.0}, av[3] = {0.0};
    pepper_xf2rav(xform, rot, av);

    auto dcm_off = CreateFRMMatrix3(
        fbb, rot[0], rot[3], rot[6], rot[1], rot[4], rot[7], rot[2], rot[5], rot[8]);
    auto dcm_rate_off = CreateFRMMatrix3(
        fbb,
        xform[3 + 0 * 6], xform[3 + 1 * 6], xform[3 + 2 * 6],
        xform[4 + 0 * 6], xform[4 + 1 * 6], xform[4 + 2 * 6],
        xform[5 + 0 * 6], xform[5 + 1 * 6], xform[5 + 2 * 6]);
    auto av_off = CreateFRMVector3(fbb, av[0], av[1], av[2]);

    flatbuffers::Offset<FRMStateVector> target_sv = 0;
    flatbuffers::Offset<FRMVector3> out_pos = 0;
    if (op == frmOperationCode_STATE_TRANSFORM) {
      double in_m[6] = {0.0};
      double mu_m3_s2 = kWgs84MuMeters3S2;
      frmResultStatus st = frmResultStatus_OK;
      if (!extract_cartesian_state_meters(g_ctx, req->SOURCE_STATE(), in_m, mu_m3_s2, st, err)) {
        return emit_frm_error(st, err);
      }
      double out_m[6] = {0.0};
      for (int i = 0; i < 6; ++i) {
        double acc = 0.0;
        for (int j = 0; j < 6; ++j) acc += xform[i + 6 * j] * in_m[j];
        out_m[i] = acc;
      }
      // Handle origin translation when SOURCE and TARGET coordinate systems have distinct celestial origins
      int src_body = (req->SOURCE_COORDINATE_SYSTEM() && req->SOURCE_COORDINATE_SYSTEM()->ORIGIN())
                         ? req->SOURCE_COORDINATE_SYSTEM()->ORIGIN()->CELESTIAL_BODY_ID()
                         : 0;
      int dst_body = (req->TARGET_COORDINATE_SYSTEM() && req->TARGET_COORDINATE_SYSTEM()->ORIGIN())
                         ? req->TARGET_COORDINATE_SYSTEM()->ORIGIN()->CELESTIAL_BODY_ID()
                         : 0;
      if (src_body != 0 && dst_body != 0 && src_body != dst_body) {
        double shift_km[6] = {0.0}, lt = 0.0;
        if (pepper_spkez(g_ctx, src_body, et, dst_frame.c_str(), "NONE", dst_body, shift_km, &lt)) {
          return emit_frm_error(frmResultStatus_MISSING_EOP_DATA, capture_ctx_error(g_ctx));
        }
        for (int i = 0; i < 6; ++i) out_m[i] += shift_km[i] * 1000.0;
      }
      std::string dst_cs_name =
          (req->TARGET_COORDINATE_SYSTEM() && req->TARGET_COORDINATE_SYSTEM()->NAME())
              ? req->TARGET_COORDINATE_SYSTEM()->NAME()->str()
              : dst_frame;
      if (!build_frm_state_vector(g_ctx, fbb, out_m, req->TARGET_REPRESENTATION(),
                                  mu_m3_s2, dst_cs_name, ep, ts, target_sv, st, err)) {
        return emit_frm_error(st, err);
      }
      out_pos = CreateFRMVector3(fbb, out_m[0], out_m[1], out_m[2]);
    }

    auto res = CreateFRMFrameTransformResult(
        fbb, frmResultStatus_OK, 0, out_pos,
        trace_id.empty() ? 0 : fbb.CreateString(trace_id),
        target_sv, dcm_off, dcm_rate_off, av_off);
    fbb.Finish(CreateFRM(fbb, 0, res), FRMIdentifier());
    return emit_flatbuffer("result", "FRM", fbb);
  }

  return emit_frm_error(frmResultStatus_UNSUPPORTED_OPERATION, "Unsupported $FRM operation code");
}

// ============================================================================
// $TIM Astronomical Time Conversion
// ============================================================================

extern "C" int convert_time(void) {
  std::string err;
  if (!furnish_mbl_if_provided("kernels", false, err)) {
    return fail_method(400, err);
  }
  PluginInvokeSlice req_slice = plugin_get_input_stream("request", 0);
  if (!req_slice.ptr || req_slice.size < 8) {
    return fail_method(400, "Missing $TIM request payload");
  }
  flatbuffers::Verifier verifier(req_slice.ptr, req_slice.size);
  if (!TIMBufferHasIdentifier(req_slice.ptr) || !VerifyTIMBuffer(verifier)) {
    return fail_method(400, "Invalid $TIM FlatBuffer request");
  }
  const TIM *tim_env = GetTIM(req_slice.ptr);
  const TIMConversionRequest *req = tim_env ? tim_env->CONVERSION_REQUEST() : nullptr;
  if (!req || !req->SOURCE()) {
    return fail_method(400, "$TIM envelope missing CONVERSION_REQUEST.SOURCE");
  }

  flatbuffers::FlatBufferBuilder fbb(1024);
  std::string trace_id = req->TRACE_ID() ? req->TRACE_ID()->str() : "";
  const TIMInstant *src = req->SOURCE();

  auto emit_tim_err = [&](timConversionStatus st, const std::string &msg) -> int {
    auto res = CreateTIMConversionResult(
        fbb, 0, 0, 0.0, st, fbb.CreateString(msg),
        trace_id.empty() ? 0 : fbb.CreateString(trace_id));
    fbb.Finish(CreateTIM(fbb, src->TIME_SYSTEM(), 0, 0, res), TIMIdentifier());
    return emit_flatbuffer("result", "TIM", fbb);
  };

  auto is_supported_scale = [](timingStandard s) -> bool {
    return s == timingStandard_UTC || s == timingStandard_TAI || s == timingStandard_TT ||
           s == timingStandard_TDB || s == timingStandard_GPS || s == timingStandard_A1 ||
           (s == timingStandard_UT1);
  };
  if (!is_supported_scale(src->TIME_SYSTEM()) || !is_supported_scale(req->TARGET_TIME_SYSTEM())) {
    return emit_tim_err(timConversionStatus_UNSUPPORTED_TIME_SYSTEM,
                        "Unsupported timingStandard in $TIM request");
  }
  if ((src->TIME_SYSTEM() == timingStandard_UT1 || req->TARGET_TIME_SYSTEM() == timingStandard_UT1) &&
      !req->HAS_DUT1()) {
    return emit_tim_err(timConversionStatus_EOP_DATA_REQUIRED,
                        "UT1 conversion requires HAS_DUT1=true with DUT1_SECONDS");
  }

  // Step 1: Extract source reading in seconds relative to 2000-01-01T12:00:00 IN THE SOURCE SCALE
  double src_scale_sec = 0.0;
  timEpochRepresentation src_fmt = src->EPOCH_FORMAT();
  if (src_fmt == timEpochRepresentation_JULIAN_DATE) {
    src_scale_sec = (src->JULIAN_DATE() - kJ2000Jd) * kSecPerDay;
  } else if (src_fmt == timEpochRepresentation_MODIFIED_JULIAN_DATE) {
    src_scale_sec = (src->JULIAN_DATE() + kMjdOffset - kJ2000Jd) * kSecPerDay;
  } else if (src_fmt == timEpochRepresentation_UNIX_SECONDS) {
    src_scale_sec = src->SECONDS() - kUnixJ2000Sec;
  } else if (src_fmt == timEpochRepresentation_GPS_SECONDS) {
    src_scale_sec = src->SECONDS() - kGpsJ2000Sec;
  } else if (src_fmt == timEpochRepresentation_GNSS_WEEK_SECONDS) {
    src_scale_sec = static_cast<double>(src->GNSS_WEEK()) * 604800.0 + src->SECONDS() - kGpsJ2000Sec;
  } else if (src_fmt == timEpochRepresentation_ISO8601) {
    if (!src->ISO8601() || !parse_iso8601_j2000_seconds(src->ISO8601()->str(), src_scale_sec)) {
      return emit_tim_err(timConversionStatus_INVALID_INPUT, "Invalid source ISO8601 string");
    }
  } else {
    return emit_tim_err(timConversionStatus_INVALID_INPUT, "Unsupported source EPOCH_FORMAT");
  }
  src_scale_sec += static_cast<double>(src->SUBSECOND_NANOS()) * 1e-9;

  // Step 2: Convert source scale reading -> TAI seconds past J2000
  bool have_lsk = has_leapseconds_loaded(g_ctx);
  auto tai_minus_utc_for_utc_sec = [&](double utc_sec, double &tai_minus_utc) -> bool {
    if (req->HAS_TAI_MINUS_UTC()) {
      tai_minus_utc = req->TAI_MINUS_UTC_SECONDS();
      return true;
    }
    if (!have_lsk) return false;
    std::string iso = format_j2000_seconds_iso8601(utc_sec, false);
    double et = 0.0, tai = 0.0;
    if (pepper_str2et(g_ctx, iso.c_str(), &et) || pepper_unitim(g_ctx, et, "TDB", "TAI", &tai)) {
      pepper_reset(g_ctx);
      return false;
    }
    tai_minus_utc = tai - utc_sec;
    return true;
  };

  double tai_sec = 0.0;
  switch (src->TIME_SYSTEM()) {
    case timingStandard_TAI:
      tai_sec = src_scale_sec;
      break;
    case timingStandard_TT:
      tai_sec = src_scale_sec - kTtMinusTai;
      break;
    case timingStandard_GPS:
      tai_sec = src_scale_sec + kTaiMinusGps;
      break;
    case timingStandard_A1:
      tai_sec = src_scale_sec - kA1MinusTai;
      break;
    case timingStandard_TDB: {
      if (have_lsk && !pepper_unitim(g_ctx, src_scale_sec, "TDB", "TAI", &tai_sec)) {
        break;
      }
      if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
      double tt = src_scale_sec - tdb_minus_tt_from_et(g_ctx, src_scale_sec);
      tai_sec = tt - kTtMinusTai;
      break;
    }
    case timingStandard_UTC: {
      double d_at = 0.0;
      if (!tai_minus_utc_for_utc_sec(src_scale_sec, d_at)) {
        return emit_tim_err(timConversionStatus_LEAP_SECOND_DATA_REQUIRED,
                            "UTC source requires LSK kernel or HAS_TAI_MINUS_UTC");
      }
      tai_sec = src_scale_sec + d_at;
      break;
    }
    case timingStandard_UT1: {
      double utc_sec = src_scale_sec - req->DUT1_SECONDS();
      double d_at = 0.0;
      if (!tai_minus_utc_for_utc_sec(utc_sec, d_at)) {
        return emit_tim_err(timConversionStatus_LEAP_SECOND_DATA_REQUIRED,
                            "UT1 source requires LSK kernel or HAS_TAI_MINUS_UTC");
      }
      tai_sec = utc_sec + d_at;
      break;
    }
    default:
      break;
  }

  // Step 3: Convert TAI seconds past J2000 -> target scale reading
  double dst_scale_sec = 0.0;
  switch (req->TARGET_TIME_SYSTEM()) {
    case timingStandard_TAI:
      dst_scale_sec = tai_sec;
      break;
    case timingStandard_TT:
      dst_scale_sec = tai_sec + kTtMinusTai;
      break;
    case timingStandard_GPS:
      dst_scale_sec = tai_sec - kTaiMinusGps;
      break;
    case timingStandard_A1:
      dst_scale_sec = tai_sec + kA1MinusTai;
      break;
    case timingStandard_TDB: {
      if (have_lsk && !pepper_unitim(g_ctx, tai_sec, "TAI", "TDB", &dst_scale_sec)) {
        break;
      }
      if (pepper_failed(g_ctx)) pepper_reset(g_ctx);
      double tt = tai_sec + kTtMinusTai;
      dst_scale_sec = tt + tdb_minus_tt_from_et(g_ctx, tt);
      break;
    }
    case timingStandard_UTC:
    case timingStandard_UT1: {
      double d_at = 0.0;
      if (req->HAS_TAI_MINUS_UTC()) {
        d_at = req->TAI_MINUS_UTC_SECONDS();
      } else if (have_lsk) {
        double et = 0.0;
        char utc_buf[64] = {0};
        double parsed_utc = 0.0;
        if (!pepper_unitim(g_ctx, tai_sec, "TAI", "TDB", &et) &&
            !pepper_timout(g_ctx, et, "YYYY-MM-DDTHR:MN:SC.######### ::UTC ::RND",
                           sizeof(utc_buf), utc_buf) &&
            parse_iso8601_j2000_seconds(utc_buf, parsed_utc)) {
          d_at = tai_sec - parsed_utc;
        } else {
          pepper_reset(g_ctx);
          return emit_tim_err(timConversionStatus_LEAP_SECOND_DATA_REQUIRED,
                              "Failed to evaluate UTC leap-second offset");
        }
      } else {
        return emit_tim_err(timConversionStatus_LEAP_SECOND_DATA_REQUIRED,
                            "UTC target requires LSK kernel or HAS_TAI_MINUS_UTC");
      }
      double utc_sec = tai_sec - d_at;
      dst_scale_sec = (req->TARGET_TIME_SYSTEM() == timingStandard_UT1)
                          ? (utc_sec + req->DUT1_SECONDS())
                          : utc_sec;
      break;
    }
    default:
      break;
  }

  double delta_seconds = dst_scale_sec - src_scale_sec;
  timEpochRepresentation dst_fmt = req->TARGET_EPOCH_FORMAT();
  double out_jd = (dst_fmt == timEpochRepresentation_MODIFIED_JULIAN_DATE)
                      ? (kJ2000Jd - kMjdOffset + dst_scale_sec / kSecPerDay)
                      : (kJ2000Jd + dst_scale_sec / kSecPerDay);
  double out_sec = 0.0;
  int out_week = 0;
  if (dst_fmt == timEpochRepresentation_UNIX_SECONDS) {
    out_sec = dst_scale_sec + kUnixJ2000Sec;
  } else if (dst_fmt == timEpochRepresentation_GPS_SECONDS) {
    out_sec = dst_scale_sec + kGpsJ2000Sec;
  } else if (dst_fmt == timEpochRepresentation_GNSS_WEEK_SECONDS) {
    double gps_s = dst_scale_sec + kGpsJ2000Sec;
    out_week = static_cast<int>(std::floor(gps_s / 604800.0));
    out_sec = gps_s - static_cast<double>(out_week) * 604800.0;
  }
  std::string out_iso = format_j2000_seconds_iso8601(
      dst_scale_sec, req->TARGET_TIME_SYSTEM() == timingStandard_UTC);
  std::string src_iso = src->ISO8601()
                            ? src->ISO8601()->str()
                            : format_j2000_seconds_iso8601(
                                  src_scale_sec, src->TIME_SYSTEM() == timingStandard_UTC);

  auto src_off = CreateTIMInstant(
      fbb, src->TIME_SYSTEM(), src->EPOCH_FORMAT(), src->JULIAN_DATE(),
      src->SECONDS(), fbb.CreateString(src_iso), src->SUBSECOND_NANOS(),
      0, src->GNSS_WEEK());
  auto dst_off = CreateTIMInstant(
      fbb, req->TARGET_TIME_SYSTEM(), dst_fmt, out_jd, out_sec,
      fbb.CreateString(out_iso), 0, 0, out_week);
  auto res_off = CreateTIMConversionResult(
      fbb, src_off, dst_off, delta_seconds, timConversionStatus_OK, 0,
      trace_id.empty() ? 0 : fbb.CreateString(trace_id));
  fbb.Finish(CreateTIM(fbb, req->TARGET_TIME_SYSTEM(), dst_off, 0, res_off), TIMIdentifier());
  return emit_flatbuffer("result", "TIM", fbb);
}

// ============================================================================
// $EVL Event Locator (PEPPER Geometry Finder: GFOCLT, GFDIST, GFSEP, GFRR)
// ============================================================================

namespace {

const char *evl_abcorr_to_spice(evlAberrationCorrection c) {
  switch (c) {
    case evlAberrationCorrection_LIGHT_TIME:
      return "LT";
    case evlAberrationCorrection_LIGHT_TIME_AND_STELLAR_ABERRATION:
      return "LT+S";
    case evlAberrationCorrection_NO_CORRECTION:
    case evlAberrationCorrection_UNSPECIFIED:
    default:
      return "NONE";
  }
}

void body_shape_and_frame(void *ctx, int body_id, std::string &shape, std::string &frame) {
  const double *vals = nullptr;
  int n = 0;
  if (!pepper_body_values_by_code(ctx, body_id, "RADII", &vals, &n) && n >= 3) {
    shape = "ELLIPSOID";
    frame = body_fixed_frame_for_id(ctx, body_id);
    int frcode = 0;
    if (!pepper_namfrm(ctx, frame.c_str(), &frcode) && frcode != 0) return;
    if (pepper_failed(ctx)) pepper_reset(ctx);
  } else if (pepper_failed(ctx)) {
    pepper_reset(ctx);
  }
  shape = "POINT";
  frame = " ";
}

struct BuiltEvlEvent {
  double start_et;
  double stop_et;
  evlEventType type;
  int occulting_body_id;
  std::string observed_id;
  bool has_extremum;
  double extremum_et;
  double extremum_val;
  pceParameter extremum_param;
  int cond_index;
};

}  // namespace

extern "C" int locate_events(void) {
  std::string err;
  if (!furnish_mbl_if_provided("kernels", false, err)) {
    return fail_method(400, err);
  }
  PluginInvokeSlice req_slice = plugin_get_input_stream("request", 0);
  if (!req_slice.ptr || req_slice.size < 8) {
    return fail_method(400, "Missing $EVL request payload");
  }
  flatbuffers::Verifier verifier(req_slice.ptr, req_slice.size);
  if (!EVLBufferHasIdentifier(req_slice.ptr) || !VerifyEVLBuffer(verifier)) {
    return fail_method(400, "Invalid $EVL FlatBuffer request");
  }
  const EVL *evl_env = GetEVL(req_slice.ptr);
  const EVLEventLocationRequest *req = evl_env ? evl_env->LOCATION_REQUEST() : nullptr;
  if (!req) {
    return fail_method(400, "$EVL envelope missing LOCATION_REQUEST");
  }

  flatbuffers::FlatBufferBuilder fbb(4096);
  std::string trace_id = req->TRACE_ID() ? req->TRACE_ID()->str() : "";
  std::string target_id = req->TARGET_OBJECT_ID() ? trim_copy(req->TARGET_OBJECT_ID()->str()) : "";
  std::string ts = req->EPOCH_TIME_SYSTEM() ? trim_copy(req->EPOCH_TIME_SYSTEM()->str()) : "UTC";
  if (ts.empty()) ts = "UTC";
  std::string start_str = req->SCAN_START_EPOCH() ? req->SCAN_START_EPOCH()->str() : "";
  std::string stop_str = req->SCAN_STOP_EPOCH() ? req->SCAN_STOP_EPOCH()->str() : "";

  auto emit_evl_report = [&](evlResultStatus st, const std::string &msg,
                             const std::vector<BuiltEvlEvent> &evts,
                             uint32_t eval_count) -> int {
    std::vector<flatbuffers::Offset<EVLEvent>> offs;
    offs.reserve(evts.size());
    for (const auto &e : evts) {
      std::string s_iso = format_et_in_system(g_ctx, e.start_et, ts);
      std::string e_iso = format_et_in_system(g_ctx, e.stop_et, ts);
      std::string x_iso = e.has_extremum ? format_et_in_system(g_ctx, e.extremum_et, ts) : "";
      offs.push_back(CreateEVLEvent(
          fbb,
          e.type,
          fbb.CreateString(s_iso),
          fbb.CreateString(e_iso),
          fbb.CreateString(ts),
          std::max(0.0, e.stop_et - e.start_et),
          target_id.empty() ? 0 : fbb.CreateString(target_id),
          e.occulting_body_id,
          0,
          e.observed_id.empty() ? 0 : fbb.CreateString(e.observed_id),
          x_iso.empty() ? 0 : fbb.CreateString(x_iso),
          e.extremum_val,
          e.extremum_param,
          e.cond_index,
          0));
    }
    auto ev_vec = fbb.CreateVector(offs);
    auto rep = CreateEVLEventReport(
        fbb, st, msg.empty() ? 0 : fbb.CreateString(msg),
        req->LOCATOR_CLASS(),
        target_id.empty() ? 0 : fbb.CreateString(target_id),
        start_str.empty() ? 0 : fbb.CreateString(start_str),
        stop_str.empty() ? 0 : fbb.CreateString(stop_str),
        fbb.CreateString(ts),
        ev_vec, eval_count, 0,
        trace_id.empty() ? 0 : fbb.CreateString(trace_id));
    fbb.Finish(CreateEVL(fbb, 0, rep), EVLIdentifier());
    return emit_flatbuffer("report", "EVL", fbb);
  };

  if (target_id.empty() || start_str.empty() || stop_str.empty()) {
    return emit_evl_report(evlResultStatus_INVALID_INPUT,
                           "TARGET_OBJECT_ID, SCAN_START_EPOCH, and SCAN_STOP_EPOCH are required",
                           {}, 0);
  }
  double et0 = 0.0, et1 = 0.0;
  if (!parse_epoch_to_et(g_ctx, start_str, ts, et0, err) ||
      !parse_epoch_to_et(g_ctx, stop_str, ts, et1, err) || et1 <= et0) {
    return emit_evl_report(evlResultStatus_INVALID_INPUT,
                           err.empty() ? "Invalid scan interval" : err, {}, 0);
  }

  double step = req->SCAN_STEP_SECONDS() > 0.0
                    ? req->SCAN_STEP_SECONDS()
                    : std::clamp((et1 - et0) / 200.0, 60.0, 21600.0);
  double cnfine[2] = {et0, et1};
  std::vector<BuiltEvlEvent> events;
  uint32_t eval_est = static_cast<uint32_t>(std::ceil((et1 - et0) / step)) + 2U;

  evlLocatorClass cls = req->LOCATOR_CLASS();
  if (cls == evlLocatorClass_ECLIPSE) {
    const EVLEclipseConfiguration *cfg = req->ECLIPSE_CONFIGURATION();
    if (!cfg || !cfg->OCCULTING_BODY_IDS() || cfg->OCCULTING_BODY_IDS()->size() == 0) {
      return emit_evl_report(evlResultStatus_MISSING_CONFIGURATION,
                             "ECLIPSE requires ECLIPSE_CONFIGURATION.OCCULTING_BODY_IDS", {}, 0);
    }
    int illum_id = cfg->ILLUMINATING_BODY_ID() != 0 ? cfg->ILLUMINATING_BODY_ID() : 10;
    std::string back_name = std::to_string(illum_id);
    std::string bshape, bframe;
    body_shape_and_frame(g_ctx, illum_id, bshape, bframe);
    const char *abcorr = evl_abcorr_to_spice(cfg->ABERRATION_CORRECTION());
    double min_dur = std::max(0.0, cfg->MINIMUM_DURATION_SECONDS());

    for (int occ_id : *cfg->OCCULTING_BODY_IDS()) {
      std::string front_name = std::to_string(occ_id);
      std::string fshape, fframe;
      body_shape_and_frame(g_ctx, occ_id, fshape, fframe);

      auto run_occ = [&](const char *occtyp, evlEventType ev_type) -> bool {
        const double *res = nullptr;
        int r_rows = 0, r_cols = 0;
        if (pepper_gf_occultation(g_ctx, occtyp, front_name.c_str(), fshape.c_str(),
                                  fframe.c_str(), back_name.c_str(), bshape.c_str(),
                                  bframe.c_str(), abcorr, target_id.c_str(), step,
                                  1, 2, cnfine, &res, &r_rows, &r_cols)) {
          err = capture_ctx_error(g_ctx);
          return false;
        }
        for (int r = 0; r < r_rows; ++r) {
          double s = res[2 * r];
          double e = res[2 * r + 1];
          if (e - s >= min_dur) {
            events.push_back(BuiltEvlEvent{
                s, e, ev_type, occ_id, back_name, true, 0.5 * (s + e), 0.0,
                pceParameter_UNSPECIFIED, -1});
          }
        }
        return true;
      };

      if (bshape == "ELLIPSOID" && fshape == "ELLIPSOID") {
        if (cfg->REPORT_UMBRA() && !run_occ("FULL", evlEventType_UMBRA)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
        }
        if (cfg->REPORT_ANTUMBRA() && !run_occ("ANNULAR", evlEventType_ANTUMBRA)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
        }
        if (cfg->REPORT_PENUMBRA() && !run_occ("PARTIAL", evlEventType_PENUMBRA)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
        }
      } else {
        if (!run_occ("ANY", evlEventType_PENUMBRA)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
        }
      }
    }
  } else if (cls == evlLocatorClass_APSIDES) {
    const EVLApsidesConfiguration *cfg = req->APSIDES_CONFIGURATION();
    if (!cfg || cfg->CENTRAL_BODY_ID() == 0) {
      return emit_evl_report(evlResultStatus_MISSING_CONFIGURATION,
                             "APSIDES requires APSIDES_CONFIGURATION.CENTRAL_BODY_ID", {}, 0);
    }
    std::string center_str = std::to_string(cfg->CENTRAL_BODY_ID());
    auto run_apsis = [&](const char *relate, evlEventType ev_type) -> bool {
      const double *res = nullptr;
      int r_rows = 0, r_cols = 0;
      if (pepper_gf_distance(g_ctx, target_id.c_str(), "NONE", center_str.c_str(),
                             relate, 0.0, 0.0, step, 1, 2, cnfine, &res, &r_rows, &r_cols)) {
        err = capture_ctx_error(g_ctx);
        return false;
      }
      for (int r = 0; r < r_rows; ++r) {
        double t = res[2 * r];
        double pt[3] = {0.0}, lt = 0.0, r_km = 0.0;
        if (!pepper_spkpos(g_ctx, target_id.c_str(), t, "J2000", "NONE", center_str.c_str(), pt, &lt)) {
          pepper_vnorm(pt, &r_km);
        } else {
          pepper_reset(g_ctx);
        }
        events.push_back(BuiltEvlEvent{
            t, t, ev_type, 0, center_str, true, t, r_km * 1000.0,
            pceParameter_POSITION_MAGNITUDE, -1});
      }
      return true;
    };
    if (cfg->REPORT_PERIAPSIS() && !run_apsis("LOCMIN", evlEventType_PERIAPSIS)) {
      return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
    }
    if (cfg->REPORT_APOAPSIS() && !run_apsis("LOCMAX", evlEventType_APOAPSIS)) {
      return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, err, {}, eval_est);
    }
  } else if (cls == evlLocatorClass_PARAMETER_CONDITION) {
    const EVLParameterConditionConfiguration *cfg = req->PARAMETER_CONDITION_CONFIGURATION();
    if (!cfg || !cfg->CONDITIONS() || cfg->CONDITIONS()->size() == 0) {
      return emit_evl_report(evlResultStatus_MISSING_CONFIGURATION,
                             "PARAMETER_CONDITION requires CONDITIONS", {}, 0);
    }
    double min_dur = std::max(0.0, cfg->MINIMUM_DURATION_SECONDS());
    for (uint32_t idx = 0; idx < cfg->CONDITIONS()->size(); ++idx) {
      const PCEParameterCondition *cond = cfg->CONDITIONS()->Get(idx);
      if (!cond || !cond->PARAMETER()) continue;
      pceParameter param = cond->PARAMETER()->PARAMETER();
      std::string second_obj =
          cond->PARAMETER()->SECOND_OBJECT_ID() ? cond->PARAMETER()->SECOND_OBJECT_ID()->str() : "";
      if (second_obj.empty() && cond->PARAMETER()->CENTRAL_BODY_ID() != 0) {
        second_obj = std::to_string(cond->PARAMETER()->CENTRAL_BODY_ID());
      }
      if (second_obj.empty()) second_obj = "399";

      const char *relate = "=";
      if (cfg->REPORT_AS_INTERVAL()) {
        relate = (cond->DIRECTION() == pceConditionDirection_DECREASING) ? "<" : ">";
      }
      const double *res = nullptr;
      int r_rows = 0, r_cols = 0;
      if (param == pceParameter_RELATIVE_RANGE || param == pceParameter_POSITION_MAGNITUDE) {
        if (pepper_gf_distance(g_ctx, target_id.c_str(), "NONE", second_obj.c_str(),
                               relate, cond->GOAL_VALUE() / 1000.0, 0.0, step,
                               1, 2, cnfine, &res, &r_rows, &r_cols)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, capture_ctx_error(g_ctx), {}, eval_est);
        }
      } else if (param == pceParameter_RELATIVE_RANGE_RATE) {
        if (pepper_gf_range_rate(g_ctx, target_id.c_str(), "NONE", second_obj.c_str(),
                                 relate, cond->GOAL_VALUE() / 1000.0, 0.0, step,
                                 1, 2, cnfine, &res, &r_rows, &r_cols)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, capture_ctx_error(g_ctx), {}, eval_est);
        }
      } else if (param == pceParameter_ANGULAR_SEPARATION) {
        std::string obs_obj = cond->PARAMETER()->OWNER_OBJECT_ID()
                                  ? cond->PARAMETER()->OWNER_OBJECT_ID()->str()
                                  : "399";
        if (pepper_gf_separation(g_ctx, target_id.c_str(), "POINT", " ",
                                 second_obj.c_str(), "POINT", " ", "NONE", obs_obj.c_str(),
                                 relate, cond->GOAL_VALUE(), 0.0, step,
                                 1, 2, cnfine, &res, &r_rows, &r_cols)) {
          return emit_evl_report(evlResultStatus_MISSING_EPHEMERIS, capture_ctx_error(g_ctx), {}, eval_est);
        }
      } else {
        return emit_evl_report(evlResultStatus_PARAMETER_UNAVAILABLE,
                               "Unsupported $PCE parameter in PARAMETER_CONDITION", {}, eval_est);
      }

      for (int r = 0; r < r_rows; ++r) {
        double s = res[2 * r];
        double e = cfg->REPORT_AS_INTERVAL() ? res[2 * r + 1] : s;
        if (!cfg->REPORT_AS_INTERVAL() || e - s >= min_dur) {
          events.push_back(BuiltEvlEvent{
              s, e, evlEventType_CONDITION_SATISFIED, 0, second_obj,
              !cfg->REPORT_AS_INTERVAL(), s, cond->GOAL_VALUE(), param,
              static_cast<int>(idx)});
        }
      }
    }
  } else {
    return emit_evl_report(evlResultStatus_UNSUPPORTED_LOCATOR_CLASS,
                           "Unsupported $EVL LOCATOR_CLASS", {}, 0);
  }

  std::sort(events.begin(), events.end(), [](const BuiltEvlEvent &a, const BuiltEvlEvent &b) {
    return a.start_et < b.start_et;
  });
  evlResultStatus final_status = evlResultStatus_OK;
  if (req->MAXIMUM_EVENTS() > 0 && events.size() > req->MAXIMUM_EVENTS()) {
    events.resize(req->MAXIMUM_EVENTS());
    final_status = evlResultStatus_BUDGET_EXCEEDED;
  }
  return emit_evl_report(final_status, "", events, eval_est);
}

// ============================================================================
// Official Propagator Family ABI (`orbpro_propagator_abi.h`) + `ingest_omm`
// ============================================================================

namespace {

enum class PropMode : uint8_t {
  kUninitialized = 0,
  kAnalytical = 1,
  kEphemerisSpk = 2
};

struct AnalyticalEntity {
  double epoch_jd;
  double sma_m;
  double mean_motion_rad_s;
  double eccentricity;
  double inclination_rad;
  double raan_rad;
  double argp_rad;
  double mean_anomaly_rad;
  double mu_m3_s2;
};

struct SpkEntity {
  int target_id;
  int center_id;
  int frame_id;
  double start_et;
  double stop_et;
  std::string frame_name;
  OrbProReferenceFrame orbpro_frame;
};

PropMode g_prop_mode = PropMode::kUninitialized;
std::vector<AnalyticalEntity> g_analytical_entities;
std::vector<SpkEntity> g_spk_entities;
void *g_ephem_spice = nullptr;
void *g_ephem_ctx = nullptr;

double gmst_rad_iau1982(double jd) {
  double tut1 = (jd - kJ2000Jd) / 36525.0;
  double sec = 67310.54841 +
               (876600.0 * 3600.0 + 8640184.812866) * tut1 +
               0.093104 * tut1 * tut1 -
               6.2e-6 * tut1 * tut1 * tut1;
  double gmst = std::fmod(sec * ((2.0 * kPi) / kSecPerDay), 2.0 * kPi);
  if (gmst < 0.0) gmst += 2.0 * kPi;
  return gmst;
}

bool solve_kepler_elliptic(double M_in, double ecc, double &E_out) {
  double M = std::fmod(M_in, 2.0 * kPi);
  if (M < 0.0) M += 2.0 * kPi;
  double E = ecc < 0.8 ? M : kPi;
  for (int i = 0; i < 200; ++i) {
    double f = E - ecc * std::sin(E) - M;
    double fp = 1.0 - ecc * std::cos(E);
    if (std::abs(fp) < 1e-16) return false;
    double d = f / fp;
    E -= d;
    if (std::abs(d) < 1e-15) {
      E_out = E;
      return true;
    }
  }
  return false;
}

int validate_and_make_omm_entity(const OrbProOMMRecord &rec, AnalyticalEntity &out) {
  if (!std::isfinite(rec.epoch_jd) || !std::isfinite(rec.mean_motion) ||
      !std::isfinite(rec.eccentricity) || !std::isfinite(rec.inclination) ||
      !std::isfinite(rec.ra_of_asc_node) || !std::isfinite(rec.arg_of_pericenter) ||
      !std::isfinite(rec.mean_anomaly) || !std::isfinite(rec.bstar) ||
      !std::isfinite(rec.mean_motion_dot) || !std::isfinite(rec.mean_motion_ddot)) {
    return ORBPRO_PROP_UNPHYSICAL;
  }
  if (rec.mean_motion <= 0.0 || rec.eccentricity < 0.0 || rec.eccentricity >= 1.0 ||
      rec.inclination < 0.0 || rec.inclination > 180.0) {
    return ORBPRO_PROP_UNPHYSICAL;
  }
  double n = (rec.mean_motion * 2.0 * kPi) / kSecPerDay;
  double a = std::cbrt(kWgs72MuMeters3S2 / (n * n));
  if (!std::isfinite(a) || a <= 0.0) return ORBPRO_PROP_UNPHYSICAL;
  out.epoch_jd = rec.epoch_jd;
  out.sma_m = a;
  out.mean_motion_rad_s = n;
  out.eccentricity = rec.eccentricity;
  out.inclination_rad = rec.inclination * kDegToRad;
  out.raan_rad = rec.ra_of_asc_node * kDegToRad;
  out.argp_rad = rec.arg_of_pericenter * kDegToRad;
  out.mean_anomaly_rad = rec.mean_anomaly * kDegToRad;
  out.mu_m3_s2 = kWgs72MuMeters3S2;
  return ORBPRO_PROP_OK;
}

int propagate_analytical_one(const AnalyticalEntity &ent, double jd, OrbProStateVector *out) {
  if (!std::isfinite(jd)) return ORBPRO_PROP_BAD_INPUT;
  double dt = (jd - ent.epoch_jd) * kSecPerDay;
  double M = ent.mean_anomaly_rad + ent.mean_motion_rad_s * dt;
  double E = 0.0;
  if (!solve_kepler_elliptic(M, ent.eccentricity, E)) {
    return ORBPRO_PROP_NOT_CONVERGED;
  }
  double cosE = std::cos(E);
  double sinE = std::sin(E);
  double beta = std::sqrt(1.0 - ent.eccentricity * ent.eccentricity);
  double x_pqw = ent.sma_m * (cosE - ent.eccentricity);
  double y_pqw = ent.sma_m * beta * sinE;
  double e_dot = ent.mean_motion_rad_s / (1.0 - ent.eccentricity * cosE);
  double vx_pqw = -ent.sma_m * sinE * e_dot;
  double vy_pqw = ent.sma_m * beta * cosE * e_dot;

  double cO = std::cos(ent.raan_rad), sO = std::sin(ent.raan_rad);
  double ci = std::cos(ent.inclination_rad), si = std::sin(ent.inclination_rad);
  double cw = std::cos(ent.argp_rad), sw = std::sin(ent.argp_rad);

  double r11 = cO * cw - sO * sw * ci;
  double r12 = -cO * sw - sO * cw * ci;
  double r21 = sO * cw + cO * sw * ci;
  double r22 = -sO * sw + cO * cw * ci;
  double r31 = sw * si;
  double r32 = cw * si;

  double xI = r11 * x_pqw + r12 * y_pqw;
  double yI = r21 * x_pqw + r22 * y_pqw;
  double zI = r31 * x_pqw + r32 * y_pqw;
  double vxI = r11 * vx_pqw + r12 * vy_pqw;
  double vyI = r21 * vx_pqw + r22 * vy_pqw;
  double vzI = r31 * vx_pqw + r32 * vy_pqw;

  double theta = gmst_rad_iau1982(jd);
  double ct = std::cos(theta), st = std::sin(theta);
  double x = ct * xI + st * yI;
  double y = -st * xI + ct * yI;
  double z = zI;
  double vx_rot = ct * vxI + st * vyI;
  double vy_rot = -st * vxI + ct * vyI;

  orbpro_state_init(out);
  out->epoch = jd;
  out->position[0] = x;
  out->position[1] = y;
  out->position[2] = z;
  out->velocity[0] = vx_rot + kEarthOmegaRadS * y;
  out->velocity[1] = vy_rot - kEarthOmegaRadS * x;
  out->velocity[2] = vzI;
  orbpro_state_set_reference_frame(out, ORBPRO_FRAME_ECEF);
  out->flags = ORBPRO_STATE_VALID;
  return ORBPRO_PROP_OK;
}

}  // namespace

extern "C" {

int32_t plugin_init(const char *tle_lines, int32_t count) {
  (void)tle_lines;
  if (count < 0) return ORBPRO_PROP_BAD_INPUT;
  return ORBPRO_PROP_UNSUPPORTED_FORMAT;
}

int32_t plugin_init_omm(const OrbProOMMRecord *records, int32_t count) {
  if (count < 0 || (count > 0 && !records)) return ORBPRO_PROP_BAD_INPUT;
  for (int32_t i = 0; i < count; ++i) {
    AnalyticalEntity probe{};
    int rc = validate_and_make_omm_entity(records[i], probe);
    if (rc != ORBPRO_PROP_OK) return rc;
  }
  if (g_analytical_entities.capacity() < static_cast<size_t>(std::max<int32_t>(count, 512))) {
    g_analytical_entities.reserve(static_cast<size_t>(std::max<int32_t>(count, 512)));
  }
  g_analytical_entities.resize(static_cast<size_t>(count));
  for (int32_t i = 0; i < count; ++i) {
    validate_and_make_omm_entity(records[i], g_analytical_entities[static_cast<size_t>(i)]);
  }
  g_spk_entities.clear();
  g_prop_mode = count > 0 ? PropMode::kAnalytical : PropMode::kUninitialized;
  return count;
}

int32_t plugin_ingest_omm_one(const OrbProOMMRecord *record) {
  if (!record) return ORBPRO_PROP_BAD_INPUT;
  AnalyticalEntity ent{};
  int rc = validate_and_make_omm_entity(*record, ent);
  if (rc != ORBPRO_PROP_OK) return rc;
  if (g_prop_mode == PropMode::kEphemerisSpk) {
    g_spk_entities.clear();
  }
  if (g_analytical_entities.capacity() < 512) {
    g_analytical_entities.reserve(512);
  }
  g_analytical_entities.push_back(ent);
  g_prop_mode = PropMode::kAnalytical;
  return static_cast<int32_t>(g_analytical_entities.size() - 1);
}

int32_t plugin_init_elements(const OrbProOrbitalElements *elements, int32_t count) {
  if (count < 0 || (count > 0 && !elements)) return ORBPRO_PROP_BAD_INPUT;
  if (g_analytical_entities.capacity() < static_cast<size_t>(std::max<int32_t>(count, 512))) {
    g_analytical_entities.reserve(static_cast<size_t>(std::max<int32_t>(count, 512)));
  }
  std::vector<AnalyticalEntity> staged;
  staged.reserve(static_cast<size_t>(count));
  for (int32_t i = 0; i < count; ++i) {
    const auto &el = elements[i];
    if (!std::isfinite(el.epoch) || !std::isfinite(el.semi_major_axis) ||
        !std::isfinite(el.eccentricity) || !std::isfinite(el.inclination) ||
        !std::isfinite(el.raan) || !std::isfinite(el.arg_periapsis) ||
        !std::isfinite(el.true_anomaly)) {
      return ORBPRO_PROP_UNPHYSICAL;
    }
    if (el.semi_major_axis <= 0.0 || el.eccentricity < 0.0 || el.eccentricity >= 1.0 ||
        el.inclination < 0.0 || el.inclination > kPi) {
      return ORBPRO_PROP_UNPHYSICAL;
    }
    double mu_m3_s2 = kWgs72MuMeters3S2;
    double sma_m = el.semi_major_axis * 1000.0;
    double n = std::sqrt(mu_m3_s2 / (sma_m * sma_m * sma_m));
    double ta = el.true_anomaly;
    double ecc = el.eccentricity;
    double sinE = std::sqrt(1.0 - ecc * ecc) * std::sin(ta) / (1.0 + ecc * std::cos(ta));
    double cosE = (ecc + std::cos(ta)) / (1.0 + ecc * std::cos(ta));
    double E = std::atan2(sinE, cosE);
    double M = E - ecc * std::sin(E);
    if (M < 0.0) M += 2.0 * kPi;

    AnalyticalEntity ent{};
    ent.epoch_jd = el.epoch;
    ent.sma_m = sma_m;
    ent.mean_motion_rad_s = n;
    ent.eccentricity = ecc;
    ent.inclination_rad = el.inclination;
    ent.raan_rad = el.raan;
    ent.argp_rad = el.arg_periapsis;
    ent.mean_anomaly_rad = M;
    ent.mu_m3_s2 = mu_m3_s2;
    staged.push_back(ent);
  }
  g_analytical_entities.assign(staged.begin(), staged.end());
  g_spk_entities.clear();
  g_prop_mode = count > 0 ? PropMode::kAnalytical : PropMode::kUninitialized;
  return count;
}

int32_t plugin_init_ephemeris(const uint8_t *bytes, uint32_t len, uint32_t format) {
  if (!bytes || len < 8) return ORBPRO_PROP_BAD_INPUT;
  std::string magic(reinterpret_cast<const char *>(bytes), 7);
  magic = upper_copy(magic);
  if (format != ORBPRO_EPHEM_FORMAT_SPK_DAF &&
      !(format == ORBPRO_EPHEM_FORMAT_AUTO && magic == "DAF/SPK")) {
    return ORBPRO_PROP_UNSUPPORTED_FORMAT;
  }
  if (magic != "DAF/SPK") {
    return ORBPRO_PROP_BAD_INPUT;
  }

  ensure_pepper_ready();
  if (!g_ephem_spice) g_ephem_spice = pepper_spice_new();
  if (!g_ephem_ctx) g_ephem_ctx = pepper_context_new(g_ephem_spice);
  pepper_spice_kclear(g_ephem_spice);
  if (pepper_spice_failed(g_ephem_spice)) pepper_spice_reset(g_ephem_spice);
  if (pepper_failed(g_ephem_ctx)) pepper_reset(g_ephem_ctx);

  if (pepper_spice_furnsh_memory(g_ephem_spice, "container.bsp", bytes, static_cast<int64_t>(len))) {
    pepper_spice_reset(g_ephem_spice);
    return ORBPRO_PROP_BAD_INPUT;
  }

  const int *ids = nullptr;
  int ids_n = 0;
  if (pepper_spk_objects(g_ephem_ctx, "container.bsp", &ids, &ids_n) || ids_n <= 0) {
    pepper_reset(g_ephem_ctx);
    pepper_spice_kclear(g_ephem_spice);
    return ORBPRO_PROP_BAD_INPUT;
  }
  std::vector<int> body_ids(ids, ids + ids_n);

  // Inspect DAF segments to pair each target body with its center & native frame
  g_spk_entities.clear();
  int handle = 0;
  if (!pepper_dafopr(g_ephem_ctx, "container.bsp", &handle)) {
    for (int body_id : body_ids) {
      const double *iv = nullptr;
      int rows = 0, cols = 0;
      double s_et = -1e20, e_et = 1e20;
      if (!pepper_spk_coverage(g_ephem_ctx, "container.bsp", body_id, &iv, &rows, &cols) && rows > 0) {
        s_et = iv[0];
        e_et = iv[2 * (rows - 1) + 1];
      } else if (pepper_failed(g_ephem_ctx)) {
        pepper_reset(g_ephem_ctx);
      }

      int center_id = 0, frame_id = 1;
      if (!pepper_dafbfs(g_ephem_ctx, handle)) {
        int found = 0;
        while (!pepper_daffna(g_ephem_ctx, &found) && found) {
          double sum[8] = {0.0}, dc[2] = {0.0};
          int ic[6] = {0};
          if (!pepper_dafgs(g_ephem_ctx, sum) && !pepper_dafus(g_ephem_ctx, sum, 2, 6, dc, ic)) {
            if (ic[0] == body_id) {
              center_id = ic[1];
              frame_id = ic[2];
              break;
            }
          }
        }
      }
      std::string fname = "J2000";
      const char *fn = nullptr;
      int fn_len = 0, fn_found = 0;
      if (!pepper_frame_name(g_ephem_ctx, frame_id, &fn, &fn_len, &fn_found) && fn_found && fn) {
        fname.assign(fn, static_cast<size_t>(fn_len));
      } else if (pepper_failed(g_ephem_ctx)) {
        pepper_reset(g_ephem_ctx);
      }
      OrbProReferenceFrame orb_frame = (fname == "IAU_EARTH" || fname == "ITRF93")
                                           ? ORBPRO_FRAME_ECEF
                                           : ORBPRO_FRAME_J2000;
      g_spk_entities.push_back(SpkEntity{body_id, center_id, frame_id, s_et, e_et, fname, orb_frame});
    }
    pepper_dafcls(g_ephem_ctx, handle);
  }
  g_analytical_entities.clear();
  g_prop_mode = g_spk_entities.empty() ? PropMode::kUninitialized : PropMode::kEphemerisSpk;
  return static_cast<int32_t>(g_spk_entities.size());
}

int32_t plugin_entity_count(void) {
  if (g_prop_mode == PropMode::kAnalytical) {
    return static_cast<int32_t>(g_analytical_entities.size());
  }
  if (g_prop_mode == PropMode::kEphemerisSpk) {
    return static_cast<int32_t>(g_spk_entities.size());
  }
  return 0;
}

int32_t plugin_propagate(double julian_date, int32_t entity_index, OrbProStateVector *out) {
  if (!out) return ORBPRO_PROP_NULL_OUTPUT;
  if (g_prop_mode == PropMode::kUninitialized) return ORBPRO_PROP_NOT_INITIALIZED;
  if (!std::isfinite(julian_date)) return ORBPRO_PROP_BAD_INPUT;
  if (entity_index < 0) return ORBPRO_PROP_BAD_ENTITY_INDEX;
  size_t idx = static_cast<size_t>(entity_index);

  if (g_prop_mode == PropMode::kAnalytical) {
    if (idx >= g_analytical_entities.size()) return ORBPRO_PROP_BAD_ENTITY_INDEX;
    return propagate_analytical_one(g_analytical_entities[idx], julian_date, out);
  }

  if (g_prop_mode == PropMode::kEphemerisSpk) {
    if (idx >= g_spk_entities.size()) return ORBPRO_PROP_BAD_ENTITY_INDEX;
    const SpkEntity &ent = g_spk_entities[idx];
    double et = (julian_date - kJ2000Jd) * kSecPerDay;
    if (et < ent.start_et || et > ent.stop_et) {
      return ORBPRO_PROP_EPOCH_OUT_OF_RANGE;
    }
    double st_km[6] = {0.0}, lt = 0.0;
    if (pepper_spkez(g_ephem_ctx, ent.target_id, et, ent.frame_name.c_str(),
                     "NONE", ent.center_id, st_km, &lt)) {
      pepper_reset(g_ephem_ctx);
      return ORBPRO_PROP_EPOCH_OUT_OF_RANGE;
    }
    orbpro_state_init(out);
    out->epoch = julian_date;
    out->position[0] = st_km[0] * 1000.0;
    out->position[1] = st_km[1] * 1000.0;
    out->position[2] = st_km[2] * 1000.0;
    out->velocity[0] = st_km[3] * 1000.0;
    out->velocity[1] = st_km[4] * 1000.0;
    out->velocity[2] = st_km[5] * 1000.0;
    orbpro_state_set_reference_frame(out, ent.orbpro_frame);
    out->flags = ORBPRO_STATE_VALID;
    return ORBPRO_PROP_OK;
  }

  return ORBPRO_PROP_NOT_INITIALIZED;
}

int32_t plugin_propagate_batch(double julian_date, OrbProStateVector *out, int32_t count) {
  if (g_prop_mode == PropMode::kUninitialized) return ORBPRO_PROP_NOT_INITIALIZED;
  if (count < 0 || !std::isfinite(julian_date)) return ORBPRO_PROP_BAD_INPUT;
  if (count > 0 && !out) return ORBPRO_PROP_NULL_OUTPUT;
  int32_t n_ent = plugin_entity_count();
  if (n_ent <= 0) return ORBPRO_PROP_NOT_INITIALIZED;
  for (int32_t e = 0; e < count; ++e) {
    int32_t rc = plugin_propagate(julian_date, e, &out[static_cast<size_t>(e)]);
    if (rc != ORBPRO_PROP_OK) return rc;
  }
  return ORBPRO_PROP_OK;
}

void plugin_destroy(void) {
  g_analytical_entities.clear();  // preserves reserved capacity for zero-growth reuse
  g_spk_entities.clear();
  if (g_ephem_spice) {
    pepper_spice_kclear(g_ephem_spice);
    if (pepper_spice_failed(g_ephem_spice)) pepper_spice_reset(g_ephem_spice);
  }
  g_prop_mode = PropMode::kUninitialized;
}

int ingest_omm(void) {
  uint32_t n = plugin_input_stream_count("omm");
  if (n == 0) return fail_method(400, "Expected at least one $OMM input frame");
  for (uint32_t i = 0; i < n; ++i) {
    PluginInvokeSlice s = plugin_get_input_stream("omm", i);
    if (!s.ptr || s.size < 8) return fail_method(400, "Truncated $OMM frame");
    flatbuffers::Verifier v(s.ptr, s.size);
    if (!OMMBufferHasIdentifier(s.ptr) || !VerifyOMMBuffer(v)) {
      return fail_method(400, "Invalid $OMM FlatBuffer frame");
    }
    const OMM *omm = GetOMM(s.ptr);
    if (!omm || !omm->EPOCH()) return fail_method(400, "$OMM missing EPOCH");
    double sec_j2000 = 0.0;
    if (!parse_iso8601_j2000_seconds(omm->EPOCH()->str(), sec_j2000)) {
      return fail_method(400, "Invalid $OMM EPOCH ISO-8601 string");
    }
    OrbProOMMRecord rec{};
    rec.epoch_jd = kJ2000Jd + sec_j2000 / kSecPerDay;
    rec.mean_motion = omm->MEAN_MOTION();
    rec.eccentricity = omm->ECCENTRICITY();
    rec.inclination = omm->INCLINATION();
    rec.ra_of_asc_node = omm->RA_OF_ASC_NODE();
    rec.arg_of_pericenter = omm->ARG_OF_PERICENTER();
    rec.mean_anomaly = omm->MEAN_ANOMALY();
    rec.bstar = omm->BSTAR();
    rec.mean_motion_dot = omm->MEAN_MOTION_DOT();
    rec.mean_motion_ddot = omm->MEAN_MOTION_DDOT();
    rec.norad_cat_id = omm->NORAD_CAT_ID();
    std::memset(rec._reserved, 0, sizeof(rec._reserved));
    int32_t rc = plugin_ingest_omm_one(&rec);
    if (rc < 0) return fail_method(422, "Unphysical or invalid $OMM orbital elements");
  }
  return 0;
}

}  // extern "C"
