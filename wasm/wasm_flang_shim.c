#include <stdint.h>

/* Fail-closed stubs for filesystem WASI preview1 calls not exposed by
 * space-data-module-sdk's browser WASI shim (which exposes no filesystem).
 * Returning WASI ERRNO_BADF (8) matches fd_prestat_get / path_open in wasiShim.js. */
int32_t __imported_wasi_snapshot_preview1_fd_filestat_get(int32_t fd, int32_t buf) {
  if (fd >= 0 && fd <= 2 && buf != 0) {
    uint8_t *p = (uint8_t *)(uintptr_t)buf;
    for (int i = 0; i < 64; ++i) p[i] = 0;
    p[16] = 2; /* FILETYPE_CHARACTER_DEVICE */
    return 0;
  }
  return 8; /* ERRNO_BADF */
}

int32_t __imported_wasi_snapshot_preview1_fd_filestat_set_size(int32_t fd, int64_t size) {
  (void)fd;
  (void)size;
  return 8; /* ERRNO_BADF */
}

int32_t __imported_wasi_snapshot_preview1_fd_filestat_set_times(
    int32_t fd, int64_t atim, int64_t mtim, int32_t fst_flags) {
  (void)fd;
  (void)atim;
  (void)mtim;
  (void)fst_flags;
  return 8; /* ERRNO_BADF */
}

int32_t __imported_wasi_snapshot_preview1_path_filestat_get(
    int32_t fd, int32_t flags, int32_t path, int32_t path_len, int32_t buf) {
  (void)fd;
  (void)flags;
  (void)path;
  (void)path_len;
  (void)buf;
  return 8; /* ERRNO_BADF */
}

int32_t __imported_wasi_snapshot_preview1_path_filestat_set_times(
    int32_t fd, int32_t flags, int32_t path, int32_t path_len,
    int64_t atim, int64_t mtim, int32_t fst_flags) {
  (void)fd;
  (void)flags;
  (void)path;
  (void)path_len;
  (void)atim;
  (void)mtim;
  (void)fst_flags;
  return 8; /* ERRNO_BADF */
}

int32_t __imported_wasi_snapshot_preview1_path_unlink_file(
    int32_t fd, int32_t path, int32_t path_len) {
  (void)fd;
  (void)path;
  (void)path_len;
  return 8; /* ERRNO_BADF */
}
