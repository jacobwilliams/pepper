/*
 * A test of PEPPER's C interface (include/pepper.h), without Python: run by
 * `meson test` (pixi run -e python test-capi).
 *
 * Instances and contexts, strings in and out, errors, results of any size,
 * a programmatic body from a C function, a prepared query.
 */
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "pepper.h"

static int failures = 0;

#define CHECK(cond, ...)                                    \
    do {                                                    \
        if (!(cond)) {                                      \
            printf("FAIL %s:%d: ", __FILE__, __LINE__);     \
            printf(__VA_ARGS__);                            \
            printf("\n");                                   \
            failures++;                                     \
        }                                                   \
    } while (0)

/* A leapseconds kernel (naif0012's values), as lines for lmpool. */
static const char *lsk[] = {
    "DELTET/DELTA_T_A = 32.184",
    "DELTET/K = 1.657D-3",
    "DELTET/EB = 1.671D-2",
    "DELTET/M = ( 6.239996D0 1.99096871D-7 )",
    "DELTET/DELTA_AT = ( 10, @1972-JAN-1 11, @1972-JUL-1 12, @1973-JAN-1",
    "13, @1974-JAN-1 14, @1975-JAN-1 15, @1976-JAN-1 16, @1977-JAN-1",
    "17, @1978-JAN-1 18, @1979-JAN-1 19, @1980-JAN-1 20, @1981-JUL-1",
    "21, @1982-JUL-1 22, @1983-JUL-1 23, @1985-JUL-1 24, @1988-JAN-1",
    "25, @1990-JAN-1 26, @1991-JAN-1 27, @1992-JUL-1 28, @1993-JUL-1",
    "29, @1994-JUL-1 30, @1996-JAN-1 31, @1997-JUL-1 32, @1999-JAN-1",
    "33, @2006-JAN-1 34, @2009-JAN-1 35, @2012-JUL-1 36, @2015-JUL-1",
    "37, @2017-JAN-1 )",
};

/* A body in linear motion relative to the Earth, in J2000. */
static int linear(void *ctx, void *user, int body, double et, double *state) {
    const double *v = (const double *)user;
    (void)ctx;
    (void)body;
    for (int i = 0; i < 3; i++) {
        state[i] = 7000.0 * (i == 0) + v[i] * et;
        state[i + 3] = v[i];
    }
    return 1;
}

/* A C kernel loader (see the test below). */
static int c_identity(void *ctx, void *user, int frame, double et, double *xform, int *relative) {
    (void)ctx; (void)user; (void)frame; (void)et;
    for (int i = 0; i < 36; i++) xform[i] = (i % 7 == 0) ? 1.0 : 0.0;
    *relative = 1;
    return 1;
}
static int c_load(void *ctx, void *user, void *kb, const char *file) {
    enum { MAXN = 200 };
    static double ets[MAXN], sts[MAXN][6], q[MAXN][4];
    static double offset[3] = {100.0, 200.0, 300.0};
    int body, n = 0;
    FILE *f = fopen(file, "r");
    if (!f || fscanf(f, "CEPH %d", &body) != 1) {
        if (f) fclose(f);
        return 1;
    }
    while (n < MAXN && fscanf(f, "%lf %lf %lf %lf %lf %lf %lf", &ets[n], &sts[n][0], &sts[n][1], &sts[n][2],
                              &sts[n][3], &sts[n][4], &sts[n][5]) == 7) {
        double a = 1e-3 * ets[n];
        q[n][0] = cos(a / 2); q[n][1] = 0; q[n][2] = 0; q[n][3] = -sin(a / 2);   /* SPICE's for ROTATE(a, 3) */
        n++;
    }
    fclose(f);
    if (pepper_builder_spk(ctx, kb, 13, body, 399, "J2000", n, ets, &sts[0][0], 7, 0, 0, 0, "CEPH", 0)) return 1;
    if (pepper_builder_add_ephemeris(ctx, kb, body + 1, 399, "J2000", -1e10, 1e10, linear, offset, "")) return 1;
    if (pepper_builder_frame_rotations(ctx, kb, "CEPH_ROT", 1400800, 399, "J2000", n, ets, &q[0][0], NULL, 7, NULL,
                                       "", 0, 0, 0, 0, "")) return 1;
    if (pepper_builder_add_frame(ctx, kb, "CEPH_MODEL", 1400801, 399, c_identity, NULL)) return 1;
    (*(int *)user)++;
    return 0;
}
static int c_detect(void *user, const char *file, const char *head, int nhead) {
    (void)user; (void)file;
    return nhead >= 4 && strncmp(head, "CEPH", 4) == 0;
}
static void c_unload(void *user, const char *file) {
    (void)file;
    (*(int *)user)--;
}

int main(void) {
    char msg[1024];
    pepper_runtime_init();
    void *spice = pepper_spice_new();
    void *ctx = pepper_context_new(spice);

    /* the kernel pool from lines (fixed-width block) */
    enum { W = 80 };
    char block[sizeof(lsk) / sizeof(lsk[0])][W];
    int n = (int)(sizeof(lsk) / sizeof(lsk[0]));
    memset(block, 0, sizeof(block));
    for (int i = 0; i < n; i++) strncpy(block[i], lsk[i], W - 1);
    CHECK(pepper_spice_lmpool(spice, n, W, &block[0][0]) == 0, "lmpool");

    /* time conversion both ways */
    double et = 0.0;
    CHECK(pepper_str2et(ctx, "2026 NOV 8 12:00:00", &et) == 0, "str2et");
    char utc[64];
    CHECK(pepper_et2utc(ctx, et, "ISOC", 3, sizeof utc, utc) == 0, "et2utc");
    CHECK(strcmp(utc, "2026-11-08T12:00:00.000") == 0, "et2utc gave '%s'", utc);

    /* a result of any size: the values of a pool variable */
    const double *values = NULL;
    int nvalues = 0, found = 0;
    CHECK(pepper_pool_values(ctx, "DELTET/M", &values, &nvalues, &found) == 0, "pool_values");
    CHECK(found && nvalues == 2 && fabs(values[1] - 1.99096871e-7) < 1e-20, "pool_values: %d %d", found,
          nvalues);

    /* a JSON kernel (a file named *.json), loaded and unloaded */
    {
        const char *json = "test_capi_kernel.json";
        FILE *f = fopen(json, "w");
        fputs("// a JSON kernel\n{ \"CAPI_JSON\": [1.5, \"@2000-01-02T12:00\"], \"+CAPI_JSON\": 3, }\n", f);
        fclose(f);
        CHECK(pepper_spice_furnsh(spice, json, -1) == 0, "furnsh of a JSON kernel");
        CHECK(pepper_pool_values(ctx, "CAPI_JSON", &values, &nvalues, &found) == 0, "pool_values (JSON)");
        CHECK(found && nvalues == 3 && values[0] == 1.5 && values[1] == 86400.0 && values[2] == 3.0,
              "pool_values (JSON): %d %d", found, nvalues);
        CHECK(pepper_spice_unload(spice, json) == 0, "unload of a JSON kernel");
        CHECK(pepper_pool_values(ctx, "CAPI_JSON", &values, &nvalues, &found) == 0 && !found,
              "the JSON kernel's variable is gone");
        remove(json);
    }

    /* an error: a missing file, reported in the instance */
    CHECK(pepper_spice_furnsh(spice, "no-such-kernel.bsp", -1) == 1, "furnsh of a missing file");
    pepper_spice_getmsg(spice, "SHORT", sizeof msg, msg);
    CHECK(strcmp(msg, "SPICE(NOSUCHFILE)") == 0, "short message '%s'", msg);
    pepper_spice_reset(spice);
    CHECK(pepper_spice_failed(spice) == 0, "reset");

    /* an error in a context: unknown body */
    double state[6], lt;
    CHECK(pepper_spkezr(ctx, "NOBODY", et, "J2000", "NONE", "EARTH", state, &lt) == 1, "spkezr of an unknown body");
    pepper_getmsg(ctx, "SHORT", sizeof msg, msg);
    CHECK(strcmp(msg, "SPICE(IDCODENOTFOUND)") == 0, "short message '%s'", msg);
    pepper_reset(ctx);

    /* a programmatic body from a C function */
    double v[3] = {1.0, 2.0, 3.0};
    CHECK(pepper_spice_add_ephemeris(spice, -999, 399, "J2000", -1e10, 1e10, linear, v) == 0, "add_ephemeris");
    CHECK(pepper_spkezr(ctx, "-999", 100.0, "J2000", "NONE", "EARTH", state, &lt) == 0, "spkezr");
    CHECK(state[0] == 7100.0 && state[1] == 200.0 && state[2] == 300.0 && state[4] == 2.0, "state %g %g %g",
          state[0], state[1], state[2]);

    /* a second context: opens the files again on units of its own */
    void *ctx2 = pepper_context_new(spice);
    double state2[6];
    CHECK(pepper_spkezr(ctx2, "-999", 100.0, "J2000", "NONE", "EARTH", state2, &lt) == 0, "spkezr, context 2");
    CHECK(state2[0] == state[0], "context 2: %g", state2[0]);
    pepper_context_free(ctx2);

    /* a prepared query, two epochs */
    void *q = NULL;
    CHECK(pepper_prepare_state(ctx, "-999", "EARTH", "J2000", "NONE", &q) == 0 && q != NULL, "prepare_state");
    double ets[2] = {0.0, 10.0}, states[2][6], lts[2];
    CHECK(pepper_query_state(ctx, q, 2, ets, &states[0][0], lts) == 0, "query_state");
    CHECK(states[1][0] == 7010.0 && states[1][2] == 30.0, "query state %g %g", states[1][0], states[1][2]);
    pepper_state_query_free(q);
    CHECK(pepper_prepare_position(ctx, "-999", "EARTH", "J2000", "NONE", &q) == 0 && q != NULL,
          "prepare_position");
    double positions[2][3];
    CHECK(pepper_query_position(ctx, q, 2, ets, &positions[0][0], NULL) == 0, "query_position");
    CHECK(positions[1][0] == states[1][0] && positions[1][2] == states[1][2], "query position %g %g",
          positions[1][0], positions[1][2]);
    pepper_position_query_free(q);

    /* a kernel built in memory: Hermite segment on a circle, loaded with no
       file, written to an SPK, the file loaded by another instance */
    {
        enum { NS = 120 };
        static double ets[NS], sts[NS][6];
        for (int k = 0; k < NS; k++) {
            double a = 1e-3 * 60.0 * k;
            ets[k] = 60.0 * k;
            sts[k][0] = 7000.0 * cos(a); sts[k][1] = 7000.0 * sin(a); sts[k][2] = 0.0;
            sts[k][3] = -7.0 * sin(a);   sts[k][4] = 7.0 * cos(a);    sts[k][5] = 0.0;
        }
        void *kb = pepper_builder_new();
        CHECK(pepper_builder_spk(ctx, kb, 13, -998, 399, "J2000", NS, ets, &sts[0][0], 7, 0, 0.0, 0.0,
                                 "CAPI", 0) == 0, "builder_spk (Hermite)");
        CHECK(pepper_builder_spk(ctx, kb, 7, -998, 399, "J2000", NS, ets, &sts[0][0], 7, 0, 0.0, 0.0,
                                 "", 0) == 1, "builder_spk of an unknown type");
        pepper_getmsg(ctx, "SHORT", sizeof msg, msg);
        CHECK(strcmp(msg, "SPICE(UNSUPPORTEDINTERP)") == 0, "short message '%s'", msg);
        pepper_reset(ctx);
        CHECK(pepper_builder_nsegments(kb) == 1, "builder: one segment");
        pepper_builder_set_format(kb, "C TEST");
        CHECK(pepper_builder_define_body(ctx, kb, "CAPI_SC", -998) == 0, "builder_define_body");
        CHECK(pepper_spice_load_kernel(spice, "capi_kernel", kb, 0) == 0, "load_kernel");
        CHECK(pepper_builder_nsegments(kb) == 0, "load_kernel empties the builder");
        CHECK(pepper_spkezr(ctx, "CAPI_SC", ets[10], "J2000", "NONE", "EARTH", state, &lt) == 0,
              "spkezr of the kernel, by the name it defines");
        CHECK(fabs(state[0] - sts[10][0]) < 1e-9 && fabs(state[4] - sts[10][4]) < 1e-12, "the kernel's state %g %g",
              state[0], state[4]);
        const char *ld, *fm;
        int ld_len, fm_len, found2;
        CHECK(pepper_kernel_format(ctx, "capi_kernel", &ld, &ld_len, &fm, &fm_len, &found2) == 0 && found2 &&
              ld_len == 0 && fm_len == 6 && strncmp(fm, "C TEST", 6) == 0, "kernel_format");
        const char *bsp = "test_capi_kernel.bsp";
        remove(bsp);
        CHECK(pepper_spice_write_kernel(spice, "capi_kernel", bsp, NULL, NULL) == 0, "write_kernel");
        void *other = pepper_spice_new();
        void *octx = pepper_context_new(other);
        CHECK(pepper_spice_furnsh(other, bsp, -1) == 0, "the written SPK loads");
        int same = 1;
        for (int k = 0; k < 500; k++) {
            double t = ets[NS - 1] * k / 499.0, s1[6], s2[6], lt1, lt2;
            pepper_spkgeo(ctx, -998, t, "J2000", 399, s1, &lt1);
            pepper_spkgeo(octx, -998, t, "J2000", 399, s2, &lt2);
            for (int j = 0; j < 6; j++) same = same && s1[j] == s2[j];
        }
        CHECK(same && !pepper_failed(ctx) && !pepper_failed(octx), "the written SPK: the same states, bitwise");
        pepper_context_free(octx);
        pepper_spice_free(other);
        CHECK(pepper_spice_unload(spice, "capi_kernel") == 0, "unload of the kernel");
        int code2, found3;
        CHECK(pepper_bodn2c(ctx, "CAPI_SC", &code2, &found3) == 0 && !found3, "the kernel's name is gone");
        pepper_builder_free(kb);
        remove(bsp);
    }

    /* a kernel loader written in C: rows "et x y z vx vy vz" after a line
       "CEPH body"; its builder gets a table segment, a provider, a frame
       table and a model frame */
    {
        static int loads = 0, unloads = 0;
        const char *path = "test_capi_loader.ceph";
        FILE *f = fopen(path, "w");
        fprintf(f, "CEPH -997\n");
        for (int k = 0; k < 120; k++) {
            double a = 1e-3 * 60.0 * k;
            fprintf(f, "%.17g %.17g %.17g 0 %.17g %.17g 0\n", 60.0 * k, 7000.0 * cos(a), 7000.0 * sin(a),
                    -7.0 * sin(a), 7.0 * cos(a));
        }
        fclose(f);
        CHECK(pepper_spice_add_loader(spice, "CEPH", 1, 6, ".ceph", c_load, c_detect, c_unload, &loads, 0) == 0,
              "add_loader (C)");
        (void)unloads;
        CHECK(pepper_spice_furnsh(spice, path, -1) == 0, "furnsh by the C loader's extension");
        CHECK(pepper_spkgeo(ctx, -997, 600.0, "J2000", 399, state, &lt) == 0 && fabs(state[0] - 7000.0 * cos(0.6)) < 1e-6,
              "the C loader's segment %g", state[0]);
        CHECK(pepper_spkgeo(ctx, -996, 1.0, "J2000", 399, state, &lt) == 0 && state[0] == 7100.0,
              "the C loader's provider %g", state[0]);
        double rot[9];
        CHECK(pepper_pxform(ctx, "J2000", "CEPH_ROT", 1234.5, rot) == 0 && fabs(rot[0] - cos(1.2345)) < 1e-8,
              "the C loader's frame table %g", rot[0]);
        CHECK(pepper_pxform(ctx, "J2000", "CEPH_MODEL", 1.0, rot) == 0 && rot[0] == 1.0,
              "the C loader's model frame %g", rot[0]);
        CHECK(loads == 1, "the C loader's user data");
        CHECK(pepper_spice_unload(spice, path) == 0 && loads == 0, "unload calls the C loader's unload");
        CHECK(pepper_spice_furnsh_loader(spice, path, -1, "NOPE") == 1, "furnsh with an unknown loader");
        pepper_spice_getmsg(spice, "SHORT", sizeof msg, msg);
        CHECK(strcmp(msg, "SPICE(UNKNOWNLOADER)") == 0, "short message '%s'", msg);
        pepper_spice_reset(spice);
        CHECK(pepper_spice_remove_loader(spice, "CEPH") == 0, "remove_loader");
        remove(path);
    }

    /* a math routine without a context */
    double r[9];
    double axis[3] = {0.0, 0.0, 1.0};
    pepper_axisar(axis, M_PI / 2, r);   /* column-major */
    CHECK(fabs(r[1] - 1.0) < 1e-15 && fabs(r[3] + 1.0) < 1e-15, "axisar %g %g", r[1], r[3]);

    pepper_context_free(ctx);
    pepper_spice_free(spice);

    if (failures == 0) printf("test_capi: all tests passed\n");
    return failures != 0;
}
