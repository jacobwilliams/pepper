#!/usr/bin/env python3
"""
Generate a 3D model of the PEPPER logo (make_logo.py): the pepper shaker
pouring planets, with textures, as

  pepper_logo.glb   glTF 2.0 binary: geometry, materials (metallic chrome,
                    glazed ceramic) and textures in one file, and a camera
                    looking at the scene as the logo shows it; each planet
                    is a node placed and tilted (its mesh around its own
                    axis, Y), so it can be spun (the website's hero does);
  pepper_logo.3ds   3D Studio, with its texture images next to it (3DS
                    stores only their file names, 8.3 DOS names).

The scene is the logo's "down" pose (the website's logo): the layout, sizes
and colors come from make_logo.py. The shaker is made of surfaces of
revolution from the logo's profile curves: the ceramic body, the chrome
collar and the chrome dome with its holes; the Greek evil eye is in the
body's texture. The planets are spheres with textures drawn from the logo's
designs (Jupiter's bands and red spot, Saturn and its rings, Earth, Mars,
the Moon, Neptune); the dust is small glowing spheres.

Units: 1 = 100 pixels of the 1000-pixel logo (the shaker is 3.5 tall); the
logo's plane is the model's XY plane, the viewer on the +Z side (glTF: Y up;
3DS: Z up, the logo in its XZ plane, viewed from -Y).

Usage:
    python make_logo_3d.py                 # writes 3d/ next to this script
    python make_logo_3d.py -o some/dir     # another output directory

Only Python's standard library is needed.
"""

import argparse
import json
import math
import os
import struct
import sys
import zlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_logo as logo  # noqa: E402  (the 2D logo: layout and colors)

SCALE = 0.01                 # model units per logo pixel
CENTER = (500.0, 385.0)      # the logo point at the model's origin (the graphic's center)
POSE = "down"                # the website's logo


# ---------------------------------------------------------------------------
# Small vector helpers
# ---------------------------------------------------------------------------
def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def norm(a):
    n = math.sqrt(dot(a, a))
    return (a[0] / n, a[1] / n, a[2] / n) if n > 0 else (0.0, 1.0, 0.0)


def rot_x(p, deg):
    t = math.radians(deg)
    c, s = math.cos(t), math.sin(t)
    return (p[0], c * p[1] - s * p[2], s * p[1] + c * p[2])


def rot_z(p, deg):
    t = math.radians(deg)
    c, s = math.cos(t), math.sin(t)
    return (c * p[0] - s * p[1], s * p[0] + c * p[1], p[2])


def hex_rgb(h):
    h = h.lstrip("#")
    return tuple(int(h[i:i + 2], 16) / 255 for i in (0, 2, 4))


def srgb_to_linear(c):
    return tuple(x / 12.92 if x <= 0.04045 else ((x + 0.055) / 1.055) ** 2.4 for x in c)


def mix(a, b, t):
    """Blend colors A and B (T = 0: A, 1: B)."""
    t = max(0.0, min(1.0, t))
    return tuple(x + (y - x) * t for x, y in zip(a, b))


def coverage(d, edge):
    """Antialiased coverage of a shape at signed distance D (< 0 inside),
    EDGE the width of one texel."""
    return max(0.0, min(1.0, 0.5 - d / edge))


# ---------------------------------------------------------------------------
# Textures (RGB or RGBA rows of floats 0..1) and PNG files
# ---------------------------------------------------------------------------
def png_bytes(width, height, pixel):
    """A PNG image from PIXEL(i, j) -> (r, g, b) or (r, g, b, a), 0..1."""
    rows = []
    channels = len(pixel(0, 0))
    for j in range(height):
        row = bytearray([0])
        for i in range(width):
            row.extend(int(round(255 * max(0.0, min(1.0, c)))) for c in pixel(i, j))
        rows.append(bytes(row))
    raw = b"".join(rows)

    def chunk(tag, data):
        return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)

    color_type = 6 if channels == 4 else 2
    return (b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, color_type, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(raw, 9))
            + chunk(b"IEND", b""))


def sphere_point(u, v):
    """The point of the unit sphere at texture coordinates (U, V): U the
    longitude (0.5 faces the viewer, +Z), V from the north pole (0) to the
    south pole (1)."""
    lat = math.pi * (0.5 - v)
    lon = 2 * math.pi * u - math.pi
    return (math.sin(lon) * math.cos(lat), math.sin(lat), math.cos(lon) * math.cos(lat))


def sphere_texture(width, height, color):
    """A planet's texture: COLOR(p, edge) of each texel's point on the unit
    sphere (EDGE: a texel's size in radians, for antialiasing)."""
    edge = math.pi / height

    def pixel(i, j):
        return color(sphere_point((i + 0.5) / width, (j + 0.5) / height), edge)

    return png_bytes(width, height, pixel)


def cap_distance(p, center, radius):
    """Signed angular distance from P to a spherical cap (< 0 inside)."""
    return math.acos(max(-1.0, min(1.0, dot(p, center)))) - radius


def ellipse_distance(p, lon0, lat0, a, b):
    """Approximate signed distance (radians) from P to an ellipse on the
    sphere centered at (LON0, LAT0), half axes A (east) and B (north)."""
    lat = math.asin(max(-1.0, min(1.0, p[1])))
    lon = math.atan2(p[0], p[2])
    dlon = (lon - lon0 + math.pi) % (2 * math.pi) - math.pi
    x, y = dlon * math.cos(lat), lat - lat0
    k = math.sqrt((x / a) ** 2 + (y / b) ** 2)
    return (k - 1) * min(a, b)


def front(dx, dy):
    """The unit sphere's point that the logo's 2D offset (DX, DY) (in radii,
    SVG's y down) shows on the face toward the viewer."""
    z = math.sqrt(max(0.0, 1 - dx * dx - dy * dy))
    return norm((dx, -dy, z))


# --- value noise, for Earth's continents and clouds and the gas bands ---
def _hash(i, j, k):
    n = (i * 73856093) ^ (j * 19349663) ^ (k * 83492791)
    n = (n << 13) ^ n
    return ((n * (n * n * 15731 + 789221) + 1376312589) & 0x7FFFFFFF) / 0x7FFFFFFF


def noise(p):
    x, y, z = p
    i, j, k = math.floor(x), math.floor(y), math.floor(z)
    fx, fy, fz = x - i, y - j, z - k
    fx, fy, fz = (f * f * (3 - 2 * f) for f in (fx, fy, fz))
    c = [[[_hash(i + a, j + b, k + d) for d in (0, 1)] for b in (0, 1)] for a in (0, 1)]
    x00 = c[0][0][0] + (c[1][0][0] - c[0][0][0]) * fx
    x10 = c[0][1][0] + (c[1][1][0] - c[0][1][0]) * fx
    x01 = c[0][0][1] + (c[1][0][1] - c[0][0][1]) * fx
    x11 = c[0][1][1] + (c[1][1][1] - c[0][1][1]) * fx
    y0 = x00 + (x10 - x00) * fy
    y1 = x01 + (x11 - x01) * fy
    return y0 + (y1 - y0) * fz


def fbm(p, octaves=4):
    total, amp, freq, norm_ = 0.0, 0.5, 1.0, 0.0
    for _ in range(octaves):
        total += amp * noise((p[0] * freq, p[1] * freq, p[2] * freq))
        norm_ += amp
        amp *= 0.5
        freq *= 2.0
    return total / norm_


# --- the planets' designs (make_logo.py's colors and features) ---
def moon_color(p, edge):
    c = hex_rgb("#c9c9d1")
    crater = hex_rgb("#8e8e98")
    # the logo's craters on the face toward the viewer, and a few behind
    craters = [(front(dx, dy), math.asin(s)) for dx, dy, s in
               [(-0.35, -0.2, 0.22), (0.3, 0.25, 0.28), (0.15, -0.45, 0.12), (-0.25, 0.5, 0.15)]]
    craters += [(norm(q), r) for q, r in [((0.6, 0.3, -0.7), 0.25), ((-0.7, -0.4, -0.6), 0.2),
                                          ((0.1, 0.8, -0.5), 0.15), ((-0.2, -0.1, -1.0), 0.3)]]
    for center, radius in craters:
        c = mix(c, crater, 0.8 * coverage(cap_distance(p, center, radius), edge))
    return c


def mars_color(p, edge):
    c = hex_rgb("#d9603b")
    c = mix(c, hex_rgb("#a8402a"), 0.7 * coverage(ellipse_distance(p, -0.2, -0.15, 0.65, 0.25), edge))
    c = mix(c, hex_rgb("#a8402a"), 0.5 * coverage(ellipse_distance(p, 2.4, 0.2, 0.5, 0.2), edge))
    lat = math.asin(max(-1.0, min(1.0, p[1])))
    c = mix(c, (1, 1, 1), 0.85 * coverage(math.radians(68) - abs(lat), edge))  # polar caps
    return c


def earth_color(p, edge):
    ocean, land = hex_rgb("#2f7fd6"), hex_rgb("#4caf63")
    lat = math.asin(max(-1.0, min(1.0, p[1])))
    h = fbm((p[0] * 1.1 + 7, p[1] * 1.1, p[2] * 1.1), 4)
    c = mix(ocean, land, coverage((0.6 - h) * 3, edge * 3))
    c = mix(c, (0.95, 0.97, 1.0), coverage(math.radians(74) - abs(lat), edge))  # ice
    cloud = fbm((p[0] * 2.5, p[1] * 8.0, p[2] * 2.5 + 3), 4)  # streaks along the latitudes
    return mix(c, (1, 1, 1), 0.7 * max(0.0, min(1.0, (cloud - 0.64) * 5)))


def banded(p, edge, base, bands, wobble):
    """A gas giant: BANDS of (y, height, color, opacity) in the logo's
    coordinates (radii, SVG's y down) over BASE, their edges wavy."""
    c = hex_rgb(base)
    y = -p[1] + wobble * (fbm((p[0] * 4, p[1] * 4, p[2] * 4), 3) - 0.5)
    for y0, hgt, color, op in bands:
        d = max(y0 - y, y - (y0 + hgt))  # signed distance across the band (radii)
        c = mix(c, hex_rgb(color), op * coverage(d, edge))
    return c


def jupiter_color(p, edge):
    bands = [(-0.75, 0.14, "#b9825a", 1), (-0.45, 0.18, "#f3dcb8", 1), (-0.2, 0.16, "#c98a5c", 1),
             (0.05, 0.2, "#f7e6c8", 1), (0.3, 0.14, "#a86a45", 1), (0.55, 0.18, "#e9c9a0", 1),
             (0.8, 0.14, "#b9825a", 1)]
    c = banded(p, edge, "#e2b98c", bands, 0.06)
    spot = ellipse_distance(p, 0.3, -math.asin(0.43), 0.25, 0.12)  # the Great Red Spot
    return mix(c, hex_rgb("#c4452d"), coverage(spot, edge))


def saturn_color(p, edge):
    bands = [(y, 0.16, "#c9a66b", 0.6) for y in (-0.55, -0.1, 0.4)]
    return banded(p, edge, "#ecd29c", bands, 0.03)


def neptune_color(p, edge):
    c = banded(p, edge, "#3f6fe0", [(-0.15, 0.14, "#7ea4ff", 0.6)], 0.04)
    spot = ellipse_distance(p, -0.25, -math.asin(0.35), 0.22, 0.11)
    return mix(c, hex_rgb("#22408f"), coverage(spot, edge))


TEXTURED = {"moon": moon_color, "mars": mars_color, "earth": earth_color,
            "jupiter": jupiter_color, "saturn": saturn_color, "neptune": neptune_color}
PLAIN = {"venus": "#e9c46a", "mercury": "#a39a92", "uranus": "#8fe0e0"}

# axis tilts: (toward the viewer, in the logo's plane), degrees; Saturn's
# rings are seen as in the logo (the ellipse's axes 0.6 : 2.15)
TILTS = {"jupiter": (0, 15), "saturn": (math.degrees(math.asin(0.6 / 2.15)), 22),
         "earth": (10, -23), "mars": (5, 25), "neptune": (0, 28), "uranus": (0, 80)}

SATURN_RING = (1.35, 2.3)  # inner and outer radius, in Saturn's radii


def ring_texture(width):
    """Saturn's rings, from the inner edge (left) to the outer: the logo's
    two shades, with gaps (alpha 0)."""
    inner, outer = SATURN_RING
    bands = [(1.35, 1.62, "#c8b48a", 0.55), (1.62, 1.92, "#f3e2b8", 0.95), (1.97, 2.27, "#d8c08f", 0.95)]
    edge = (outer - inner) / width

    def pixel(i, j):
        r = inner + (outer - inner) * (i + 0.5) / width
        c, a = hex_rgb("#d8c08f"), 0.0
        for r0, r1, color, op in bands:
            k = op * coverage(max(r0 - r, r - r1), edge)
            if k > 0:
                c = mix(c, hex_rgb(color), k / max(a + k, 1e-9) if a < 1 else k)
                a = a + k * (1 - a)
        return c + (a,)

    return png_bytes(width, 4, pixel)


# ---------------------------------------------------------------------------
# Meshes: positions, normals, texture coordinates (glTF's: V down), triangles
# ---------------------------------------------------------------------------
class Mesh:
    def __init__(self, name, material):
        self.name, self.material = name, material
        self.pos, self.nrm, self.uv, self.tri, self.group = [], [], [], [], []
        # a placement kept apart from the vertices (a planet's: its center,
        # and its axis tilted TOWARD the viewer then IN the logo's plane, in
        # degrees): the glTF file has it as its node's transform, so the
        # planet can spin about its own axis; the 3DS file applies it
        self.translation, self.tilt = None, None

    def placed(self):
        """Positions and normals with the placement applied."""
        if self.translation is None:
            return self.pos, self.nrm
        toward, inplane = self.tilt
        t = self.translation
        pos = [tuple(a + b for a, b in zip(rot_z(rot_x(p, toward), inplane), t)) for p in self.pos]
        nrm = [rot_z(rot_x(n, toward), inplane) for n in self.nrm]
        return pos, nrm

    def quaternion(self):
        """The placement's rotation (rotation about X by TOWARD, then about
        Z by INPLANE), as glTF's (x, y, z, w)."""
        a, b = (math.radians(x) / 2 for x in self.tilt)
        qx = (math.sin(a), 0.0, 0.0, math.cos(a))
        qz = (0.0, 0.0, math.sin(b), math.cos(b))
        # qz * qx
        return (qz[3] * qx[0] + qz[0] * qx[3] + qz[1] * qx[2] - qz[2] * qx[1],
                qz[3] * qx[1] - qz[0] * qx[2] + qz[1] * qx[3] + qz[2] * qx[0],
                qz[3] * qx[2] + qz[0] * qx[1] - qz[1] * qx[0] + qz[2] * qx[3],
                qz[3] * qx[3] - qz[0] * qx[0] - qz[1] * qx[1] - qz[2] * qx[2])

    def vertex(self, p, n, uv):
        self.pos.append(p)
        self.nrm.append(n)
        self.uv.append(uv)
        return len(self.pos) - 1

    def triangle(self, a, b, c, group=1):
        """Add triangle ABC, outward (counterclockwise seen from where its
        vertices' normals point); degenerate ones are dropped."""
        pa, pb, pc = self.pos[a], self.pos[b], self.pos[c]
        f = cross(sub(pb, pa), sub(pc, pa))
        if dot(f, f) < 1e-20:
            return
        n = tuple(x + y + z for x, y, z in zip(self.nrm[a], self.nrm[b], self.nrm[c]))
        self.tri.append((a, b, c) if dot(f, n) > 0 else (a, c, b))
        self.group.append(group)

    def transform(self, f):
        """Apply the rotation and translation F to positions (F(p, True)) and
        normals (F(n, False))."""
        self.pos = [f(p, True) for p in self.pos]
        self.nrm = [norm(f(n, False)) for n in self.nrm]


def bezier(points, n):
    """N + 1 points of the Bezier curve (quadratic or cubic) with control POINTS."""
    out = []
    for k in range(n + 1):
        t = k / n
        if len(points) == 3:
            p0, p1, p2 = points
            out.append(tuple((1 - t) ** 2 * a + 2 * (1 - t) * t * b + t * t * c for a, b, c in zip(p0, p1, p2)))
        else:
            p0, p1, p2, p3 = points
            out.append(tuple((1 - t) ** 3 * a + 3 * (1 - t) ** 2 * t * b + 3 * (1 - t) * t * t * c + t ** 3 * d
                             for a, b, c, d in zip(p0, p1, p2, p3)))
    return out


def line(p0, p1, n):
    return [tuple(a + (b - a) * k / n for a, b in zip(p0, p1)) for k in range(n + 1)]


class Profile:
    """A shaker part's half profile: segments of (radius, height) points in
    the logo's pixels, from the bottom up; the corners between segments are
    sharp. V (texture) is the arc length's fraction."""

    def __init__(self, segments):
        self.segments = segments
        self.length = [0.0]
        for seg in segments:
            for a, b in zip(seg, seg[1:]):
                self.length.append(self.length[-1] + math.dist(a, b))
            self.length.append(self.length[-1])  # the corner (zero length)
        self.total = self.length[-1]
        self.flat = [p for seg in segments for p in seg + [seg[-1]]]

    def at(self, v):
        """(radius, height) at the fraction V of the arc length."""
        s = v * self.total
        lo, hi = 0, len(self.length) - 1
        while hi - lo > 1:
            mid = (lo + hi) // 2
            if self.length[mid] <= s:
                lo = mid
            else:
                hi = mid
        span = self.length[hi] - self.length[lo]
        t = (s - self.length[lo]) / span if span > 0 else 0.0
        a, b = self.flat[lo], self.flat[hi]
        return (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t)


def lathe_point(r, h, u):
    """The point at radius R, height H and angle U (0..1; 0.5 faces +Z)."""
    a = 2 * math.pi * u - math.pi
    return (r * math.sin(a), h, r * math.cos(a))


def lathe(name, material, profile, slices=72):
    """The surface of revolution of PROFILE (logo pixels; Y up)."""
    m = Mesh(name, material)
    s0 = 0.0
    for g, seg in enumerate(profile.segments):
        lengths = [0.0]
        for a, b in zip(seg, seg[1:]):
            lengths.append(lengths[-1] + math.dist(a, b))
        rings = []
        for k, (r, h) in enumerate(seg):
            # the outward normal from the profile's tangent (central differences)
            pa, pb = seg[max(k - 1, 0)], seg[min(k + 1, len(seg) - 1)]
            dr, dh = pb[0] - pa[0], pb[1] - pa[1]
            nr, nh = dh, -dr
            ln = math.hypot(nr, nh) or 1.0
            nr, nh = nr / ln, nh / ln
            v = (s0 + lengths[k]) / profile.total
            ring = []
            for j in range(slices + 1):
                u = j / slices
                p = lathe_point(r, h, u)
                n = lathe_point(nr, nh, u)
                ring.append(m.vertex(p, n, (u, 1 - v)))
            rings.append(ring)
        for k in range(len(rings) - 1):
            for j in range(slices):
                a, b, c, d = rings[k][j], rings[k][j + 1], rings[k + 1][j + 1], rings[k + 1][j]
                m.triangle(a, b, c, 1 << (g % 32))
                m.triangle(a, c, d, 1 << (g % 32))
        s0 += lengths[-1]
    return m


def sphere(name, material, radius, stacks, slices):
    m = Mesh(name, material)
    rows = []
    for i in range(stacks + 1):
        v = i / stacks
        row = []
        for j in range(slices + 1):
            u = j / slices
            p = sphere_point(u, v)
            row.append(m.vertex(tuple(radius * x for x in p), p, (u, v)))
        rows.append(row)
    for i in range(stacks):
        for j in range(slices):
            a, b, c, d = rows[i][j], rows[i][j + 1], rows[i + 1][j + 1], rows[i + 1][j]
            m.triangle(a, b, c)
            m.triangle(a, c, d)
    return m


def annulus(name, material, inner, outer, slices):
    """A flat ring in the XZ plane, both sides (U: inner to outer)."""
    m = Mesh(name, material)
    for side in (1, -1):
        ring = []
        for j in range(slices + 1):
            a = 2 * math.pi * j / slices
            c, s = math.cos(a), math.sin(a)
            ring.append((m.vertex((inner * c, 0, inner * s), (0, side, 0), (0.0, 0.5)),
                         m.vertex((outer * c, 0, outer * s), (0, side, 0), (1.0, 0.5))))
        for j in range(slices):
            (a, b), (d, c) = ring[j], ring[j + 1]
            m.triangle(a, b, c)
            m.triangle(a, c, d)
    return m


# ---------------------------------------------------------------------------
# The shaker
# ---------------------------------------------------------------------------
def body_profile():
    """make_logo.shaker()'s body, as (radius, height) (height = -y)."""
    return Profile([
        line((0, 0), (68, 0), 6),                               # the bottom
        bezier([(68, 0), (92, 0), (92, 24)], 10),               # its rounded edge
        line((92, 24), (92, 90), 8),
        bezier([(92, 90), (92, 160), (72, 200), (72, 240)], 40),
        line((72, 240), (0, 240), 6),                           # under the collar
    ])


def collar_profile():
    """The collar (a 168 x 28 rectangle with corners of radius 7)."""
    def arc(cx, cy, a0, a1, n=6):
        return [(cx + 7 * math.cos(math.radians(a0 + (a1 - a0) * k / n)),
                 cy + 7 * math.sin(math.radians(a0 + (a1 - a0) * k / n))) for k in range(n + 1)]
    return Profile([
        line((70, 238), (77, 238), 2) + arc(77, 245, -90, 0)[1:] + line((84, 245), (84, 259), 6)[1:]
        + arc(77, 259, 0, 90)[1:] + line((77, 266), (60, 266), 2)[1:],
    ])


def dome_profile():
    top = -logo.CAP_TOP
    return Profile([bezier([(80, 264), (80, 322), (42, top), (0, top)], 48)])


def dome_holes(profile):
    """The pour holes: points on the dome (as in the logo, a hole at the
    top and two rings), and their radius (pixels)."""
    holes = [lathe_point(*reversed(profile.at(1.0)), 0.5)]
    for v, count, phase in [(0.72, 6, 0.0), (0.42, 10, 0.05)]:
        r, h = profile.at(v)
        holes += [lathe_point(r, h, phase + k / count) for k in range(count)]
    return holes, 5.0


def body_texture(profile, width=1024, height=512):
    """The glazed ceramic with the evil eye (make_logo.shaker's emblem: navy,
    white, light blue and black discs), facing the viewer at height 120."""
    # the eye's center: on the outer wall at height 120, facing the viewer
    # (U = 0.5); distances are measured on the surface: along the profile
    # (arc length) and around the body (the arc at the texel's radius)
    v_eye = min((abs(profile.at(k / 4000)[1] - 120), k / 4000) for k in range(4001)
                if profile.at(k / 4000)[0] > 60)[1]
    rings = [(40, "#0e2a6b"), (37.5, "#1846a8"), (29, "#ffffff"), (19, "#5fb4ee"), (10, "#0b0f1e")]
    glaze = hex_rgb("#f4ecdd")
    edge = profile.total / height

    def pixel(i, j):
        u, v = (i + 0.5) / width, 1 - (j + 0.5) / height
        r, _ = profile.at(v)
        x = (u - 0.5) * 2 * math.pi * r       # around the body
        y = (v - v_eye) * profile.total       # up the profile
        d = math.hypot(x, y)
        c = glaze
        for radius, color in rings:
            c = mix(c, hex_rgb(color), coverage(d - radius, edge))
        # the eye's glint, up and to the left of the pupil
        g = math.hypot(x + 4, y - 4)
        return mix(c, (1, 1, 1), 0.9 * coverage(g - 3, edge))

    return png_bytes(width, height, pixel)


def dome_texture(profile, width=1024, height=256):
    holes, radius = dome_holes(profile)
    chrome, dark = hex_rgb("#dfe3ea"), hex_rgb("#1b1e28")
    edge = profile.total / height

    def pixel(i, j):
        u, v = (i + 0.5) / width, 1 - (j + 0.5) / height
        r, h = profile.at(v)
        p = lathe_point(r, h, u)
        d = min(math.dist(p, q) for q in holes)
        return mix(chrome, dark, coverage(d - radius, edge))

    return png_bytes(width, height, pixel)


# ---------------------------------------------------------------------------
# The scene
# ---------------------------------------------------------------------------
def to_model(x, y, z=0.0):
    """Logo pixels (SVG: y down) to model units (Y up, the viewer at +Z)."""
    return ((x - CENTER[0]) * SCALE, -(y - CENTER[1]) * SCALE, z * SCALE)


def add_shaker(meshes, materials, textures):
    """The shaker, placed in the logo's pose: its meshes, materials and
    textures added to MESHES, MATERIALS and TEXTURES. Returns the logo point
    of the holes (the spray's origin) and the direction the cap points
    (degrees, SVG)."""
    bp, cp, dp = body_profile(), collar_profile(), dome_profile()
    parts = [lathe("BODY", "ceramic", bp), lathe("COLLAR", "collar", cp), lathe("CAP", "chrome", dp)]
    textures["BODY.PNG"] = body_texture(bp)
    textures["CAP.PNG"] = dome_texture(dp)
    origin, tilt = logo.POSES[POSE]["origin"], logo.POSES[POSE]["tilt"]
    o = to_model(*origin)

    def place_shaker(p, is_point):
        q = rot_z(p, -tilt)  # SVG's clockwise rotation, with y up
        if not is_point:
            return q
        return (q[0] * SCALE + o[0], q[1] * SCALE + o[1], q[2] * SCALE + o[2])

    for m in parts:
        m.transform(place_shaker)
    meshes += parts
    materials["ceramic"] = {"texture": "BODY.PNG", "color": "#f4ecdd", "metal": 0.0, "rough": 0.3}
    # not fully metallic: a fully metallic surface shows only reflections,
    # black in a viewer without an environment map
    materials["chrome"] = {"texture": "CAP.PNG", "color": "#dfe3ea", "metal": 0.75, "rough": 0.22}
    materials["collar"] = {"color": "#b4bac6", "metal": 0.75, "rough": 0.28}

    ox, oy = origin
    tx, ty = logo.rotate(0, logo.CAP_TOP, tilt)
    return (ox + tx, oy + ty), math.degrees(math.atan2(ty, tx))


def spray_places(tip, axis):
    """Where make_logo.spray() puts the planets and the dust: for each entry
    of make_logo.SPRAY, (index, kind, radius in logo pixels, center in model
    units)."""
    for k, (dist, off, kind, r) in enumerate(logo.SPRAY):
        a = math.radians(axis + off)
        cx, cy = tip[0] + dist * math.cos(a), tip[1] + dist * math.sin(a)
        depth = 45 * math.sin(1.7 * k + 0.4)  # some in front of the logo's plane, some behind
        yield k, kind, r, to_model(cx, cy, depth)


def build_scene():
    """The meshes, the materials and the textures (name -> PNG bytes)."""
    meshes, materials, textures = [], {}, {}

    # --- the shaker: upright at the origin (logo pixels), then placed ---
    tip, axis = add_shaker(meshes, materials, textures)

    # --- the planets and the dust, where make_logo.spray() puts them ---
    ndust = 0
    for k, kind, r, center in spray_places(tip, axis):
        if kind == "dust":
            ndust += 1
            m = sphere(f"DUST{ndust}", "dust", r * SCALE, 6, 12)
        else:
            big = r >= 20
            m = sphere(kind.upper()[:10], kind, r * SCALE, 36 if big else 24, 72 if big else 48)
            if kind in TEXTURED:
                name = kind.upper()[:8] + ".PNG"
                textures[name] = sphere_texture(512, 256, TEXTURED[kind])
                materials[kind] = {"texture": name, "color": "#ffffff", "metal": 0.0, "rough": 0.85}
            else:
                materials[kind] = {"color": PLAIN[kind], "metal": 0.0, "rough": 0.85}
        m.translation, m.tilt = center, TILTS.get(kind, (0, 0))
        meshes.append(m)
        if kind == "saturn":
            ring = annulus("RINGS", "rings", SATURN_RING[0] * r * SCALE, SATURN_RING[1] * r * SCALE, 96)
            ring.translation, ring.tilt = m.translation, m.tilt
            meshes.append(ring)
            textures["RINGS.PNG"] = ring_texture(512)
            materials["rings"] = {"texture": "RINGS.PNG", "color": "#ffffff", "metal": 0.0, "rough": 0.8,
                                  "blend": True, "double": True}
    materials["dust"] = {"color": "#ffe7b8", "metal": 0.0, "rough": 0.6, "glow": 0.9}
    return meshes, materials, textures


# ---------------------------------------------------------------------------
# glTF 2.0 binary (.glb)
# ---------------------------------------------------------------------------
def write_glb(path, meshes, materials, textures):
    blob = bytearray()
    views, accessors = [], []

    def add_view(data, target=None):
        while len(blob) % 4:
            blob.append(0)
        view = {"buffer": 0, "byteOffset": len(blob), "byteLength": len(data)}
        if target:
            view["target"] = target
        blob.extend(data)
        views.append(view)
        return len(views) - 1

    def add_accessor(values, kind, ctype, target, minmax=False):
        flat = [x for v in values for x in (v if isinstance(v, tuple) else (v,))]
        fmt = {5126: "f", 5125: "I", 5123: "H"}[ctype]
        acc = {"bufferView": add_view(struct.pack(f"<{len(flat)}{fmt}", *flat), target),
               "componentType": ctype, "count": len(values), "type": kind}
        if minmax:
            acc["min"] = [min(v[i] for v in values) for i in range(3)]
            acc["max"] = [max(v[i] for v in values) for i in range(3)]
        accessors.append(acc)
        return len(accessors) - 1

    images, gl_textures = [], []
    tex_index = {}
    for name, data in textures.items():
        images.append({"bufferView": add_view(data), "mimeType": "image/png", "name": name})
        gl_textures.append({"source": len(images) - 1, "sampler": 0})
        tex_index[name] = len(gl_textures) - 1

    gl_materials, mat_index = [], {}
    for name, m in materials.items():
        pbr = {"baseColorFactor": list(srgb_to_linear(hex_rgb(m["color"]))) + [1.0],
               "metallicFactor": m["metal"], "roughnessFactor": m["rough"]}
        if "texture" in m:
            pbr["baseColorTexture"] = {"index": tex_index[m["texture"]]}
        mat = {"name": name, "pbrMetallicRoughness": pbr}
        if m.get("glow"):
            mat["emissiveFactor"] = [m["glow"] * x for x in srgb_to_linear(hex_rgb(m["color"]))]
        if "emission" in m:
            # a glowing pattern: the emissive texture, at full strength
            mat["emissiveTexture"] = {"index": tex_index[m["emission"]]}
            mat["emissiveFactor"] = [1.0, 1.0, 1.0]
        if m.get("blend"):
            mat["alphaMode"] = "BLEND"
        if m.get("double"):
            mat["doubleSided"] = True
        gl_materials.append(mat)
        mat_index[name] = len(gl_materials) - 1

    gl_meshes, nodes = [], []
    for m in meshes:
        indices = [i for t in m.tri for i in t]
        prim = {"attributes": {"POSITION": add_accessor(m.pos, "VEC3", 5126, 34962, True),
                               "NORMAL": add_accessor(m.nrm, "VEC3", 5126, 34962),
                               "TEXCOORD_0": add_accessor(m.uv, "VEC2", 5126, 34962)},
                "indices": add_accessor(indices, "SCALAR", 5123 if len(m.pos) < 65536 else 5125, 34963),
                "material": mat_index[m.material]}
        gl_meshes.append({"name": m.name, "primitives": [prim]})
        node = {"name": m.name, "mesh": len(gl_meshes) - 1}
        if m.translation is not None:
            node["translation"] = list(m.translation)
            node["rotation"] = list(m.quaternion())
        nodes.append(node)
    # a camera looking at the scene as the logo shows it (the whole of it)
    nodes.append({"name": "Camera", "camera": 0, "translation": [0.0, 0.0, 14.0]})

    gltf = {
        "asset": {"version": "2.0", "generator": "PEPPER media/make_logo_3d.py"},
        "scene": 0,
        "scenes": [{"name": "PEPPER logo", "nodes": list(range(len(nodes)))}],
        "nodes": nodes, "meshes": gl_meshes, "materials": gl_materials,
        "textures": gl_textures, "images": images,
        "samplers": [{"magFilter": 9729, "minFilter": 9987, "wrapS": 10497, "wrapT": 33071}],
        "cameras": [{"type": "perspective", "perspective": {"yfov": 0.6, "znear": 0.1, "zfar": 100.0}}],
        "accessors": accessors, "bufferViews": views,
        "buffers": [{"byteLength": len(blob)}],
    }
    while len(blob) % 4:
        blob.append(0)
    js = json.dumps(gltf, separators=(",", ":")).encode()
    js += b" " * (-len(js) % 4)
    with open(path, "wb") as f:
        f.write(struct.pack("<III", 0x46546C67, 2, 12 + 8 + len(js) + 8 + len(blob)))
        f.write(struct.pack("<II", len(js), 0x4E4F534A) + js)
        f.write(struct.pack("<II", len(blob), 0x004E4942) + bytes(blob))


# ---------------------------------------------------------------------------
# 3D Studio (.3ds)
# ---------------------------------------------------------------------------
def chunk(cid, payload=b"", children=()):
    data = payload + b"".join(children)
    return struct.pack("<HI", cid, 6 + len(data)) + data


def cstr(s):
    return s.encode("ascii") + b"\0"


def color24(hexcolor):
    return chunk(0x0011, bytes(int(round(255 * c)) for c in hex_rgb(hexcolor)))


def percent(value):
    return chunk(0x0030, struct.pack("<h", int(round(value))))


def write_3ds(path, meshes, materials):
    """3DS: Z up; the model's (x, y, z) (Y up) is (x, -z, y)."""
    mats = []
    for name, m in materials.items():
        sub_ = [chunk(0xA000, cstr(name[:16])),
                chunk(0xA010, b"", [color24(m["color"])]),
                chunk(0xA020, b"", [color24(m["color"])]),
                chunk(0xA030, b"", [color24("#ffffff" if m["metal"] else "#333333")]),
                chunk(0xA040, b"", [percent(100 * (1 - m["rough"]))]),
                chunk(0xA041, b"", [percent(100 if m["metal"] else 30)])]
        if m.get("glow"):
            sub_.append(chunk(0xA084, b"", [percent(100 * m["glow"])]))
        if m.get("double"):
            sub_.append(chunk(0xA081))
        if "texture" in m:
            sub_.append(chunk(0xA200, b"", [percent(100), chunk(0xA300, cstr(m["texture"]))]))
            if m.get("blend"):
                sub_.append(chunk(0xA210, b"", [percent(100), chunk(0xA300, cstr(m["texture"]))]))
        mats.append(chunk(0xAFFF, b"", sub_))

    objects = []
    for m in meshes:
        if len(m.pos) > 65535 or len(m.tri) > 65535:
            raise ValueError(f"{m.name}: too many vertices or faces for 3DS")
        pos, _ = m.placed()
        verts = b"".join(struct.pack("<3f", p[0], -p[2], p[1]) for p in pos)
        uvs = b"".join(struct.pack("<2f", u, 1 - v) for u, v in m.uv)
        faces = b"".join(struct.pack("<4H", a, b, c, 7) for a, b, c in m.tri)
        face_mat = chunk(0x4130, cstr(m.material[:16]) + struct.pack("<H", len(m.tri))
                         + struct.pack(f"<{len(m.tri)}H", *range(len(m.tri))))
        smooth = chunk(0x4150, struct.pack(f"<{len(m.group)}I", *m.group))
        trimesh = chunk(0x4100, b"", [
            chunk(0x4110, struct.pack("<H", len(m.pos)) + verts),
            chunk(0x4140, struct.pack("<H", len(m.uv)) + uvs),
            chunk(0x4160, struct.pack("<12f", 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)),
            chunk(0x4120, struct.pack("<H", len(m.tri)) + faces, [face_mat, smooth]),
        ])
        objects.append(chunk(0x4000, cstr(m.name[:10]), [trimesh]))

    editor = chunk(0x3D3D, b"", [chunk(0x3D3E, struct.pack("<I", 3))] + mats
                   + [chunk(0x0100, struct.pack("<f", 1.0))] + objects)
    with open(path, "wb") as f:
        f.write(chunk(0x4D4D, b"", [chunk(0x0002, struct.pack("<I", 3)), editor]))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-o", "--output", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "3d"),
                    help="output directory (default: 3d/ next to this script)")
    args = ap.parse_args()
    os.makedirs(args.output, exist_ok=True)

    meshes, materials, textures = build_scene()
    write_glb(os.path.join(args.output, "pepper_logo.glb"), meshes, materials, textures)
    write_3ds(os.path.join(args.output, "pepper_logo.3ds"), meshes, materials)
    for name, data in textures.items():
        with open(os.path.join(args.output, name), "wb") as f:
            f.write(data)
    nv = sum(len(m.pos) for m in meshes)
    nt = sum(len(m.tri) for m in meshes)
    print(f"wrote {args.output}: pepper_logo.glb, pepper_logo.3ds and {len(textures)} textures "
          f"({len(meshes)} meshes, {nv} vertices, {nt} triangles)")


if __name__ == "__main__":
    main()
