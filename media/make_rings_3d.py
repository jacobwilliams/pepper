#!/usr/bin/env python3
"""
Generate a variant of the 3D PEPPER logo (make_logo_3d.py): the same pepper
shaker, pouring out rings of power instead of planets, as

  pepper_rings.glb   glTF 2.0 binary: geometry, materials and textures in
                     one file; each ring is a node placed and tilted (its
                     meshes around its own axis, Y: a diameter of the ring,
                     through its stone), so it can be spun like a coin (the
                     website's rings.html does);
  pepper_rings.3ds   3D Studio (without the glowing inscription: 3DS has
                     no emissive textures).

The rings take the planets' places (make_logo.SPRAY), a little larger
than the planets, the largest first:

  ONE           Jupiter   plain gold, domed, with a glowing inscription
                          (an invented script, drawn here)
  FIRE          Saturn    gold, a ruby in a bezel
  AIR           Earth     gold, a sapphire
  WATER         Neptune   silver, a white stone
  DWARF1, 2     Uranus,   broad gold bands, square-cut emerald and amber
                Mars
  MAN1, 2, 3    Venus,    iron bands with a silver inlay and a small
                Moon,     dark red stone
                Mercury

The dust becomes golden sparks. A ring's meshes (band, setting, stone,
inlay) are nodes named RING, RING_SET, RING_GEM, RING_INLAY with the same
placement; the website spins every node whose name before "_" is a ring's.

Units, axes and the camera are make_logo_3d.py's.

Usage:
    python make_rings_3d.py                 # writes 3d/ next to this script
    python make_rings_3d.py -o some/dir     # another output directory

Only Python's standard library is needed.
"""

import argparse
import math
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_logo_3d as base  # noqa: E402  (the shaker, the layout, the writers)
from make_logo_3d import (SCALE, Mesh, Profile, add_shaker, cross, dot, hex_rgb, lathe,  # noqa: E402
                          mix, norm, png_bytes, rot_x, sphere, spray_places, sub)

# The planet each ring replaces, and its design: its size (its outer radius
# R over the planet's radius: a ring shows less than a disc), the band's
# profile (as fractions of R: thickness, width along the axis, the
# cross-section's squareness), its metal, and its stone (or none).
RINGS = {
    "jupiter": dict(name="ONE", metal="gold", size=1.5, thick=0.26, width=0.4, square=2.3, inscription=True),
    "saturn": dict(name="FIRE", metal="gold", size=1.3, thick=0.2, width=0.3, square=3.0,
                   stone=dict(color="#d11a2a", girdle=0.3, sides=8)),
    "earth": dict(name="AIR", metal="gold", size=1.3, thick=0.2, width=0.3, square=3.0,
                  stone=dict(color="#2050d8", girdle=0.3, sides=8)),
    "neptune": dict(name="WATER", metal="silver", size=1.3, thick=0.2, width=0.3, square=3.0,
                    stone=dict(color="#eef6ff", girdle=0.3, sides=12)),
    "uranus": dict(name="DWARF1", metal="gold", size=1.3, thick=0.24, width=0.55, square=6.0,
                   stone=dict(color="#1f9d55", girdle=0.36, sides=4)),
    "mars": dict(name="DWARF2", metal="gold", size=1.3, thick=0.24, width=0.55, square=6.0,
                 stone=dict(color="#e08a1e", girdle=0.36, sides=4)),
    "venus": dict(name="MAN1", metal="iron", size=1.3, thick=0.22, width=0.38, square=4.0, inlay=True,
                  stone=dict(color="#a3121f", girdle=0.2, sides=6)),
    "moon": dict(name="MAN2", metal="iron", size=1.3, thick=0.22, width=0.38, square=4.0, inlay=True,
                 stone=dict(color="#a3121f", girdle=0.2, sides=6)),
    "mercury": dict(name="MAN3", metal="iron", size=1.3, thick=0.22, width=0.38, square=4.0, inlay=True,
                    stone=dict(color="#a3121f", girdle=0.2, sides=6)),
}

# each ring's tilt: (toward the viewer, in the logo's plane), degrees
TILTS = {"ONE": (18, 12), "FIRE": (34, -22), "AIR": (-26, 30), "WATER": (16, -40),
         "DWARF1": (-30, 16), "DWARF2": (38, 24), "MAN1": (-22, -30), "MAN2": (26, 40), "MAN3": (-34, -12)}

# metals: not fully metallic (see make_logo_3d.py's chrome)
METALS = {
    "gold": {"color": "#f0c24e", "metal": 0.9, "rough": 0.18},
    "silver": {"color": "#e4e9f0", "metal": 0.9, "rough": 0.15},
    "iron": {"color": "#8a8f9a", "metal": 0.8, "rough": 0.38},
}

ONE_GLYPHS = 36              # letters around the One ring
ONE_BAND = 0.1               # half-height of the inscription, as a fraction of the profile's length


# ---------------------------------------------------------------------------
# Geometry (logo pixels; the ring in its local XY plane, its axis Z, its
# stone at +Y; scaled to model units at the end)
# ---------------------------------------------------------------------------
def sgnpow(x, p):
    return math.copysign(abs(x) ** p, x)


def band_profile(rc, thick, width, square, n=64):
    """The band's cross-section: a superellipse around radius RC, THICK
    radially and WIDTH along the axis, as one closed (radius, height) loop,
    counterclockwise from the middle of the inner face: the outer face's
    middle is at half the loop's length (the inscription's place)."""
    pts = []
    for k in range(n + 1):
        t = math.pi + 2 * math.pi * k / n
        pts.append((rc + 0.5 * thick * sgnpow(math.cos(t), 2 / square),
                    0.5 * width * sgnpow(math.sin(t), 2 / square)))
    return Profile([pts])


def to_ring_plane(m):
    """Turn a lathe (axis Y) so its axis is Z: the ring in the XY plane."""
    m.transform(lambda p, is_point: rot_x(p, 90))


def facet(m, a, b, c, center):
    """A flat triangle ABC of a cut stone, its normal pointing away from CENTER."""
    n = norm(cross(sub(b, a), sub(c, a)))
    g = tuple((x + y + z) / 3 for x, y, z in zip(a, b, c))
    if dot(n, sub(g, center)) < 0:
        n = tuple(-x for x in n)
    m.triangle(m.vertex(a, n, (0.5, 0.5)), m.vertex(b, n, (0.5, 0.5)), m.vertex(c, n, (0.5, 0.5)))


def stone(name, material, girdle, sides, y0):
    """A cut stone with its girdle (radius GIRDLE) at height Y0 on the Y axis,
    its table up: SIDES crown facets, a table, and a pavilion down to a
    point. Flat facets."""
    m = Mesh(name, material)
    crown, pavilion, table = 0.42 * girdle, 0.75 * girdle, 0.55 * girdle
    center = (0.0, y0, 0.0)
    off = math.pi / sides if sides == 4 else 0.0  # a square stone sits square to the ring

    def ring(r, h, k):
        a = 2 * math.pi * k / sides + off
        return (r * math.sin(a), h, r * math.cos(a))

    top = (0.0, y0 + crown, 0.0)
    culet = (0.0, y0 - pavilion, 0.0)
    for k in range(sides):
        g0, g1 = ring(girdle, y0, k), ring(girdle, y0, k + 1)
        t0, t1 = ring(table, y0 + crown, k), ring(table, y0 + crown, k + 1)
        facet(m, g0, g1, t1, center)
        facet(m, g0, t1, t0, center)
        facet(m, t0, t1, top, center)
        facet(m, g1, g0, culet, center)
    return m


def setting(name, material, girdle, sides, y0, rim):
    """The bezel holding the stone: a cup from inside the band (height RIM)
    up around the stone's girdle; SIDES flat sides for a square stone, round
    otherwise."""
    r = 1.12 * girdle
    prof = Profile([[(0.55 * girdle, rim - 0.4 * girdle), (r, y0 - 0.35 * girdle),
                     (r, y0 + 0.1 * girdle), (0.96 * girdle, y0 + 0.1 * girdle)]])
    return lathe(name, material, prof, slices=4 if sides == 4 else 32)


# ---------------------------------------------------------------------------
# The One ring's inscription: an invented flowing script (not any real
# alphabet), glowing on the outer face
# ---------------------------------------------------------------------------
def arc(cx, cy, rx, ry, a0, a1, n=10):
    return [(cx + rx * math.cos(math.radians(a0 + (a1 - a0) * k / n)),
             cy + ry * math.sin(math.radians(a0 + (a1 - a0) * k / n))) for k in range(n + 1)]


def glyph(rng):
    """One letter: polylines in x 0..1 (across the letter), y -1..1 (across
    the line of text): a stem, one or two bowls, and sometimes a mark above
    or below."""
    strokes = []
    stem_x = rng.uniform(0.3, 0.55)
    kind = rng.random()
    if kind < 0.7:
        lo, hi = (-0.45, 0.85) if rng.random() < 0.5 else (-0.85, 0.45)
        strokes.append([(stem_x, lo), (stem_x + 0.04, hi)])
        if rng.random() < 0.5:  # a hook at the top of the stem
            strokes.append(arc(stem_x + 0.14, hi, 0.14, 0.12, 180, 20, 6))
    for side in rng.sample([-1, 1], rng.choice([1, 1, 2])):
        cx = stem_x + side * 0.2
        a0, a1 = (100, 400) if side > 0 else (80, -220)
        strokes.append(arc(cx, -0.05, 0.2, 0.36, a0, a1, 14))
    mark = rng.random()
    if mark < 0.3:  # a tilde above
        strokes.append([(0.25 + 0.5 * k / 8, 0.72 + 0.08 * math.sin(math.pi * k / 4)) for k in range(9)])
    elif mark < 0.45:  # dots below
        strokes += [[(x, -0.75), (x + 0.02, -0.73)] for x in (0.35, 0.6)]
    # the line of text joins the letters, as cursive does
    strokes.append(arc(0.5, -0.42, 0.5, 0.08, 180, 360, 8))
    return strokes


def inscription_texture(profile, outer_radius, width=2048, height=512):
    """The One ring's emissive texture: fiery letters on the outer face,
    black elsewhere."""
    rng = random.Random(1)
    letters = [glyph(rng) for _ in range(ONE_GLYPHS)]
    # one letter's width, in units of the line's half-height (the surface's
    # proportions: around the ring, and along the profile)
    aspect = (2 * math.pi * outer_radius / ONE_GLYPHS) / (ONE_BAND * profile.total)
    segments = []  # per letter: ((x0, y0), (x1, y1)) in units of the half-height
    for g, strokes in enumerate(letters):
        segs = []
        for line_ in strokes:
            pts = [((g + x) * aspect, y) for x, y in line_]
            segs += list(zip(pts, pts[1:]))
        segments.append(segs)
    core, halo = hex_rgb("#fff0b0"), hex_rgb("#ff6a10")
    stroke = 0.065
    edge = 2.0 / (ONE_BAND * 2 * height)  # one texel, in half-heights

    def seg_dist(p, a, b):
        ab, ap = (b[0] - a[0], b[1] - a[1]), (p[0] - a[0], p[1] - a[1])
        ll = ab[0] ** 2 + ab[1] ** 2
        t = 0.0 if ll == 0 else max(0.0, min(1.0, (ap[0] * ab[0] + ap[1] * ab[1]) / ll))
        return math.hypot(ap[0] - t * ab[0], ap[1] - t * ab[1])

    def pixel(i, j):
        u, v = (i + 0.5) / width, 1 - (j + 0.5) / height
        y = (v - 0.5) / ONE_BAND
        if abs(y) > 1.4:
            return (0.0, 0.0, 0.0)
        x = u * ONE_GLYPHS * aspect
        g = int(u * ONE_GLYPHS)
        d = min(seg_dist((x, y), a, b)
                for k in (g - 1, g, g + 1) if 0 <= k < ONE_GLYPHS for a, b in segments[k])
        cov = max(0.0, min(1.0, 0.5 - (d - stroke) / edge))
        glow = 0.55 * math.exp(-max(d - stroke, 0.0) / 0.12)
        c = tuple(h * glow for h in halo)
        return mix(c, core, cov)

    return png_bytes(width, height, pixel)


# ---------------------------------------------------------------------------
# The scene
# ---------------------------------------------------------------------------
def build_ring(design, r, materials, textures):
    """The meshes of one ring of outer radius R (logo pixels)."""
    name = design["name"]
    thick, width = design["thick"] * r, design["width"] * r
    rc = r - thick / 2
    metal = design["metal"]
    materials.setdefault(metal, dict(METALS[metal]))
    big = bool(design.get("inscription"))  # the One ring: the finest mesh (the inscription)
    prof = band_profile(rc, thick, width, design["square"], 64 if big else 40)
    band_mat = metal
    if design.get("inscription"):
        textures["ONEGLOW.PNG"] = inscription_texture(prof, r)
        band_mat = "one"
        materials["one"] = dict(METALS["gold"], emission="ONEGLOW.PNG")
    band = lathe(name, band_mat, prof, slices=160 if big else 72)
    to_ring_plane(band)
    meshes = [band]

    if design.get("inlay"):
        materials.setdefault("inlay", dict(METALS["silver"]))
        inlay = lathe(f"{name}_INLAY", "inlay",
                      band_profile(r - 0.01 * r, 0.045 * r, 0.09 * r, 2.0, 16), slices=72)
        to_ring_plane(inlay)
        meshes.append(inlay)

    st = design.get("stone")
    if st:
        girdle = st["girdle"] * r
        y0 = r + 0.32 * girdle  # the girdle, above the band's outer face
        key = "stone_" + st["color"].lstrip("#")
        materials.setdefault(key, {"color": st["color"], "metal": 0.0, "rough": 0.04, "glow": 0.35})
        set_mat = "setting_" + metal
        materials.setdefault(set_mat, dict(METALS[metal]))
        meshes.append(setting(f"{name}_SET", set_mat, girdle, st["sides"], y0, r - 0.4 * thick))
        meshes.append(stone(f"{name}_GEM", key, girdle, st["sides"], y0))

    for m in meshes:
        m.transform(lambda p, is_point: tuple(SCALE * x for x in p) if is_point else p)
    return meshes


def build_scene():
    """The meshes, the materials and the textures (name -> PNG bytes)."""
    meshes, materials, textures = [], {}, {}
    tip, axis = add_shaker(meshes, materials, textures)
    nspark = 0
    for k, kind, r, center in spray_places(tip, axis):
        if kind == "dust":
            nspark += 1
            m = sphere(f"SPARK{nspark}", "spark", r * SCALE, 6, 12)
            m.translation, m.tilt = center, (0, 0)
            meshes.append(m)
            continue
        design = RINGS[kind]
        for m in build_ring(design, design["size"] * r, materials, textures):
            m.translation, m.tilt = center, TILTS[design["name"]]
            meshes.append(m)
    materials["spark"] = {"color": "#ffd27a", "metal": 0.0, "rough": 0.6, "glow": 1.0}
    return meshes, materials, textures


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-o", "--output", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "3d"),
                    help="output directory (default: 3d/ next to this script)")
    args = ap.parse_args()
    os.makedirs(args.output, exist_ok=True)

    meshes, materials, textures = build_scene()
    base.write_glb(os.path.join(args.output, "pepper_rings.glb"), meshes, materials, textures)
    base.write_3ds(os.path.join(args.output, "pepper_rings.3ds"), meshes, materials)
    for name, data in textures.items():
        with open(os.path.join(args.output, name), "wb") as f:
            f.write(data)
    nv = sum(len(m.pos) for m in meshes)
    nt = sum(len(m.tri) for m in meshes)
    print(f"wrote {args.output}: pepper_rings.glb, pepper_rings.3ds and {len(textures)} textures "
          f"({len(meshes)} meshes, {nv} vertices, {nt} triangles)")


if __name__ == "__main__":
    main()
