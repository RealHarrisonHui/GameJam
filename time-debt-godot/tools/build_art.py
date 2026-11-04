#!/usr/bin/env python3
"""Bakes the level map (albedo + normal) and sprites for ARREARS.

Everything is original to this prototype: procedural rasters from numpy and
authored SVGs rendered by rsvg-convert. Geometry comes from
resources/level_layout.json, the same file the game loads.

  .tools/venv/bin/python tools/build_art.py
"""
import json, math, subprocess, pathlib
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage as ndi

ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT = ROOT / "assets" / "art"
SVG = ROOT / "assets" / "svg"
OUT.mkdir(parents=True, exist_ok=True)
SVG.mkdir(parents=True, exist_ok=True)

S = 2  # pixels per world unit
W, H = 1280 * S, 720 * S
rng = np.random.default_rng(1926)
layout = json.loads((ROOT / "resources" / "level_layout.json").read_text())


def hex3(h):
    h = h.lstrip("#")
    return np.array([int(h[i : i + 2], 16) / 255 for i in (0, 2, 4)], dtype=np.float32)


# Palette: bottle-green marble, walnut, brass, ivory, amber lamplight.
INK = hex3("#100d0b")
WALNUT_D = hex3("#1b1411")
WALNUT_L = hex3("#2b1e17")
BRASS = hex3("#c49a45")
BRASS_D = hex3("#6b4f22")
BRASS_L = hex3("#efd694")
IVORY = hex3("#efe6cf")
GREEN_D = hex3("#27463f")
GREEN_L = hex3("#3b6157")
IRON_D = hex3("#23262a")
IRON_L = hex3("#34383d")
OAK_D = hex3("#6a4525")
OAK_L = hex3("#8c6233")
AMBER = hex3("#c98f3c")
CREAM = hex3("#e6d4a8")
OXBLOOD = hex3("#6d1f1d")


def rect(r):
    """Gloss [l, b, r, t] to integer pixel bounds x0, y0, x1, y1."""
    l, b, rr, t = r
    return (round((l + 640) * S), round((360 - t) * S), round((rr + 640) * S), round((360 - b) * S))


def mask_of(rects):
    m = np.zeros((H, W), dtype=bool)
    for r in rects:
        x0, y0, x1, y1 = rect(r)
        m[y0:y1, x0:x1] = True
    return m


def noise(scale, octaves=4, seed=None):
    """Smooth value noise in 0..1."""
    g = np.random.default_rng(seed if seed is not None else rng.integers(1 << 30))
    out = np.zeros((H, W), dtype=np.float32)
    amp, total = 1.0, 0.0
    for o in range(octaves):
        cell = max(2, int(scale / (2**o)))
        gh, gw = H // cell + 2, W // cell + 2
        grid = g.random((gh, gw), dtype=np.float32)
        layer = ndi.zoom(grid, cell, order=3)[: H, : W]
        out += layer * amp
        total += amp
        amp *= 0.5
    return np.clip(out / total, 0, 1)


yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
wx, wy = xx / S, yy / S  # world coordinates

F = layout["floors"]
floor_mask = mask_of(F.values())
wall_mask = ~floor_mask
dist_to_floor = ndi.distance_transform_edt(wall_mask).astype(np.float32) / S  # in walls: px to floor
dist_to_wall = ndi.distance_transform_edt(floor_mask).astype(np.float32) / S  # in floors: px to wall


def lerp3(a, b, t):
    return a[None, None, :] * (1 - t[..., None]) + b[None, None, :] * t[..., None]


def line(d, width, soft=0.5):
    """1 on a band |d| < width/2, with a soft edge (world units)."""
    return np.clip((width / 2 - np.abs(d)) / soft + 0.5, 0, 1)


albedo = np.zeros((H, W, 3), dtype=np.float32)
height = np.zeros((H, W), dtype=np.float32)

# ------------------------------------------------------------------ walls
grain_n = noise(90, 3, 11)
grain = np.sin((wy * 0.5 + grain_n * 14 + wx * 0.01)) * 0.5 + 0.5
wall = lerp3(WALNUT_D, WALNUT_L, grain * 0.55 + noise(40, 2, 12) * 0.25)
albedo[wall_mask] = wall[wall_mask]
height[wall_mask] = 0.9 + grain[wall_mask] * 0.02

# brass fillet following every floor edge
fillet = line(dist_to_floor - 7.5, 2.0) * wall_mask
inner = line(dist_to_floor - 2.0, 3.6) * wall_mask
albedo = albedo * (1 - fillet[..., None]) + BRASS[None, None, :] * fillet[..., None] * 0.92
height += fillet * 0.10
albedo = albedo * (1 - inner[..., None]) + (BRASS_D[None, None, :] * 0.55) * inner[..., None]
height -= inner * 0.25

# ------------------------------------------------------------------ floors
def apply_floor(mask, color, h):
    albedo[mask] = color[mask]
    height[mask] = h[mask]


def tile_lines(px, py, size, thick):
    gx = np.abs(((px / size) % 1.0) - 0.5) * size
    gy = np.abs(((py / size) % 1.0) - 0.5) * size
    return np.clip((np.maximum(gx, gy) - (size / 2 - thick)) / 0.6, 0, 1)


# Marble tile: start room and exit chamber share a family.
veins = noise(180, 5, 21)
vein = np.clip(1 - np.abs(np.sin(veins * 11 + wx * 0.02 + wy * 0.033)) * 5.0, 0, 1) ** 2
check = (((wx // 40).astype(int) + (wy // 40).astype(int)) % 2).astype(np.float32)
marble_t = check * 0.62 + noise(60, 3, 22) * 0.22
marble = lerp3(GREEN_D, GREEN_L, marble_t)
marble = marble * (1 - vein[..., None] * 0.2) + IVORY[None, None, :] * vein[..., None] * 0.2
g = tile_lines(wx, wy, 40, 0.9)
marble = marble * (1 - g[..., None]) + (BRASS * 0.72)[None, None, :] * g[..., None]
marble_h = 0.30 - g * 0.10 + vein * 0.01

start = mask_of([F["start_room"], F["door_1"]])
apply_floor(start, marble, marble_h)

# Iron plate: the two danger lanes.
plate_g = tile_lines(wx + 6, wy + 2, 46, 1.0)
plate_n = noise(35, 4, 31)
plate = lerp3(IRON_D, IRON_L, plate_n * 0.8 + (((wx + 6) // 46 + (wy + 2) // 46) % 2) * 0.12)
scratch = np.clip(noise(16, 2, 32) - 0.62, 0, 1) * 1.2
plate = plate * (1 + scratch[..., None] * 0.6)
plate = plate * (1 - plate_g[..., None] * 0.75)
fx = np.abs((((wx + 6) / 46) % 1) - 0.5) * 46
fy = np.abs((((wy + 2) / 46) % 1) - 0.5) * 46
rivet = np.clip(2.3 - np.hypot(fx - 19, fy - 19), 0, 1)
plate = plate * (1 - rivet[..., None]) + (IRON_L * 1.6)[None, None, :] * rivet[..., None]
plate_h = 0.22 - plate_g * 0.1 + rivet * 0.09 + plate_n * 0.015
lanes = mask_of([F["slot"], F["gallery"]])
apply_floor(lanes, plate, plate_h)


def hazard_edge(mask_rect, axis):
    """Oxblood-and-ivory chevrons along the long edges of a lane."""
    x0, y0, x1, y1 = rect(mask_rect)
    u = wx if axis == "x" else wy
    v = wy if axis == "x" else wx
    v0 = (y0 if axis == "x" else x0) / S
    v1 = (y1 if axis == "x" else x1) / S
    band = np.minimum(v - v0, v1 - v)
    chev = (((u + band * 1.0) / 18.0) % 1.0) < 0.5
    edge = (band >= 3.0) & (band <= 8.0)
    m = np.zeros((H, W), dtype=bool)
    m[y0:y1, x0:x1] = True
    edge &= m
    colour = np.where(chev[..., None], OXBLOOD[None, None, :] * 1.15, (IVORY * 0.62)[None, None, :])
    albedo[edge] = colour[edge]
    height[edge] += 0.01


hazard_edge(F["slot"], "y")
hazard_edge(F["gallery"], "x")

# Parquet: the safe rooms are warm. Herringbone by construction.
pb = 44.0
pw = 11.0
a = (wx + wy) / math.sqrt(2)
b = (wx - wy) / math.sqrt(2)
col = np.floor(a / (pb * 0.7071))
par = np.mod(col, 2) == 0
u_ = np.where(par, a, b)
v_ = np.where(par, b, a)
stave = np.floor(v_ / pw)
offset = stave * 11.0
along = np.mod(u_ + offset, pb)
stave_id = stave * 13.0 + np.floor((u_ + offset) / pb) * 7.0
tone = (np.sin(stave_id * 12.9898) * 43758.5453) % 1.0
pg = np.maximum(np.clip((np.abs(np.mod(v_, pw) - pw / 2) - (pw / 2 - 0.55)) / 0.5, 0, 1), np.clip((np.abs(along - pb / 2) - (pb / 2 - 0.55)) / 0.5, 0, 1))
wood_grain = np.sin(u_ * 0.5 + noise(60, 3, 41) * 22) * 0.5 + 0.5
parquet = lerp3(OAK_D, OAK_L, tone.astype(np.float32) * 0.75 + wood_grain * 0.2)
parquet = parquet * (1 - pg[..., None] * 0.72)
parquet_h = 0.3 - pg * 0.07 + wood_grain * 0.01
safe = mask_of([F["door_2"], F["still_room"], F["door_3"], F["alcove"]])
apply_floor(safe, parquet.astype(np.float32), parquet_h.astype(np.float32))

# Rug in the grace room: cream deco fans on amber, no red anywhere near it.
rug_x0, rug_y0, rug_x1, rug_y1 = 395.0, 214.0, 635.0, 464.0
in_rug = (wx >= rug_x0) & (wx <= rug_x1) & (wy >= rug_y0) & (wy <= rug_y1)
rx, ry = wx - (rug_x0 + rug_x1) / 2, wy - (rug_y0 + rug_y1) / 2
d_edge = np.minimum.reduce([wx - rug_x0, rug_x1 - wx, wy - rug_y0, rug_y1 - wy])
rug = np.broadcast_to(AMBER[None, None, :] * 0.74, (H, W, 3)).copy()
fan_y = np.mod(ry + 125, 38.0)
fan_x = np.mod(rx + 120 + (np.floor((ry + 125) / 38.0) % 2) * 19.0, 38.0) - 19.0
fan_r = np.hypot(fan_x, 38.0 - fan_y)
fan = np.minimum.reduce([np.abs(fan_r - 8.0), np.abs(fan_r - 15.0), np.abs(fan_r - 22.0)])
fan_on = np.clip(1.1 - fan, 0, 1) * (fan_r < 25.0)
rug = rug * (1 - fan_on[..., None] * 0.7) + (CREAM * 0.86)[None, None, :] * fan_on[..., None] * 0.7
for dist_, width, colour in [(5.0, 2.2, CREAM), (11.0, 1.2, CREAM), (2.0, 1.4, BRASS_D)]:
    l_ = line(d_edge - dist_, width)
    rug = rug * (1 - l_[..., None]) + colour[None, None, :] * l_[..., None]
weave = (np.sin(wx * 3.0) * np.sin(wy * 3.0)) * 0.02 + noise(14, 2, 51) * 0.06
rug = np.clip(rug * (1 + weave[..., None] - 0.03), 0, 1)
albedo[in_rug] = rug[in_rug]
height[in_rug] = 0.36 + fan_on[in_rug] * 0.015

# Exit chamber: pale gold marble, the brightest floor on the map.
exit_m = mask_of([F["door_4"], F["exit_chamber"]])
ex_t = noise(70, 4, 61)
ex_v = np.clip(1 - np.abs(np.sin(noise(150, 4, 62) * 10 + wy * 0.03 + wx * 0.018)) * 5.0, 0, 1) ** 2
ex = lerp3(hex3("#a08a5c"), hex3("#d9c490"), ex_t * 0.55 + check * 0.28)
ex = ex * (1 - ex_v[..., None] * 0.25) + hex3("#7b5e2e")[None, None, :] * ex_v[..., None] * 0.25
g2 = tile_lines(wx, wy, 40, 0.9)
ex = ex * (1 - g2[..., None]) + BRASS_D[None, None, :] * g2[..., None]
apply_floor(exit_m, ex, 0.30 - g2 * 0.10 + 0 * ex_t)

# Door thresholds: bright brass plates so doors read at a glance.
for key in ("door_1", "door_2", "door_3", "door_4"):
    x0, y0, x1, y1 = rect(F[key])
    plate_t = np.ones((y1 - y0, x1 - x0), dtype=np.float32)
    albedo[y0:y1, x0:x1] = BRASS * (0.86 + 0.14 * noise(10, 2, 71)[y0:y1, x0:x1, None])
    height[y0:y1, x0:x1] = 0.33

# Rails: a recessed groove along each hazard route, with end stops.
def groove_mask(route):
    (ax, ay), (bx, by) = [((p[0] + 640), (360 - p[1])) for p in route]
    dx, dy = bx - ax, by - ay
    ln2 = dx * dx + dy * dy
    t = np.clip(((wx - ax) * dx + (wy - ay) * dy) / ln2, 0, 1)
    return np.hypot(wx - (ax + t * dx), wy - (ay + t * dy)), (ax, ay), (bx, by)


for route in layout["routes"].values():
    d, A, B = groove_mask(route)
    g_o = np.clip(9.0 - d, 0, 1)
    g_i = np.clip(5.5 - d, 0, 1)
    g_c = np.clip(1.4 - d, 0, 1)
    albedo = albedo * (1 - g_o[..., None]) + (BRASS_D * 0.8)[None, None, :] * g_o[..., None]
    albedo = albedo * (1 - g_i[..., None]) + (INK * 1.0)[None, None, :] * g_i[..., None]
    albedo = albedo * (1 - g_c[..., None] * 0.6) + (BRASS * 0.8)[None, None, :] * g_c[..., None] * 0.6
    height -= g_i * 0.12
    for P_ in (A, B):
        rr = np.hypot(wx - P_[0], wy - P_[1])
        cap = np.clip(14.0 - rr, 0, 1)
        hub = np.clip(7.0 - rr, 0, 1)
        albedo = albedo * (1 - cap[..., None]) + (BRASS * 0.75)[None, None, :] * cap[..., None]
        albedo = albedo * (1 - hub[..., None]) + INK[None, None, :] * hub[..., None]
        height += cap * 0.04 - hub * 0.12

# ------------------------------------------------------------ shadow baking
blur = lambda a, s: ndi.gaussian_filter(a, s * S)
wall_f = wall_mask.astype(np.float32)
ao = 1 - 0.55 * np.exp(-dist_to_wall / 11.0)
drop = np.roll(np.roll(wall_f, 9 * S, axis=0), 6 * S, axis=1)
drop = blur(drop, 5.0)
floor_shade = ao * (1 - drop * 0.42)
albedo[floor_mask] = albedo[floor_mask] * floor_shade[floor_mask, None]

# Vault spill: warm light soaks the exit chamber and bleeds through its door.
ex_cx, ex_cy = 1175.0, 290.0
ex_r = np.hypot(wx - ex_cx, wy - ex_cy)
spill = np.clip(1.0 - ex_r / 150.0, 0, 1) ** 2 * floor_mask
albedo = np.clip(albedo * (1 + spill[..., None] * 0.28), 0, 1)

# Ambient vignette over the whole composition, kept off the danger lanes' contrast.
vx = (wx - 640) / 640
vy = (wy - 325) / 370
vig = 1 - np.clip(vx * vx + vy * vy - 0.55, 0, 1) * 0.5
albedo *= vig[..., None]

# --------------------------------------------------------- wall set-pieces
im = Image.fromarray((np.clip(albedo, 0, 1) * 255).astype(np.uint8), "RGB")
hm = Image.fromarray((np.clip(height, 0, 1) * 255).astype(np.uint8), "L")
d_a = ImageDraw.Draw(im, "RGBA")
d_h = ImageDraw.Draw(hm)


def col(c, a=255):
    return (int(c[0] * 255), int(c[1] * 255), int(c[2] * 255), a)


def panel(x0, y0, x1, y1, inset=True):
    """A recessed walnut panel with a double brass line."""
    p = [x0 * S, y0 * S, x1 * S, y1 * S]
    d_a.rectangle(p, fill=col(WALNUT_D * 0.72))
    d_a.rectangle(p, outline=col(BRASS_D, 255), width=2 * S)
    q = [(x0 + 6) * S, (y0 + 6) * S, (x1 - 6) * S, (y1 - 6) * S]
    d_a.rectangle(q, outline=col(BRASS, 150), width=1 * S)
    d_h.rectangle(p, fill=200)
    d_h.rectangle(p, outline=240, width=2 * S)
    d_h.rectangle(q, outline=222, width=1 * S)


def fan(cx, cy, r, ang0, ang1, n=9):
    """Deco sunburst, the recurring ornament."""
    for i in range(n + 1):
        a = math.radians(ang0 + (ang1 - ang0) * i / n)
        x = cx + math.cos(a) * r
        y = cy + math.sin(a) * r
        x2 = cx + math.cos(a) * r * 0.32
        y2 = cy + math.sin(a) * r * 0.32
        d_a.line([x2 * S, y2 * S, x * S, y * S], fill=col(BRASS, 170), width=S)
        d_h.line([x2 * S, y2 * S, x * S, y * S], fill=238, width=S)
    for f in (1.0, 0.32):
        box = [(cx - r * f) * S, (cy - r * f) * S, (cx + r * f) * S, (cy + r * f) * S]
        d_a.arc(box, ang0, ang1, fill=col(BRASS, 190), width=S)
        d_h.arc(box, ang0, ang1, fill=238, width=S)


# Cartouche above the rooms (the title is a Label over this).
panel(66, 72, 544, 142)
fan(92, 142, 58, 270, 360, 8)
fan(518, 142, 58, 180, 270, 8)

# The long machinery window: gears turn behind this frame. Matches Clockwork.WINDOW.
MW = (70, 518, 1210, 596)
d_a.rectangle([MW[0] * S, MW[1] * S, MW[2] * S, MW[3] * S], fill=col(hex3("#0c0a09")))
d_a.rectangle([(MW[0] - 3) * S, (MW[1] - 3) * S, (MW[2] + 3) * S, (MW[3] + 3) * S], outline=col(BRASS_D), width=3 * S)
d_h.rectangle([MW[0] * S, MW[1] * S, MW[2] * S, MW[3] * S], fill=90)
d_h.rectangle([(MW[0] - 3) * S, (MW[1] - 3) * S, (MW[2] + 3) * S, (MW[3] + 3) * S], outline=240, width=3 * S)
# Plaque below the exit chamber.
panel(1132, 398, 1218, 490)
fan(1175, 490, 38, 180, 360, 12)
for i in range(2):
    y = 414 + i * 14
    d_a.line([1152 * S, y * S, 1198 * S, y * S], fill=col(BRASS, 150), width=S)
    d_h.line([1152 * S, y * S, 1198 * S, y * S], fill=235, width=S)
# Fans flanking the dial.
fan(690, 480, 62, 270, 360, 8)
fan(1016, 480, 62, 180, 270, 8)

im = Image.composite(im, im, Image.new("L", im.size, 255))

albedo_final = np.asarray(im, dtype=np.float32) / 255

# ------------------------------------------------------------ normal map
hh = np.asarray(hm, dtype=np.float32) / 255
hh = ndi.gaussian_filter(hh, 0.8)
grain_detail = (noise(10, 2, 81) - 0.5) * 0.012
hh = hh + grain_detail
gx_ = ndi.sobel(hh, axis=1) / 8.0
gy_ = ndi.sobel(hh, axis=0) / 8.0
k = 9.0
nx, ny, nz = -gx_ * k, -gy_ * k, np.ones_like(hh)
ln = np.sqrt(nx * nx + ny * ny + nz * nz)
normal = np.stack([nx / ln, -ny / ln * -1, nz / ln], axis=-1) * 0.5 + 0.5  # Godot: y-down texture space

# Alpha carries the floor mask: the shaders keep their fine lines on walkable surfaces.
alpha = np.where(floor_mask, 1.0, 0.5)[..., None]
Image.fromarray((np.clip(np.concatenate([albedo_final, alpha], axis=2), 0, 1) * 255).astype(np.uint8), "RGBA").save(OUT / "map_albedo.png", optimize=True)
Image.fromarray((np.clip(normal, 0, 1) * 255).astype(np.uint8), "RGB").resize((1280, 720), Image.LANCZOS).save(OUT / "map_normal.png", optimize=True)
print("map done")

# ---------------------------------------------------------------- sprites


def gear_path(cx, cy, r_out, r_in, teeth, tip=0.42, root=0.58):
    pts = []
    for i in range(teeth):
        a0 = 2 * math.pi * i / teeth
        step = 2 * math.pi / teeth
        for r, f in ((r_in, 0.0), (r_out, (1 - tip) / 2), (r_out, (1 + tip) / 2), (r_in, 1.0 - 0.0)):
            a = a0 + f * step
            pts.append((cx + r * math.cos(a), cy + r * math.sin(a)))
    return "M" + " L".join(f"{x:.2f},{y:.2f}" for x, y in pts) + "Z"


def saw_path(cx, cy, r_out, r_in, teeth, lean=0.55):
    """Raked saw teeth, so spin direction reads even in a still frame."""
    pts = []
    step = 2 * math.pi / teeth
    for i in range(teeth):
        a0 = step * i
        pts.append((cx + r_in * math.cos(a0), cy + r_in * math.sin(a0)))
        a1 = a0 + step * lean
        pts.append((cx + r_out * math.cos(a1), cy + r_out * math.sin(a1)))
        a2 = a0 + step * (lean + 0.08)
        pts.append((cx + r_out * 0.97 * math.cos(a2), cy + r_out * 0.97 * math.sin(a2)))
    return "M" + " L".join(f"{x:.2f},{y:.2f}" for x, y in pts) + "Z"


def write_svg(name, size, body, px=None):
    px = px or size
    svg = f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {size} {size}" width="{px}" height="{px}">{body}</svg>'
    path = SVG / f"{name}.svg"
    path.write_text(svg)
    subprocess.run(["rsvg-convert", "-w", str(px), "-h", str(px), str(path), "-o", str(OUT / f"{name}.png")], check=True)


# Hazard: 64-unit box, tooth tips at radius 28, as the collision description says.
GRAD = '''<defs>
<radialGradient id="brass" cx="42%" cy="36%" r="72%"><stop offset="0" stop-color="#f6e2a6"/><stop offset=".5" stop-color="#cfa44f"/><stop offset="1" stop-color="#7a5a26"/></radialGradient>
<radialGradient id="lacquer" cx="40%" cy="34%" r="80%"><stop offset="0" stop-color="#b53a2f"/><stop offset=".6" stop-color="#7c1d1b"/><stop offset="1" stop-color="#3f0d0e"/></radialGradient>
<radialGradient id="steel" cx="40%" cy="34%" r="80%"><stop offset="0" stop-color="#55595e"/><stop offset="1" stop-color="#17191c"/></radialGradient>
<radialGradient id="ivory" cx="40%" cy="32%" r="75%"><stop offset="0" stop-color="#fffaf0"/><stop offset=".65" stop-color="#ece1c6"/><stop offset="1" stop-color="#b9a981"/></radialGradient>
</defs>'''
c = 32
body = GRAD
body += f'<path d="{saw_path(c, c, 28.0, 21.5, 14)}" fill="url(#brass)" stroke="#120d09" stroke-width="1.1" stroke-linejoin="round"/>'
body += f'<circle cx="{c}" cy="{c}" r="19.4" fill="url(#lacquer)" stroke="#120d09" stroke-width="1"/>'
for i in range(6):
    a = 2 * math.pi * i / 6
    x, y = c + 13.2 * math.cos(a), c + 13.2 * math.sin(a)
    body += f'<circle cx="{x:.2f}" cy="{y:.2f}" r="3.6" fill="#180806" stroke="#d9b866" stroke-width=".7"/>'
write_svg("hazard_wheel", 64, body, 256)

body = GRAD
body += f'<circle cx="{c}" cy="{c}" r="8.6" fill="url(#brass)" stroke="#120d09" stroke-width="1"/>'
body += f'<circle cx="{c}" cy="{c}" r="4.2" fill="#180806"/>'
body += f'<rect x="{c-0.9}" y="{c-7.6}" width="1.8" height="6" fill="#fff4cf"/>'
write_svg("hazard_hub", 64, body, 256)

# Player: seen from above. An ivory bowler, a dark band, a hint of shoulders. r = 13 in a 64 box.
body = GRAD
body += f'<ellipse cx="{c-1}" cy="{c}" rx="9.6" ry="13" fill="#7f97a8" stroke="#0a0908" stroke-width="1.3"/>'  # shoulders, facing +x
body += f'<circle cx="{c}" cy="{c}" r="10.2" fill="url(#ivory)" stroke="#0a0908" stroke-width="1.2"/>'  # brim
body += f'<circle cx="{c}" cy="{c}" r="6.5" fill="#16100c"/>'  # band
body += f'<circle cx="{c-0.3}" cy="{c-0.3}" r="5.4" fill="url(#ivory)"/>'  # crown
write_svg("player_body", 64, body, 256)

body = '<defs><radialGradient id="g" cx="50%" cy="50%" r="50%"><stop offset="0" stop-color="#fffbe8"/><stop offset=".55" stop-color="#ffd27a"/><stop offset="1" stop-color="#b87a22"/></radialGradient></defs>'
body += f'<rect x="{c-3.2}" y="{c-3.6}" width="6.4" height="7.2" rx="1.6" fill="url(#g)" stroke="#2a1a08" stroke-width="1"/>'
write_svg("player_lantern", 64, body, 128)

# Vault door: 128-unit box, a brass ring with locking bolts around a bright mouth.
c2 = 64
body = GRAD
for i in range(12):
    a = 2 * math.pi * i / 12
    x, y = c2 + 53 * math.cos(a), c2 + 53 * math.sin(a)
    deg = math.degrees(a)
    body += f'<rect x="{x-6:.2f}" y="{y-3.4:.2f}" width="12" height="6.8" rx="1" fill="url(#brass)" stroke="#15100a" stroke-width="1" transform="rotate({deg:.2f} {x:.2f} {y:.2f})"/>'
body += f'<circle cx="{c2}" cy="{c2}" r="50" fill="none" stroke="#15100a" stroke-width="10.5"/>'
body += f'<circle cx="{c2}" cy="{c2}" r="50" fill="none" stroke="url(#brass)" stroke-width="8"/>'
for i in range(36):
    a = 2 * math.pi * i / 36
    x0, y0 = c2 + 47 * math.cos(a), c2 + 47 * math.sin(a)
    x1, y1 = c2 + 53 * math.cos(a), c2 + 53 * math.sin(a)
    body += f'<line x1="{x0:.2f}" y1="{y0:.2f}" x2="{x1:.2f}" y2="{y1:.2f}" stroke="#5e4620" stroke-width=".8"/>'
write_svg("vault_ring", 128, body, 384)

# Dial: walnut-mounted enamel clock, 256-unit box, the wall's set-piece.
c3 = 128
body = GRAD + '<defs><radialGradient id="enamel" cx="46%" cy="40%" r="72%"><stop offset="0" stop-color="#1f3a39"/><stop offset=".7" stop-color="#132524"/><stop offset="1" stop-color="#0a1414"/></radialGradient></defs>'
body += f'<circle cx="{c3}" cy="{c3}" r="121" fill="#0d0a08"/>'
body += f'<circle cx="{c3}" cy="{c3}" r="116" fill="none" stroke="url(#brass)" stroke-width="7"/>'
body += f'<circle cx="{c3}" cy="{c3}" r="110" fill="url(#enamel)" stroke="#0d0a08" stroke-width="2"/>'
body += f'<circle cx="{c3}" cy="{c3}" r="104" fill="none" stroke="#c49a45" stroke-width=".8" opacity=".8"/>'
for i in range(60):
    a = 2 * math.pi * i / 60
    major = i % 5 == 0
    r0 = 90 if major else 98
    x0, y0 = c3 + r0 * math.cos(a), c3 + r0 * math.sin(a)
    x1, y1 = c3 + 103 * math.cos(a), c3 + 103 * math.sin(a)
    body += f'<line x1="{x0:.2f}" y1="{y0:.2f}" x2="{x1:.2f}" y2="{y1:.2f}" stroke="#d8b768" stroke-width="{2.4 if major else 0.9}" opacity="{1 if major else .6}"/>'


def roman(s, cx, cy, deg, h=13.0):
    """Roman numerals are only strokes, so they need no font."""
    out = ""
    w = {"I": 3.6, "V": 8.5, "X": 8.5}
    total = sum(w[ch] for ch in s)
    x = -total / 2
    for ch in s:
        if ch == "I":
            out += f'<line x1="{x+1.8}" y1="{-h/2}" x2="{x+1.8}" y2="{h/2}"/>'
        elif ch == "V":
            out += f'<polyline fill="none" points="{x},{-h/2} {x+4.2},{h/2} {x+8.5},{-h/2}"/>'
        elif ch == "X":
            out += f'<line x1="{x}" y1="{-h/2}" x2="{x+8.5}" y2="{h/2}"/><line x1="{x+8.5}" y1="{-h/2}" x2="{x}" y2="{h/2}"/>'
        x += w[ch]
    return f'<g transform="translate({cx:.2f} {cy:.2f}) rotate({deg:.2f})" stroke="#e8d7a6" stroke-width="1.7" stroke-linecap="square">{out}</g>'


NUM = ["XII", "I", "II", "III", "IIII", "V", "VI", "VII", "VIII", "IX", "X", "XI"]
for i, s in enumerate(NUM):
    deg = 30 * i
    a = math.radians(deg - 90)
    body += roman(s, c3 + 74 * math.cos(a), c3 + 74 * math.sin(a), deg)
# inner chapter ring with sunburst guilloche
for i in range(72):
    a = 2 * math.pi * i / 72
    x0, y0 = c3 + 22 * math.cos(a), c3 + 22 * math.sin(a)
    x1, y1 = c3 + 58 * math.cos(a), c3 + 58 * math.sin(a)
    body += f'<line x1="{x0:.2f}" y1="{y0:.2f}" x2="{x1:.2f}" y2="{y1:.2f}" stroke="#2f5451" stroke-width=".7"/>'
body += f'<circle cx="{c3}" cy="{c3}" r="58" fill="none" stroke="#c49a45" stroke-width="1" opacity=".7"/>'
write_svg("dial_face", 256, body, 640)

# Clock hands point up (-y) from the box centre.
body = GRAD
body += f'<path d="M{c3-3.5},{c3+18} L{c3-2},{c3-86} L{c3},{c3-96} L{c3+2},{c3-86} L{c3+3.5},{c3+18}Z" fill="url(#brass)" stroke="#0d0a08" stroke-width="1"/>'
write_svg("dial_minute", 256, body, 512)
body = GRAD
body += f'<path d="M{c3-5},{c3+14} L{c3-3},{c3-40} L{c3-8},{c3-48} L{c3},{c3-62} L{c3+8},{c3-48} L{c3+3},{c3-40} L{c3+5},{c3+14}Z" fill="url(#brass)" stroke="#0d0a08" stroke-width="1"/>'
body += f'<circle cx="{c3}" cy="{c3}" r="7" fill="url(#brass)" stroke="#0d0a08" stroke-width="1"/><circle cx="{c3}" cy="{c3}" r="2.6" fill="#0d0a08"/>'
write_svg("dial_hour", 256, body, 512)


# Machinery gears, three sizes, drawn at the size they are used.
def gear_svg(name, teeth, r_out, spokes=5):
    box = int(r_out * 2 + 8)
    cc = box / 2
    r_in = r_out - max(5.0, r_out * 0.13)
    b = GRAD
    b += f'<path d="{gear_path(cc, cc, r_out, r_in, teeth)}" fill="url(#brass)" stroke="#120d09" stroke-width="1"/>'
    b += f'<circle cx="{cc}" cy="{cc}" r="{r_in*0.78:.2f}" fill="#120d09"/>'
    for i in range(spokes):
        deg = 360 * i / spokes
        b += f'<rect x="{cc-3.2}" y="{cc-r_in*0.82:.2f}" width="6.4" height="{r_in*0.82:.2f}" fill="url(#brass)" transform="rotate({deg} {cc} {cc})"/>'
    b += f'<circle cx="{cc}" cy="{cc}" r="{max(5.5, r_in*0.2):.2f}" fill="url(#brass)" stroke="#120d09" stroke-width="1"/>'
    b += f'<circle cx="{cc}" cy="{cc}" r="2.2" fill="#120d09"/>'
    write_svg(name, box, b, box * 2)


gear_svg("gear_large", 32, 66.0, 6)
gear_svg("gear_medium", 20, 42.0, 5)
gear_svg("gear_small", 12, 26.0, 4)

# Project icon.
body = GRAD + f'<rect width="64" height="64" rx="10" fill="#100d0b"/><path d="{saw_path(c, c, 28.0, 21.5, 14)}" fill="url(#brass)" stroke="#120d09" stroke-width="1"/><circle cx="{c}" cy="{c}" r="19.4" fill="url(#lacquer)"/><circle cx="{c}" cy="{c}" r="9.6" fill="url(#ivory)" stroke="#0d0b0a" stroke-width="1.1"/><circle cx="{c}" cy="{c}" r="6.1" fill="#16100c"/><circle cx="{c}" cy="{c}" r="5.1" fill="url(#ivory)"/>'
write_svg("icon", 64, body, 256)

# Soft radial for lights.
yy2, xx2 = np.mgrid[-1:1:512j, -1:1:512j]
r = np.clip(1 - np.hypot(xx2, yy2), 0, 1)
soft = (r**2 * (3 - 2 * r)) ** 1.3
Image.fromarray((np.dstack([np.ones_like(soft), np.ones_like(soft), np.ones_like(soft), soft]) * 255).astype(np.uint8), "RGBA").save(OUT / "soft_light.png", optimize=True)
print("sprites done")

# Glass over the machinery window: inner shadow and one diagonal sheen.
gw, gh = 1140, 78
gy, gx = np.mgrid[0:gh, 0:gw].astype(np.float32)
edge = np.minimum.reduce([gx, gw - 1 - gx, gy, gh - 1 - gy])
inner_shadow = np.clip(1 - edge / 22.0, 0, 1) ** 1.6 * 0.85
rgba = np.zeros((gh, gw, 4), dtype=np.float32)
rgba[..., 0:3] = 0.015
rgba[..., 3] = inner_shadow
Image.fromarray((rgba * 255).astype(np.uint8), "RGBA").save(OUT / "window_glass.png", optimize=True)
print("glass done")

# Soft strip for the repayment trail: bright along the middle, nothing at the edges.
ty = np.linspace(-1, 1, 64)
profile = np.clip(1 - np.abs(ty), 0, 1) ** 1.8
strip = np.ones((64, 16, 4), dtype=np.float32)
strip[..., 3] = profile[:, None]
Image.fromarray((strip * 255).astype(np.uint8), "RGBA").save(OUT / "trail_soft.png", optimize=True)
print("trail done")
