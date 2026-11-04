class_name HazardRoute
extends RefCounted
## A polyline travelled there and back at constant speed. Position is only a
## function of the clock, so hazards hold no state that could drift.

var points: PackedVector2Array
var one_way: int
var phase: int
var length: float = 0.0


func _init(p_points: PackedVector2Array, p_one_way: int, p_phase: int) -> void:
	points = p_points
	one_way = p_one_way
	phase = p_phase
	for i: int in range(1, points.size()):
		length += points[i - 1].distance_to(points[i])


func period() -> int:
	return 2 * one_way


## Distance along the route at this hazard tick.
func travel_at(tick: int) -> float:
	var t: int = posmod(tick + phase, period())
	var leg: int = t if t <= one_way else period() - t
	return length * float(leg) / float(one_way)


func position_at(tick: int) -> Vector2:
	var d: float = travel_at(tick)
	for i: int in range(1, points.size()):
		var segment: float = points[i - 1].distance_to(points[i])
		if d <= segment:
			return points[i - 1].lerp(points[i], d / segment)
		d -= segment
	return points[points.size() - 1]


## First-contact test while the clock runs c0..c1 and the player moves p0..p1.
## Each clock tick is one linear substep and swept_contact covers the relative
## motion inside it, so at double speed nothing can pass between samples.
func hits_player(reach: float, c0: int, c1: int, p0: Vector2, p1: Vector2) -> bool:
	var steps: int = maxi(1, c1 - c0)
	for k: int in range(1, steps + 1):
		var pa: Vector2 = p0.lerp(p1, float(k - 1) / float(steps))
		var pb: Vector2 = p0.lerp(p1, float(k) / float(steps))
		var ha: Vector2 = position_at(mini(c1, c0 + k - 1))
		var hb: Vector2 = position_at(mini(c1, c0 + k))
		if swept_contact(reach, pa, pb, ha, hb):
			return true
	return false


## Do two circles of summed radius r touch at any instant while both move
## linearly? In the hazard's frame this is one point-to-segment distance test.
static func swept_contact(r: float, p0: Vector2, p1: Vector2, h0: Vector2, h1: Vector2) -> bool:
	var a: Vector2 = p0 - h0
	var b: Vector2 = p1 - h1
	var d: Vector2 = b - a
	var len_sq: float = d.length_squared()
	var u: float = 0.0
	if len_sq > 0.0:
		u = clampf(-a.dot(d) / len_sq, 0.0, 1.0)
	return a.lerp(b, u).length() <= r
