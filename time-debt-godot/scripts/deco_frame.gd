class_name DecoFrame
extends Control
## The brass double frame with stepped corners used by every screen in the game.

const INK := Color(0.055, 0.044, 0.038, 0.97)
const BRASS := Color(0.77, 0.6, 0.27)
const BRASS_DIM := Color(0.42, 0.31, 0.13)

@export var step: float = 16.0


func _ready() -> void:
	resized.connect(queue_redraw)


func _draw() -> void:
	var r := Rect2(Vector2.ZERO, size)
	draw_colored_polygon(_stepped(r, step), INK)
	_outline(_stepped(r, step), BRASS, 2.0)
	_outline(_stepped(r.grow(-7.0), step * 0.75), BRASS_DIM, 1.0)


func _outline(points: PackedVector2Array, color: Color, width: float) -> void:
	var closed: PackedVector2Array = points.duplicate()
	closed.append(points[0])
	draw_polyline(closed, color, width, true)


static func _stepped(r: Rect2, s: float) -> PackedVector2Array:
	var a: Vector2 = r.position
	var b: Vector2 = r.end
	return PackedVector2Array([
		Vector2(a.x + s, a.y), Vector2(b.x - s, a.y), Vector2(b.x - s, a.y + s), Vector2(b.x, a.y + s),
		Vector2(b.x, b.y - s), Vector2(b.x - s, b.y - s), Vector2(b.x - s, b.y), Vector2(a.x + s, b.y),
		Vector2(a.x + s, b.y - s), Vector2(a.x, b.y - s), Vector2(a.x, a.y + s), Vector2(a.x + s, a.y + s),
	])
