class_name LevelLayout
extends RefCounted
## Level geometry, read from resources/level_layout.json and translated from the
## Haskell prototype's coordinates (centre origin, y up) to Godot's (top-left, y down).

const PATH: String = "res://resources/level_layout.json"

var start: Vector2
var start_area: Rect2
var safe_area: Rect2
var refuge: Rect2
var exit_rect: Rect2
var walls: Array[Rect2] = []
var floors: Dictionary[String, Rect2] = {}
var routes: Dictionary[String, PackedVector2Array] = {}


static func load_default() -> LevelLayout:
	var data: Dictionary = JSON.parse_string(FileAccess.get_file_as_string(PATH))
	var layout := LevelLayout.new()
	layout.start = _point(data["start"])
	layout.start_area = _rect(data["start_area"])
	layout.safe_area = _rect(data["safe_area"])
	layout.refuge = _rect(data["refuge"])
	layout.exit_rect = _rect(data["exit"])
	for wall: Array in data["walls"]:
		layout.walls.append(_rect(wall))
	var floor_data: Dictionary = data["floors"]
	for key: String in floor_data:
		layout.floors[key] = _rect(floor_data[key])
	var route_data: Dictionary = data["routes"]
	for key: String in route_data:
		var points := PackedVector2Array()
		for point: Array in route_data[key]:
			points.append(_point(point))
		layout.routes[key] = points
	return layout


static func _point(p: Array) -> Vector2:
	return Vector2(float(p[0]) + 640.0, 360.0 - float(p[1]))


## Gloss rects are [left, bottom, right, top].
static func _rect(r: Array) -> Rect2:
	return Rect2(float(r[0]) + 640.0, 360.0 - float(r[3]), float(r[2]) - float(r[0]), float(r[3]) - float(r[1]))
