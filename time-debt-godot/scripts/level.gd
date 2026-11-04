class_name Level
extends Node2D
## Collision and light occlusion for the level, built from LevelLayout so that
## geometry has a single source. Everything drawn lives in the child art nodes.

var layout: LevelLayout


func _ready() -> void:
	layout = LevelLayout.load_default()
	var body := StaticBody2D.new()
	body.name = "Walls"
	add_child(body)
	for wall: Rect2 in layout.walls:
		var shape := RectangleShape2D.new()
		shape.size = wall.size
		var collider := CollisionShape2D.new()
		collider.shape = shape
		collider.position = wall.get_center()
		body.add_child(collider)
		var polygon := OccluderPolygon2D.new()
		polygon.polygon = PackedVector2Array([wall.position, Vector2(wall.end.x, wall.position.y), wall.end, Vector2(wall.position.x, wall.end.y)])
		var occluder := LightOccluder2D.new()
		occluder.occluder = polygon
		add_child(occluder)
