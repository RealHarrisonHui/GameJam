class_name Clockwork
extends Node2D
## The bank's machinery: the wall dial and the gear train behind the long window.
## Every angle is a pure function of the hazard clock, so the building visibly stops
## when the wheels stop and races when they race.

const HAND_TICKS_PER_TURN: float = 180.0
const GEAR_TICKS_PER_TURN: float = 360.0
const WINDOW := Rect2(70, 518, 1140, 78)        ## matches the frame baked by tools/build_art.py
const MESH_OVERLAP: float = 5.0

## A straight train seen through a slot. name, pitch radius, teeth.
const SMALL: Array = ["gear_small", 26.0, 12]
const MEDIUM: Array = ["gear_medium", 42.0, 20]
const LARGE: Array = ["gear_large", 66.0, 32]
const SEQUENCE: Array = [MEDIUM, LARGE, SMALL, MEDIUM, SMALL, LARGE, MEDIUM, SMALL, LARGE, SMALL, MEDIUM, LARGE, SMALL, MEDIUM, LARGE, MEDIUM]

var _gears: Array[Sprite2D] = []
var _rates: PackedFloat32Array = PackedFloat32Array()

@onready var minute_hand: Sprite2D = $Dial/Minute
@onready var hour_hand: Sprite2D = $Dial/Hour
@onready var train: Control = $Gears


func _ready() -> void:
	train.position = WINDOW.position
	train.size = WINDOW.size
	($Glass as Sprite2D).position = WINDOW.get_center()
	var material_gears: Material = ($Dial/Face as Sprite2D).material
	var direction: float = 1.0
	var x: float = 10.0
	var previous_radius: float = 0.0
	for entry: Array in SEQUENCE:
		var radius: float = entry[1]
		if previous_radius > 0.0:
			x += previous_radius + radius - MESH_OVERLAP
		previous_radius = radius
		var gear := Sprite2D.new()
		gear.texture = load("res://assets/art/%s.png" % entry[0])
		gear.scale = Vector2(0.5, 0.5)
		gear.position = Vector2(x, WINDOW.size.y / 2.0)
		gear.modulate = Color(0.52, 0.5, 0.47)
		gear.material = material_gears
		train.add_child(gear)
		_gears.append(gear)
		_rates.append(direction * TAU / GEAR_TICKS_PER_TURN * 32.0 / float(entry[2]))
		direction = -direction
	set_clock(0)


func set_clock(clock: int) -> void:
	var c: float = float(clock)
	minute_hand.rotation = c * TAU / HAND_TICKS_PER_TURN
	hour_hand.rotation = c * TAU / (HAND_TICKS_PER_TURN * 12.0)
	for i: int in range(_gears.size()):
		_gears[i].rotation = c * _rates[i]
