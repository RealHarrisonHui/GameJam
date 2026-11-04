class_name ExitDoor
extends Node2D
## The open vault. Decorative animation runs on the presentation clock from effects.gd.

@onready var ring: Sprite2D = $Ring
@onready var mouth: Sprite2D = $Mouth

var _open: float = 0.0
var _spin: float = 0.05


func animate(pclock: float, delta: float) -> void:
	ring.rotation += _spin * delta
	(mouth.material as ShaderMaterial).set_shader_parameter("pclock", pclock)
	(mouth.material as ShaderMaterial).set_shader_parameter("open", _open)
	mouth.scale = Vector2.ONE * (0.38 + 0.012 * sin(pclock * 1.6) + _open * 0.3)


func set_opened(opened: bool) -> void:
	_open = 1.0 if opened else 0.0
	_spin = 1.4 if opened else 0.05
