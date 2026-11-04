class_name Hazard
extends Node2D
## A hazard's position and spin are read from the shared hazard clock. Collision
## uses `route` analytically; the children only decide how that looks, so spin and
## trail stop dead and race exactly as the wheel itself does.

const SPIN_PER_TICK: float = TAU / 90.0
const TRAIL_SAMPLES: int = 12
const AURA_RADIUS: float = 30.0
const COLD := Color(0.72, 0.94, 1.0)
const HOT := Color(1.0, 0.72, 0.4)

var route: HazardRoute
var clock: int = 0
var trail_enabled: bool = true
var _mode: TimeDebt.Mode = TimeDebt.Mode.NORMAL
var _trail: PackedVector2Array = PackedVector2Array()

@onready var wheel: Sprite2D = $Wheel
@onready var hub: Sprite2D = $Hub
@onready var trail: Line2D = $Trail
@onready var sparks: GPUParticles2D = $Sparks
@onready var glow: PointLight2D = $Glow


func configure(p_route: HazardRoute) -> void:
	route = p_route
	set_clock(0)


func set_clock(p_clock: int) -> void:
	clock = p_clock
	position = route.position_at(clock)
	wheel.rotation = float(clock) * SPIN_PER_TICK
	hub.rotation = -float(clock) * SPIN_PER_TICK * 0.5
	_update_trail()


func set_mode_visual(mode: TimeDebt.Mode) -> void:
	_mode = mode
	wheel.modulate = Color(0.92, 1.0, 1.12) if mode == TimeDebt.Mode.FROZEN else Color.WHITE
	sparks.emitting = trail_enabled and mode == TimeDebt.Mode.REPAYING
	queue_redraw()


func set_effects_enabled(enabled: bool) -> void:
	trail_enabled = enabled
	glow.visible = enabled
	if not enabled:
		_trail.clear()
		trail.clear_points()
	sparks.emitting = enabled and _mode == TimeDebt.Mode.REPAYING


func clear_transients() -> void:
	_trail.clear()
	trail.clear_points()
	sparks.emitting = false
	sparks.restart()
	sparks.emitting = false


## Bounded history of real positions while repaying, shrinking away otherwise.
func _update_trail() -> void:
	if not trail_enabled:
		return
	if _mode == TimeDebt.Mode.REPAYING:
		_trail.append(position)
		if _trail.size() > TRAIL_SAMPLES:
			_trail.remove_at(0)
	elif _trail.size() > 0 and _mode != TimeDebt.Mode.FROZEN:
		_trail.remove_at(0)
	trail.points = _trail


## A hairline just outside the tooth tips: the frozen wheel must still say "danger".
func _draw() -> void:
	if _mode == TimeDebt.Mode.FROZEN:
		draw_arc(Vector2.ZERO, AURA_RADIUS, 0.0, TAU, 56, COLD, 1.6, true)
	elif _mode == TimeDebt.Mode.REPAYING:
		draw_arc(Vector2.ZERO, AURA_RADIUS, 0.0, TAU, 56, HOT, 2.0, true)
