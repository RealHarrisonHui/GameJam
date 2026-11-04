class_name PlayerVisual
extends Node2D
## How the player looks. Presentation only: it reads the body's last move and the
## debt, and the collision body never reads anything from here.

const HALO_RADIUS: float = 22.0
const TURN_RATE: float = 14.0
const COLD := Color(0.62, 0.9, 1.0)
const HOT := Color(1.0, 0.36, 0.25)

var debt_fraction: float = 0.0
var mode: TimeDebt.Mode = TimeDebt.Mode.NORMAL
var pclock: float = 0.0

@onready var body: Sprite2D = $Body
@onready var lantern: Sprite2D = $Body/Lantern
@onready var light: PointLight2D = $LanternLight
@onready var player: Player = get_parent()


func _process(delta: float) -> void:
	pclock += delta
	var move: Vector2i = player.last_move
	if move != Vector2i.ZERO:
		var target: float = Vector2(move).angle()
		body.rotation = lerp_angle(body.rotation, target, clampf(TURN_RATE * delta, 0.0, 1.0))
	lantern.scale = Vector2.ONE * (1.0 + 0.05 * sin(pclock * 9.0) * sin(pclock * 3.7))
	light.energy = light_energy_base() * (1.0 + 0.045 * sin(pclock * 11.0) * sin(pclock * 2.9))


func light_energy_base() -> float:
	return 1.2


func set_debt(fraction: float, p_mode: TimeDebt.Mode) -> void:
	if fraction == debt_fraction and p_mode == mode:
		return
	debt_fraction = fraction
	mode = p_mode
	queue_redraw()


## Debt ring around the player, so it can be read without looking at the HUD.
func _draw() -> void:
	if debt_fraction <= 0.0:
		return
	var color: Color = COLD if mode == TimeDebt.Mode.FROZEN else HOT
	var start: float = -PI / 2.0
	draw_arc(Vector2.ZERO, HALO_RADIUS, 0.0, TAU, 48, Color(0.04, 0.03, 0.03, 0.55), 5.0, true)
	draw_arc(Vector2.ZERO, HALO_RADIUS, start, start + TAU * debt_fraction, 48, color, 3.2, true)
	for i: int in range(3):
		var a: float = start + TAU * float(i) / 3.0
		draw_line(Vector2.from_angle(a) * (HALO_RADIUS - 3.0), Vector2.from_angle(a) * (HALO_RADIUS + 3.0), Color(0.05, 0.04, 0.04), 1.4, true)
