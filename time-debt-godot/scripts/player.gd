class_name Player
extends CharacterBody2D
## The player's collision body. Each axis is resolved separately so the player
## slides along walls, matching the Haskell prototype. Speed never reads the
## temporal mode: there is no code path by which it could.

var speed: float = 3.25
var last_move: Vector2i = Vector2i.ZERO

@onready var collider: CollisionShape2D = $Collider


func configure(config: GameplayConfig) -> void:
	speed = config.player_speed
	(collider.shape as CircleShape2D).radius = config.player_radius


func step_move(move: Vector2i) -> void:
	last_move = move
	var delta: Vector2 = move_delta(move, speed)
	if delta.x != 0.0:
		move_and_collide(Vector2(delta.x, 0.0), false, 0.01)
	if delta.y != 0.0:
		move_and_collide(Vector2(0.0, delta.y), false, 0.01)


## Normalised, so diagonals are not faster.
static func move_delta(move: Vector2i, p_speed: float) -> Vector2:
	var direction := Vector2(move)
	if direction == Vector2.ZERO:
		return Vector2.ZERO
	return direction.normalized() * p_speed
