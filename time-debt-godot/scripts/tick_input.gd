class_name TickInput
extends RefCounted
## Everything one simulation tick needs from the outside world.

var move: Vector2i = Vector2i.ZERO      ## each axis -1, 0 or 1; y down
var freeze_held: bool = false
var freeze_pressed: bool = false        ## key-down edge, true for one tick only
var pause_pressed: bool = false
var restart_pressed: bool = false
var confirm_pressed: bool = false


static func make(p_move: Vector2i = Vector2i.ZERO, p_held: bool = false, p_pressed: bool = false) -> TickInput:
	var input := TickInput.new()
	input.move = p_move
	input.freeze_held = p_held
	input.freeze_pressed = p_pressed
	return input
