class_name InputState
extends RefCounted
## Turns key events into per-tick input. Held state comes only from press and
## release events, never from polling, so clearing it makes a key that is still
## physically down count as up until it is pressed again. OS key repeat is ignored.

const MOVE_ACTIONS: PackedStringArray = ["move_left", "move_right", "move_up", "move_down"]

var _held: Dictionary[String, bool] = {}
var _freeze_pressed: bool = false
var _pause_pressed: bool = false
var _restart_pressed: bool = false
var _confirm_pressed: bool = false


func handle_event(event: InputEvent) -> void:
	if event is InputEventKey and (event as InputEventKey).echo:
		return
	for action: String in MOVE_ACTIONS:
		if event.is_action_pressed(action):
			_held[action] = true
		elif event.is_action_released(action):
			_held.erase(action)
	if event.is_action_pressed("freeze"):
		_held["freeze"] = true
		_freeze_pressed = true
	elif event.is_action_released("freeze"):
		_held.erase("freeze")
	if event.is_action_pressed("pause"):
		_pause_pressed = true
	if event.is_action_pressed("restart"):
		_restart_pressed = true
	if event.is_action_pressed("confirm"):
		_confirm_pressed = true


## A click on a screen's button arrives as the same one-shot as the Enter key.
func request_confirm() -> void:
	_confirm_pressed = true


func sample() -> TickInput:
	var input := TickInput.new()
	input.move = Vector2i(_axis("move_right") - _axis("move_left"), _axis("move_down") - _axis("move_up"))
	input.freeze_held = _held.has("freeze")
	input.freeze_pressed = _freeze_pressed
	input.pause_pressed = _pause_pressed
	input.restart_pressed = _restart_pressed
	input.confirm_pressed = _confirm_pressed
	return input


## Called after every tick, so a press reaches exactly one tick.
func consume_one_shots() -> void:
	_freeze_pressed = false
	_pause_pressed = false
	_restart_pressed = false
	_confirm_pressed = false


## Forget held keys. A key still physically down must be pressed again.
func clear() -> void:
	_held.clear()
	consume_one_shots()


func _axis(action: String) -> int:
	return 1 if _held.has(action) else 0
