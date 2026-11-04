extends SceneTree
## Plays the intended route with a small state-machine policy through the real
## Game.tick and records the inputs as run-length segments in tests/solution.json.
## The policy only reads what a player can see. tests/test_runner.gd replays the file.
##   godot --headless --path . --script res://tools/derive_solution.gd

const OUT_PATH: String = "res://tests/solution.json"

var game: Game
var segments: Array = []
var was_held: bool = false


func _initialize() -> void:
	_run()


func _do(move: Vector2i, held: bool) -> void:
	game.tick(TickInput.make(move, held, held and not was_held))
	was_held = held
	if segments.size() > 0:
		var last: Array = segments[-1]
		if last[1] == move.x and last[2] == move.y and last[3] == int(held):
			last[0] += 1
			return
	segments.append([1, move.x, move.y, int(held)])


func _run() -> void:
	game = (load("res://scenes/main.tscn") as PackedScene).instantiate()
	root.add_child(game)
	game.set_physics_process(false)
	await physics_frame
	await physics_frame
	var confirm := TickInput.new()
	confirm.confirm_pressed = true
	game.tick(confirm)
	var player: Player = game.player
	var gallery: Hazard = game.hazards[1]

	# First crossing: walk up to the door, stop the wheel at the top of its slot, cross.
	while player.global_position.x < 250.0:
		_do(Vector2i(1, 0), false)
	var shuttle: Hazard = game.hazards[0]
	while shuttle.position.y > 246.0 and shuttle.position.y < 334.0:
		_do(Vector2i.ZERO, false)
	while player.global_position.x < 375.0:
		_do(Vector2i(1, 0), true)
	# Grace room: let go, walk to the gallery door while the debt clears.
	while player.global_position.y > 175.0:
		_do(Vector2i(1, -1), false)
	while player.global_position.x < 625.0:
		_do(Vector2i(1, 0), false)
	while player.global_position.y > 142.0:
		_do(Vector2i(0, -1), false)
	# Second crossing: wait for the wheel at the far end of the gallery, then borrow.
	while game.time.mode != TimeDebt.Mode.NORMAL or gallery.position.x < 1195.0:
		_do(Vector2i.ZERO, false)
	_do(Vector2i(0, -1), true)
	while player.global_position.y > 125.0:
		_do(Vector2i(0, -1), true)
	while player.global_position.x < 1064.0:
		_do(Vector2i(1, 0), true)
	while player.global_position.y < 150.0:
		_do(Vector2i(0, 1), true)
	# Alcove: release, step back, and let the double-speed wheel run past.
	while player.global_position.y < 178.0:
		_do(Vector2i(0, 1), false)
	while game.time.mode != TimeDebt.Mode.NORMAL:
		_do(Vector2i.ZERO, false)
	# Wait until the wheel is heading home, past the alcove.
	var last_x: float = gallery.position.x
	var guard: int = 0
	while guard < 600:
		_do(Vector2i.ZERO, false)
		var going_left: bool = gallery.position.x < last_x
		last_x = gallery.position.x
		if going_left and gallery.position.x < 1020.0:
			break
		guard += 1
	# Up, along, and down through the exit door.
	while player.global_position.y > 125.0:
		_do(Vector2i(0, -1), false)
	while player.global_position.x < 1175.0:
		_do(Vector2i(1, 0), false)
	guard = 0
	while game.phase == Game.Phase.PLAYING and guard < 300:
		_do(Vector2i(0, 1), false)
		guard += 1

	var ok: bool = game.phase == Game.Phase.WON
	print("policy finished: phase=%d ticks=%d borrowed=%d clock=%d" % [game.phase, game.attempt_ticks, game.borrowed_ticks, game.time.hazard_clock])
	if ok:
		var file: FileAccess = FileAccess.open(OUT_PATH, FileAccess.WRITE)
		file.store_string(JSON.stringify({"note": "segments are [ticks, dx, dy, freeze_held]; y down; first tick follows Enter on the title", "segments": segments}, " "))
		file.close()
		print("wrote %s (%d segments)" % [OUT_PATH, segments.size()])
	game.free()
	quit(0 if ok else 1)
