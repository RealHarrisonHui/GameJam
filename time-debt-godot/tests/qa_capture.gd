extends SceneTree
## Graphical QA, not a test: runs the real game in a real window, replays the
## recorded solution through the same key-event path a player uses, and saves
## screenshots. Run with --fixed-fps 60 so captures land on exact ticks.
##   godot --path . --fixed-fps 60 --script res://tests/qa_capture.gd -- <out_dir> [reduced]

const SOLUTION_PATH: String = "res://tests/solution.json"

var game: Game
var out_dir: String
var tag: String = ""


func _initialize() -> void:
	_run()


func _key(physical: Key, pressed: bool) -> InputEventKey:
	var event := InputEventKey.new()
	event.physical_keycode = physical
	event.pressed = pressed
	return event


## Saves the frame being drawn without yielding, so a capture cannot add a tick.
## HDR 2D viewports hold linear values; the screen gets the sRGB conversion, so do it here too.
func _shot(name: String) -> void:
	var path: String = "%s/%s%s.png" % [out_dir, tag, name]
	var ticks: int = game.attempt_ticks
	var mode: int = game.time.mode
	var debt: int = game.time.debt_ticks
	var save: Callable = func() -> void:
		var image: Image = root.get_texture().get_image()
		if root.use_hdr_2d:
			image = _linear_to_srgb(image)
		image.save_png(path)
		print("shot ", name, " tick ", ticks, " mode ", mode, " debt ", debt)
	RenderingServer.frame_post_draw.connect(save, CONNECT_ONE_SHOT)


## Converts in float: going through 8-bit first would crush the shadows into bands.
func _linear_to_srgb(image: Image) -> Image:
	image.convert(Image.FORMAT_RGBAF)
	var data: PackedFloat32Array = image.get_data().to_float32_array()
	for i: int in range(data.size()):
		if i % 4 == 3:
			data[i] = 1.0
			continue
		var c: float = clampf(data[i], 0.0, 1.0)
		data[i] = c * 12.92 if c < 0.0031308 else 1.055 * pow(c, 1.0 / 2.4) - 0.055
	var out: Image = Image.create_from_data(image.get_width(), image.get_height(), false, Image.FORMAT_RGBAF, data.to_byte_array())
	out.convert(Image.FORMAT_RGBA8)
	return out


func _wait(frames: int) -> void:
	for i: int in range(frames):
		await process_frame


func _set_keys(move: Vector2i, held: bool, state: Dictionary) -> void:
	var wanted: Dictionary = {
		KEY_D: move.x > 0, KEY_A: move.x < 0, KEY_S: move.y > 0, KEY_W: move.y < 0, KEY_SPACE: held,
	}
	for key: Key in wanted:
		if bool(state.get(key, false)) != bool(wanted[key]):
			game._input(_key(key, wanted[key]))
			state[key] = wanted[key]


func _run() -> void:
	var args: PackedStringArray = OS.get_cmdline_user_args()
	out_dir = args[0] if args.size() > 0 else "user://"
	var reduced: bool = args.size() > 1 and args[1] == "reduced"
	tag = "reduced_" if reduced else ""
	game = (load("res://scenes/main.tscn") as PackedScene).instantiate()
	game.pause_on_focus_loss = false
	root.add_child(game)
	if reduced:
		game.set_reduced_effects(true)
	await _wait(60)
	_shot("title")
	await _wait(2)
	await physics_frame
	game._input(_key(KEY_ENTER, true))
	await physics_frame
	game._input(_key(KEY_ENTER, false))

	# Keys are set just before the tick that reads them: physics_frame fires ahead of
	# _physics_process, so each key set here lands on exactly one solution tick.
	var data: Dictionary = JSON.parse_string(FileAccess.get_file_as_string(SOLUTION_PATH))
	var state: Dictionary = {}
	var total: int = 0
	var shots: Dictionary = {30: "normal", 58: "freeze_transition", 72: "frozen", 90: "repay_transition", 112: "repaying", 225: "normal_gallery", 300: "frozen_gallery", 430: "repaying_gallery", 600: "waiting_gallery"}
	for segment: Array in data["segments"]:
		var move := Vector2i(int(segment[1]), int(segment[2]))
		var held: bool = int(segment[3]) != 0
		for i: int in range(int(segment[0])):
			_set_keys(move, held, state)
			await physics_frame
			total += 1
			if shots.has(total):
				_shot(shots[total])
			if game.phase != Game.Phase.PLAYING:
				break
		if game.phase != Game.Phase.PLAYING:
			break
	await _wait(3)
	print("end of replay phase=", game.phase, " ticks=", game.attempt_ticks, " replayed=", total, " won=", game.phase == Game.Phase.WON)
	await _wait(40)
	_shot("won")
	await _wait(2)
	game._input(_key(KEY_ENTER, true))
	await physics_frame
	game._input(_key(KEY_ENTER, false))
	await _wait(3)
	# Walk straight into the shuttle for the loss screen, with a pause on the way.
	for i: int in range(30):
		_set_keys(Vector2i(1, 0), false, state)
		await physics_frame
	game._input(_key(KEY_P, true))
	await physics_frame
	game._input(_key(KEY_P, false))
	await _wait(10)
	_shot("paused")
	await _wait(2)
	game._input(_key(KEY_P, true))
	await physics_frame
	game._input(_key(KEY_P, false))
	await _wait(2)
	state.clear()
	var guard: int = 0
	while game.phase == Game.Phase.PLAYING and guard < 400:
		_set_keys(Vector2i(1, 0), false, state)
		await physics_frame
		guard += 1
	await _wait(4)
	_shot("lost_impact")
	await _wait(40)
	_shot("lost")
	await _wait(3)
	game.free()
	quit(0)
