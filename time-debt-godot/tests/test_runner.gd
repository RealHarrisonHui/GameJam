extends SceneTree
## Lightweight headless test runner. Exits nonzero if any check fails.
##   godot --headless --path . --script res://tests/test_runner.gd

const MAIN_SCENE: String = "res://scenes/main.tscn"
const SOLUTION_PATH: String = "res://tests/solution.json"
const HASKELL_SOLUTION_PATH: String = "res://tests/haskell_solution.json"

var _failures: int = 0
var _checks: int = 0
var _current: String = ""
var game: Game


func _initialize() -> void:
	_run()


func _run() -> void:
	_time_rules()
	_route_rules()
	_input_rules()
	game = (load(MAIN_SCENE) as PackedScene).instantiate()
	root.add_child(game)
	game.set_physics_process(false)
	await physics_frame
	await physics_frame
	_layout_rules()
	_movement_rules()
	_lethality_rules()
	_flow_rules()
	_level_rules()
	_solution_rules()
	print("%d checks, %d failed" % [_checks, _failures])
	game.free()
	quit(1 if _failures > 0 else 0)


func _section(name: String) -> void:
	_current = name
	print("-- ", name)


func _check(condition: bool, what: String) -> void:
	_checks += 1
	if not condition:
		_failures += 1
		printerr("FAIL [%s] %s" % [_current, what])


func _approx(a: float, b: float, what: String, eps: float = 0.02) -> void:
	_check(absf(a - b) <= eps, "%s: got %f, wanted %f" % [what, a, b])


# ---------------------------------------------------------------- time rules


func _time_rules() -> void:
	_section("normal advancement")
	var t := TimeDebt.new()
	for i: int in range(10):
		t.step(false, false)
	_check(t.hazard_clock == 10 and t.debt_ticks == 0 and t.mode == TimeDebt.Mode.NORMAL, "ten normal ticks move the clock ten")

	_section("frozen clock and debt accumulation")
	t = TimeDebt.new()
	t.step(true, true)
	_check(t.mode == TimeDebt.Mode.FROZEN and t.debt_ticks == 1 and t.hazard_clock == 0, "press freezes and accrues on the same tick")
	for i: int in range(29):
		t.step(true, false)
	_check(t.hazard_clock == 0 and t.debt_ticks == 30, "thirty frozen ticks: clock still, debt 30")

	_section("release into repayment at twice speed")
	t.step(false, false)
	_check(t.mode == TimeDebt.Mode.REPAYING and t.debt_ticks == 29 and t.hazard_clock == 2, "release repays on the same tick at +2")
	var repay_ticks: int = 1
	while t.mode == TimeDebt.Mode.REPAYING:
		var before: int = t.hazard_clock
		t.step(false, false)
		_check(t.hazard_clock == before + 2, "each repaying tick advances the clock two")
		repay_ticks += 1
	_check(repay_ticks == 30, "debt of 30 takes exactly 30 ticks, took %d" % repay_ticks)
	_check(t.debt_ticks == 0 and t.mode == TimeDebt.Mode.NORMAL, "back to NORMAL at zero debt")

	_section("complete borrow/repay cycle")
	for n: int in [1, 2, 37, 120, 179]:
		var c := TimeDebt.new()
		c.step(true, true)
		for i: int in range(n - 1):
			c.step(true, false)
		for i: int in range(n):
			c.step(false, false)
		_check(c.hazard_clock == 2 * n and c.debt_ticks == 0 and c.mode == TimeDebt.Mode.NORMAL, "%d frozen + %d repaying equals 2N clock ticks" % [n, n])
	var last := TimeDebt.new()
	last.step(true, true)
	last.step(false, false)
	_check(last.hazard_clock == 2 and last.mode == TimeDebt.Mode.NORMAL, "the last repayment tick still advances the clock by two")

	_section("automatic release at the cap")
	t = TimeDebt.new()
	t.step(true, true)
	for i: int in range(178):
		t.step(true, false)
	_check(t.mode == TimeDebt.Mode.FROZEN and t.debt_ticks == 179, "tick 179 is still frozen")
	t.step(true, false)
	_check(t.mode == TimeDebt.Mode.REPAYING and t.debt_ticks == 180 and t.hazard_clock == 0, "the cap flips to REPAYING for the following tick, clock unmoved")
	t.step(true, false)
	_check(t.debt_ticks == 179 and t.hazard_clock == 2, "key still held: repayment proceeds")

	_section("no freezing during repayment, fresh press required")
	t.step(true, true)
	_check(t.mode == TimeDebt.Mode.REPAYING and t.debt_ticks == 178, "a press during repayment does nothing")
	while t.mode == TimeDebt.Mode.REPAYING:
		t.step(true, false)
	var clock: int = t.hazard_clock
	for i: int in range(5):
		t.step(true, false)
	_check(t.mode == TimeDebt.Mode.NORMAL and t.hazard_clock == clock + 5, "held key without a press does not re-freeze")
	t.step(true, true)
	_check(t.mode == TimeDebt.Mode.FROZEN, "a fresh press freezes again")

	_section("release with no debt, tap edge, pause resume")
	t = TimeDebt.new()
	t.mode = TimeDebt.Mode.FROZEN
	t.step(false, false)
	_check(t.mode == TimeDebt.Mode.NORMAL and t.hazard_clock == 1, "frozen with no debt, released: NORMAL")
	t = TimeDebt.new()
	t.step(false, true)
	_check(t.mode == TimeDebt.Mode.NORMAL and t.hazard_clock == 1, "pressed but no longer held on the tick: no freeze")
	t = TimeDebt.new()
	t.step(true, true)
	t.resume_from_pause()
	_check(t.mode == TimeDebt.Mode.REPAYING, "paused while frozen with debt: resume into REPAYING")
	t = TimeDebt.new()
	t.mode = TimeDebt.Mode.FROZEN
	t.resume_from_pause()
	_check(t.mode == TimeDebt.Mode.NORMAL, "paused while frozen without debt: resume into NORMAL")

	_section("fuzz: invariants and no teleport")
	var rng := RandomNumberGenerator.new()
	rng.seed = 20261107
	var route := HazardRoute.new(PackedVector2Array([Vector2(600, 100), Vector2(1210, 100)]), 90, 40)
	var per_tick: float = route.length / float(route.one_way)
	t = TimeDebt.new()
	var held: bool = false
	var ok_debt: bool = true
	var ok_step: bool = true
	var ok_teleport: bool = true
	for i: int in range(20000):
		var pressed: bool = false
		if rng.randf() < 0.05:
			held = not held
			pressed = held
		var c0: int = t.hazard_clock
		var p0: Vector2 = route.position_at(c0)
		t.step(held, pressed)
		ok_debt = ok_debt and t.debt_ticks >= 0 and t.debt_ticks <= 180
		ok_step = ok_step and (t.hazard_clock - c0) in [0, 1, 2]
		ok_teleport = ok_teleport and p0.distance_to(route.position_at(t.hazard_clock)) <= 2.0 * per_tick + 0.001
	_check(ok_debt, "debt stays in 0..180")
	_check(ok_step, "the clock only ever moves 0, 1 or 2")
	_check(ok_teleport, "a hazard never moves further than two normal ticks in one tick")


func _route_rules() -> void:
	_section("route and sweep")
	var route := HazardRoute.new(PackedVector2Array([Vector2(300, 220), Vector2(300, 360)]), 20, 0)
	_check(route.position_at(0) == Vector2(300, 220), "starts at first point")
	_check(route.position_at(20) == Vector2(300, 360), "reaches last point after one_way")
	_check(route.position_at(40) == Vector2(300, 220), "returns after a period")
	_check(route.position_at(10) == Vector2(300, 290), "constant speed")

	_section("fast hazards cannot tunnel")
	var fast := HazardRoute.new(PackedVector2Array([Vector2(0, 0), Vector2(1000, 0)]), 2, 0)
	# Samples at x = 0, 500 and 1000 are all far from x = 250, but the path crosses it.
	_check(fast.hits_player(10.0, 0, 1, Vector2(250, 0), Vector2(250, 0)), "swept check catches a hazard jumping past a still player")
	_check(not fast.hits_player(10.0, 0, 1, Vector2(250, 50), Vector2(250, 50)), "and does not invent contact off the path")
	_check(HazardRoute.swept_contact(5.0, Vector2(-100, 0), Vector2(100, 0), Vector2(100, 0), Vector2(-100, 0)), "head-on pass between samples is contact")
	_check(not HazardRoute.swept_contact(5.0, Vector2(-100, 0), Vector2(-100, 0), Vector2(100, 0), Vector2(100, 0)), "distant still pair is not contact")


func _key(physical: Key, pressed: bool, echo: bool = false) -> InputEventKey:
	var event := InputEventKey.new()
	event.physical_keycode = physical
	event.pressed = pressed
	event.echo = echo
	return event


func _input_rules() -> void:
	_section("input edges")
	var s := InputState.new()
	s.handle_event(_key(KEY_SPACE, true))
	_check(s.sample().freeze_held and s.sample().freeze_pressed, "press gives held and edge")
	s.consume_one_shots()
	_check(s.sample().freeze_held and not s.sample().freeze_pressed, "edge lasts one tick")
	s.handle_event(_key(KEY_SPACE, true, true))
	_check(not s.sample().freeze_pressed, "OS key repeat is not a press")
	s.clear()
	_check(not s.sample().freeze_held, "clear forgets a key still physically down")
	s.handle_event(_key(KEY_SPACE, true, true))
	_check(not s.sample().freeze_held, "repeat of a cleared key does not bring it back")
	s.handle_event(_key(KEY_W, true))
	s.handle_event(_key(KEY_RIGHT, true))
	_check(s.sample().move == Vector2i(1, -1), "WASD and arrows combine, y down")
	s.handle_event(_key(KEY_S, true))
	_check(s.sample().move == Vector2i(1, 0), "opposite keys cancel")


# ------------------------------------------------------------ layout and motion


func _layout_rules() -> void:
	_section("coordinate translation from Gloss")
	var layout: LevelLayout = game.level.layout
	_check(layout.start == Vector2(120, 290), "start (-520, 70) -> (120, 290)")
	_check(layout.exit_rect == Rect2(1141, 256, 68, 68), "exit translated")
	_check(layout.routes["shuttle"][0] == Vector2(300, 240), "shuttle route translated")
	_check(layout.routes["gallery"][1] == Vector2(1210, 100), "gallery route translated")
	_check(layout.walls.size() == 20, "all twenty wall rects present")
	_check(Engine.physics_ticks_per_second == 60, "60 physics ticks per second")


func _start_playing() -> void:
	game.phase = Game.Phase.TITLE
	var confirm := TickInput.new()
	confirm.confirm_pressed = true
	game.tick(confirm)
	_check(game.phase == Game.Phase.PLAYING, "attempt started")


func _movement_rules() -> void:
	_section("player speed unchanged between modes")
	_start_playing()
	var speed: float = game.config.player_speed
	game.player.global_position = Vector2(500, 300)
	var p0: Vector2 = game.player.global_position
	game.tick(TickInput.make(Vector2i(1, 0)))
	_check(game.time.mode == TimeDebt.Mode.NORMAL, "normal mode")
	var normal_step: float = p0.distance_to(game.player.global_position)
	p0 = game.player.global_position
	game.tick(TickInput.make(Vector2i(1, 0), true, true))
	_check(game.time.mode == TimeDebt.Mode.FROZEN, "frozen mode")
	var frozen_step: float = p0.distance_to(game.player.global_position)
	game.tick(TickInput.make(Vector2i.ZERO, true, false))
	p0 = game.player.global_position
	game.tick(TickInput.make(Vector2i(1, 0)))
	_check(game.time.mode == TimeDebt.Mode.REPAYING, "repaying mode")
	var repay_step: float = p0.distance_to(game.player.global_position)
	_approx(normal_step, speed, "normal step")
	_approx(frozen_step, speed, "frozen step")
	_approx(repay_step, speed, "repaying step")

	_section("normalised diagonal")
	_start_playing()
	game.player.global_position = Vector2(500, 300)
	p0 = game.player.global_position
	game.tick(TickInput.make(Vector2i(1, 1)))
	_approx(p0.distance_to(game.player.global_position), speed, "diagonal step length")
	_approx(Player.move_delta(Vector2i(-1, 1), speed).length(), speed, "move_delta length")

	_section("walls block movement")
	_start_playing()
	for i: int in range(300):
		game.tick(TickInput.make(Vector2i(-1, 0)))
	_approx(game.player.global_position.x, 40.0 + 13.0, "stops at the outer left wall", 0.2)
	_approx(game.player.global_position.y, 290.0, "without drifting", 0.01)
	for i: int in range(300):
		game.tick(TickInput.make(Vector2i(-1, -1)))
	_check(game.player.global_position.y >= 160.0 + 12.8, "stops at the top of the start room")
	_approx(game.player.global_position.x, 53.0, "slides along the wall, keeping x", 0.2)
	_start_playing()
	game.player.global_position = Vector2(200, 200)
	for i: int in range(60):
		game.tick(TickInput.make(Vector2i(1, 0)))
	_check(game.player.global_position.x <= 240.0 - 12.8, "start-room east wall blocks away from the door")


func _lethality_rules() -> void:
	_section("frozen hazards remain lethal")
	_start_playing()
	game.tick(TickInput.make(Vector2i.ZERO, true, true))
	_check(game.time.mode == TimeDebt.Mode.FROZEN, "frozen")
	game.player.global_position = game.hazards[0].position + Vector2(0, 30)
	game.tick(TickInput.make(Vector2i.ZERO, true, false))
	_check(game.phase == Game.Phase.LOST and game.hit_by == 0, "touching a frozen wheel loses")

	_section("contact reach")
	_start_playing()
	game.tick(TickInput.make(Vector2i.ZERO, true, true))
	var h: Vector2 = game.hazards[1].position
	game.player.global_position = h + Vector2(0, 38.5)
	game.tick(TickInput.make(Vector2i.ZERO, true, false))
	_check(game.phase == Game.Phase.PLAYING, "38.5 px from centre is a graze, not contact")
	game.player.global_position = h + Vector2(0, 37.5)
	game.tick(TickInput.make(Vector2i.ZERO, true, false))
	_check(game.phase == Game.Phase.LOST, "37.5 px is contact")

	_section("repaying hazards cannot tunnel through the player in-game")
	for x: float in [640.0, 777.0, 901.0, 1033.0, 1166.0]:
		_start_playing()
		game.tick(TickInput.make(Vector2i.ZERO, true, true))
		for i: int in range(179):
			game.tick(TickInput.make(Vector2i.ZERO, true, false))
		game.player.global_position = Vector2(x, 100)
		_check(game.phase == Game.Phase.PLAYING or game.hit_by == 1, "still alive or already struck by the gallery wheel")
		var guard: int = 0
		while game.phase == Game.Phase.PLAYING and guard < 200:
			game.tick(TickInput.new())
			guard += 1
		_check(game.phase == Game.Phase.LOST and game.hit_by == 1, "a player standing at x=%d is struck by a double-speed wheel" % int(x))

	_section("loss beats simultaneous exit contact")
	_start_playing()
	var saved: Rect2 = game.exit_rect
	game.tick(TickInput.make(Vector2i.ZERO, true, true))
	var at: Vector2 = game.hazards[1].position + Vector2(0, 30)
	game.exit_rect = Rect2(at - Vector2(20, 20), Vector2(40, 40))
	game.player.global_position = at
	game.tick(TickInput.make(Vector2i.ZERO, true, false))
	_check(game.phase == Game.Phase.LOST, "same-tick hazard and exit contact is a loss")
	game.exit_rect = saved

	_section("exit wins")
	_start_playing()
	game.player.global_position = game.exit_rect.get_center()
	game.tick(TickInput.new())
	_check(game.phase == Game.Phase.WON, "reaching the exit wins")
	var frozen_pos: Vector2 = game.player.global_position
	for i: int in range(30):
		game.tick(TickInput.make(Vector2i(-1, 0)))
	_check(game.phase == Game.Phase.WON and game.player.global_position == frozen_pos, "gameplay stops after the result")


func _snapshot() -> Array:
	var positions: Array[Vector2] = []
	for hazard: Hazard in game.hazards:
		positions.append(hazard.position)
	return [game.phase, game.time.mode, game.time.debt_ticks, game.time.hazard_clock, game.player.global_position, positions, game.attempt_ticks, game.borrowed_ticks, game.hit_by]


func _flow_rules() -> void:
	_section("pause stops simulation")
	_start_playing()
	game.tick(TickInput.make(Vector2i(1, 0)))
	game.tick(TickInput.make(Vector2i(1, 0), true, true))
	var pause := TickInput.new()
	pause.pause_pressed = true
	game.tick(pause)
	_check(game.phase == Game.Phase.PAUSED, "paused")
	var before: Array = _snapshot()
	for i: int in range(120):
		game.tick(TickInput.make(Vector2i(1, 0), true, i == 0))
	_check(before == _snapshot(), "120 paused ticks with keys down change nothing")
	game.tick(pause)
	_check(game.phase == Game.Phase.PLAYING and game.time.mode == TimeDebt.Mode.REPAYING, "paused while frozen resumes into REPAYING")

	_section("stale keys after pause")
	_start_playing()
	game.input_state.clear()
	game._input(_key(KEY_SPACE, true))
	game._input(_key(KEY_D, true))
	game.tick(game.input_state.sample())
	game.input_state.consume_one_shots()
	_check(game.time.mode == TimeDebt.Mode.FROZEN, "real key events freeze")
	game._notification(Node.NOTIFICATION_APPLICATION_FOCUS_OUT)
	_check(game.phase == Game.Phase.PAUSED, "focus loss pauses")
	game._input(_key(KEY_P, true))
	game.tick(game.input_state.sample())
	game.input_state.clear()
	_check(game.phase == Game.Phase.PLAYING, "P resumes")
	var x0: float = game.player.global_position.x
	var moved: TickInput = game.input_state.sample()
	game.tick(moved)
	_check(not moved.freeze_held and moved.move == Vector2i.ZERO, "held Space and D do not survive the pause")
	_check(game.player.global_position.x == x0, "no movement from a stale key")

	_section("restart restores the initial state")
	_start_playing()
	var fresh: Array = _snapshot()
	for i: int in range(45):
		game.tick(TickInput.make(Vector2i(1, -1), i >= 10, i == 10))
	_check(fresh != _snapshot(), "state actually changed")
	var restart := TickInput.new()
	restart.restart_pressed = true
	game.tick(restart)
	_check(fresh == _snapshot(), "restart restores every gameplay value")
	game.player.global_position = game.exit_rect.get_center()
	game.tick(TickInput.new())
	var retry := TickInput.new()
	retry.confirm_pressed = true
	game.tick(retry)
	_check(fresh == _snapshot(), "retry from a result restores it too")
	_check(is_equal_approx(Engine.time_scale, 1.0), "Engine.time_scale is untouched")


func _level_rules() -> void:
	_section("level: safe spawn, safe rooms across the cycle, at repayment speed")
	var layout: LevelLayout = game.level.layout
	var reach: float = game.config.contact_reach()
	var radius: float = game.config.player_radius
	var cycle: int = 360
	var areas: Array[Rect2] = [layout.start_area, layout.safe_area]
	for area: Rect2 in areas:
		var inner: Rect2 = area.grow(-radius)
		var safe: bool = true
		for hazard: Hazard in game.hazards:
			for step: int in [1, 2]:
				for c: int in range(cycle):
					for px: float in [inner.position.x, inner.end.x]:
						for py: float in [inner.position.y, inner.get_center().y, inner.end.y]:
							var p := Vector2(px, py)
							if hazard.route.hits_player(reach, c, c + step, p, p):
								safe = false
		_check(safe, "room at %s is safe over the full cycle at x1 and x2" % str(area))
	var alcove_low := Vector2(layout.refuge.get_center().x, layout.refuge.end.y - radius)
	var alcove_safe: bool = true
	for c: int in range(cycle):
		for step: int in [1, 2]:
			alcove_safe = alcove_safe and not game.hazards[1].route.hits_player(reach, c, c + step, alcove_low, alcove_low)
	_check(alcove_safe, "the alcove is safe over the full cycle")
	_start_playing()
	_check(layout.start_area.has_point(game.player.global_position), "spawn is in the start area")
	game.tick(TickInput.new())
	_check(game.phase == Game.Phase.PLAYING, "spawn is not immediately lethal")


# ------------------------------------------------------------------- solution


func _load_solution(path: String = SOLUTION_PATH) -> Array:
	var data: Dictionary = JSON.parse_string(FileAccess.get_file_as_string(path))
	return data["segments"]


## Replays run-length segments [count, dx, dy, held] through the real tick. The
## callable is invoked before every tick so a test can poke at presentation.
func _play_solution(before_tick: Callable = Callable(), path: String = SOLUTION_PATH) -> Array:
	_start_playing()
	var log_lines: Array = []
	var was_held: bool = false
	for segment: Array in _load_solution(path):
		var count: int = int(segment[0])
		var move := Vector2i(int(segment[1]), int(segment[2]))
		var held: bool = int(segment[3]) != 0
		for i: int in range(count):
			if before_tick.is_valid():
				before_tick.call()
			game.tick(TickInput.make(move, held, held and not was_held))
			was_held = held
			log_lines.append(_snapshot())
			if game.phase != Game.Phase.PLAYING:
				return log_lines
	return log_lines


func _solution_rules() -> void:
	if not FileAccess.file_exists(SOLUTION_PATH):
		_section("solution")
		_check(false, "tests/solution.json is missing")
		return
	_section("recorded tick-input solution completes the level")
	var reference: Array = _play_solution()
	_check(game.phase == Game.Phase.WON, "replay reaches the exit, ended in phase %d after %d ticks" % [game.phase, game.attempt_ticks])
	_check(game.borrowed_ticks > 0, "the solution borrows time (%d ticks)" % game.borrowed_ticks)
	print("   solution: %d ticks (%.2f s), %d ticks borrowed" % [game.attempt_ticks, game.attempt_ticks / 60.0, game.borrowed_ticks])

	_section("the Haskell prototype's own script wins here too")
	_play_solution(Callable(), HASKELL_SOLUTION_PATH)
	_check(game.phase == Game.Phase.WON, "Haskell solutionScript reaches the exit in Godot, ended in phase %d after %d ticks" % [game.phase, game.attempt_ticks])
	print("   haskell script: %d ticks, %d borrowed" % [game.attempt_ticks, game.borrowed_ticks])

	_section("deterministic replay")
	_check(reference == _play_solution(), "two replays match tick for tick")

	_section("reduced effects does not alter gameplay")
	game.set_reduced_effects(true)
	_check(reference == _play_solution(), "a replay in reduced mode matches tick for tick")
	var counter: Array[int] = [0]
	var toggler: Callable = func() -> void:
		counter[0] += 1
		if counter[0] % 17 == 0:
			game.set_reduced_effects(not game.reduced_effects)
	_check(reference == _play_solution(toggler), "toggling mid-run matches tick for tick")
	game.set_reduced_effects(false)
