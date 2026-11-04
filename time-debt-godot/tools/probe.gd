extends SceneTree
## Breadth-first search over the real player body and hazard routes with freezing
## disabled: can each crossing be done by timing and dodging alone? States are
## (clock mod hazard period, position rounded to a grid cell), so "no route" is
## strong evidence rather than a proof. Same method as the Haskell test/Probe.hs.
##   godot --headless --path . --script res://tools/probe.gd

var game: Game


func _initialize() -> void:
	_run()


func _run() -> void:
	game = (load("res://scenes/main.tscn") as PackedScene).instantiate()
	root.add_child(game)
	game.set_physics_process(false)
	await physics_frame
	await physics_frame
	_straight_walks()
	for cell: float in [3.0, 2.0]:
		print("-- grid %.1f px" % cell)
		_crossing1(cell)
		_crossing2(cell)
	game.free()
	quit(0)


## How often does simply walking through the slot work, by height and starting phase?
func _straight_walks() -> void:
	var route: HazardRoute = game.hazards[0].route
	var reach: float = game.config.contact_reach()
	for y: float in [268.0, 290.0, 312.0]:
		var ok: int = 0
		for phase: int in range(route.period()):
			var clock: int = phase
			var p := Vector2(225, y)
			var alive: bool = true
			while p.x < 373.0 and alive:
				var q: Vector2 = p + Vector2(game.config.player_speed, 0)
				alive = not route.hits_player(reach, clock, clock + 1, p, q)
				p = q
				clock += 1
			if alive:
				ok += 1
		print("Straight walk at y=%d, no freeze: %d of %d phases survive" % [int(y), ok, route.period()])


## The start room is safe, so every clock phase is available at its door; the
## search only has to cover the cells between there and the goal.
func _crossing1(cell: float) -> void:
	var seeds: Array[Vector2] = []
	for y: float in range(268, 313, 4):
		seeds.append(Vector2(225, y))
	var goal: Callable = func(p: Vector2) -> bool: return p.x >= 373.0
	var bounds := Rect2(220, 160, 160, 260)
	_search("Crossing 1, no freeze: start room -> grace room", game.hazards[0].route, seeds, goal, bounds, cell)


func _crossing2(cell: float) -> void:
	var seeds: Array[Vector2] = []
	for x: float in range(604, 648, 4):
		seeds.append(Vector2(x, 165))
	var goal: Callable = func(p: Vector2) -> bool: return p.x >= 1140.0 and p.y >= 150.0
	var bounds := Rect2(560, 40, 700, 125)
	_search("Crossing 2, no freeze: grace room -> exit door", game.hazards[1].route, seeds, goal, bounds, cell)
	var goal_alcove: Callable = func(p: Vector2) -> bool: return p.x >= 1036.0 and p.x <= 1092.0 and p.y >= 150.0
	_search("Crossing 2a, no freeze: grace room -> alcove", game.hazards[1].route, seeds, goal_alcove, bounds, cell)


func _search(label: String, route: HazardRoute, seeds: Array[Vector2], goal: Callable, bounds: Rect2, cell: float) -> void:
	var started: int = Time.get_ticks_msec()
	var reach: float = game.config.contact_reach()
	var player: Player = game.player
	var period: int = route.period()
	var seen: Dictionary[int, bool] = {}
	var frontier: Array[Vector3] = []
	for phase: int in range(period):
		for p: Vector2 in seeds:
			frontier.append(Vector3(p.x, p.y, phase))
	var moves: Array[Vector2i] = []
	for dx: int in [-1, 0, 1]:
		for dy: int in [-1, 0, 1]:
			moves.append(Vector2i(dx, dy))
	var states: int = 0
	var closest: float = 1e9
	var found: bool = false
	var found_ticks: int = 0
	var depth: int = 0
	while frontier.size() > 0 and not found:
		depth += 1
		var next: Array[Vector3] = []
		for state: Vector3 in frontier:
			var clock: int = int(state.z)
			var p0 := Vector2(state.x, state.y)
			for move: Vector2i in moves:
				player.global_position = p0
				player.step_move(move)
				var p1: Vector2 = player.global_position
				if not bounds.has_point(p1):
					continue
				if route.hits_player(reach, clock, clock + 1, p0, p1):
					continue
				var c1: int = (clock + 1) % period
				var key: int = c1 + period * (int(p1.x / cell) + 2000 * int(p1.y / cell))
				if seen.has(key):
					continue
				seen[key] = true
				states += 1
				if goal.call(p1):
					found = true
					found_ticks = depth
				next.append(Vector3(p1.x, p1.y, c1))
		frontier = next
	print("%-52s %s  (%d states, %.1f s)" % [label, ("route in %d ticks" % found_ticks) if found else "NO ROUTE FOUND", states, (Time.get_ticks_msec() - started) / 1000.0])
