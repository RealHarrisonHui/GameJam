class_name Game
extends Node2D
## Explicit game state and the update order of one simulation tick. Presentation
## listens through signals and never feeds back into any of this.

enum Phase { TITLE, PLAYING, PAUSED, WON, LOST }

signal phase_changed(phase: Phase, previous: Phase)
signal mode_changed(mode: TimeDebt.Mode, previous: TimeDebt.Mode)
signal attempt_started
signal hazard_struck(index: int, at: Vector2)
signal effects_mode_changed(reduced: bool)
signal ticked

@export var config: GameplayConfig

var time := TimeDebt.new()
var phase: Phase = Phase.TITLE
var attempt_ticks: int = 0
var borrowed_ticks: int = 0
var hit_by: int = -1
var reduced_effects: bool = false
var pause_on_focus_loss: bool = true
var attract_clock: int = 0
var input_state := InputState.new()
var hazards: Array[Hazard] = []
var exit_rect: Rect2

@onready var level: Level = $World/Level
@onready var player: Player = $World/Player


func _ready() -> void:
	time.max_debt_ticks = config.max_debt_ticks
	exit_rect = level.layout.exit_rect
	player.configure(config)
	var shuttle: Hazard = $World/Hazards/Shuttle
	var gallery: Hazard = $World/Hazards/Gallery
	shuttle.configure(HazardRoute.new(level.layout.routes["shuttle"], config.shuttle_one_way, config.shuttle_phase))
	gallery.configure(HazardRoute.new(level.layout.routes["gallery"], config.gallery_one_way, config.gallery_phase))
	hazards = [shuttle, gallery]
	_reset_attempt()
	if DisplayServer.screen_get_refresh_rate() > 61.0:
		get_tree().physics_interpolation = true


func _input(event: InputEvent) -> void:
	input_state.handle_event(event)
	if event.is_action_pressed("toggle_effects") and not (event is InputEventKey and (event as InputEventKey).echo):
		set_reduced_effects(not reduced_effects)


func _physics_process(_delta: float) -> void:
	var before: Phase = phase
	tick(input_state.sample())
	if phase != before:
		input_state.clear()
	else:
		input_state.consume_one_shots()


func _notification(what: int) -> void:
	if what == NOTIFICATION_APPLICATION_FOCUS_OUT or what == NOTIFICATION_WM_WINDOW_FOCUS_OUT:
		if not pause_on_focus_loss:
			return
		if phase == Phase.PLAYING:
			_set_phase(Phase.PAUSED)
		input_state.clear()


## One simulation tick. A tick that changes screen simulates nothing.
func tick(input: TickInput) -> void:
	match phase:
		Phase.TITLE:
			if input.confirm_pressed:
				_new_attempt()
			else:
				attract_clock += 1
				for hazard: Hazard in hazards:
					hazard.set_clock(attract_clock)
		Phase.PLAYING:
			if input.restart_pressed:
				_new_attempt()
			elif input.pause_pressed:
				_set_phase(Phase.PAUSED)
			else:
				_simulate(input)
		Phase.PAUSED:
			if input.restart_pressed:
				_new_attempt()
			elif input.pause_pressed:
				var previous: TimeDebt.Mode = time.mode
				time.resume_from_pause()
				_set_phase(Phase.PLAYING)
				if time.mode != previous:
					mode_changed.emit(time.mode, previous)
		Phase.WON, Phase.LOST:
			if input.restart_pressed or input.confirm_pressed:
				_new_attempt()
	ticked.emit()


func set_reduced_effects(reduced: bool) -> void:
	reduced_effects = reduced
	effects_mode_changed.emit(reduced)


## Update order within a playing tick. Later steps see earlier results:
##   1. Temporal transition from input and debt.
##   2. Player movement against walls, at a speed independent of the mode.
##   3. Hazard positions from the new clock.
##   4. Swept collision over the whole tick. Contact means LOST.
##   5. Exit, only if step 4 found no contact, so a loss beats a simultaneous win.
func _simulate(input: TickInput) -> void:
	var previous_mode: TimeDebt.Mode = time.mode
	var c0: int = time.hazard_clock
	time.step(input.freeze_held, input.freeze_pressed)
	var c1: int = time.hazard_clock
	var p0: Vector2 = player.global_position
	player.step_move(input.move)
	var p1: Vector2 = player.global_position
	for hazard: Hazard in hazards:
		hazard.set_clock(c1)
	attempt_ticks += 1
	if c1 == c0:
		borrowed_ticks += 1
	if time.mode != previous_mode:
		mode_changed.emit(time.mode, previous_mode)
	var reach: float = config.contact_reach()
	for i: int in range(hazards.size()):
		if hazards[i].route.hits_player(reach, c0, c1, p0, p1):
			hit_by = i
			hazard_struck.emit(i, p1)
			_set_phase(Phase.LOST)
			return
	if _circle_hits_rect(p1, config.player_radius, exit_rect):
		_set_phase(Phase.WON)


func _new_attempt() -> void:
	_reset_attempt()
	attempt_started.emit()
	_set_phase(Phase.PLAYING)


func _reset_attempt() -> void:
	var previous_mode: TimeDebt.Mode = time.mode
	time.reset()
	attempt_ticks = 0
	borrowed_ticks = 0
	hit_by = -1
	attract_clock = 0
	player.global_position = level.layout.start
	player.last_move = Vector2i.ZERO
	player.reset_physics_interpolation()
	for hazard: Hazard in hazards:
		hazard.set_clock(0)
		hazard.reset_physics_interpolation()
	input_state.clear()
	if previous_mode != time.mode:
		mode_changed.emit(time.mode, previous_mode)


func _set_phase(next: Phase) -> void:
	var previous: Phase = phase
	phase = next
	phase_changed.emit(next, previous)


## Strict overlap, so a circle resting exactly against the edge is not inside it.
static func _circle_hits_rect(center: Vector2, radius: float, rect: Rect2) -> bool:
	var closest := Vector2(clampf(center.x, rect.position.x, rect.end.x), clampf(center.y, rect.position.y, rect.end.y))
	return center.distance_squared_to(closest) < radius * radius
