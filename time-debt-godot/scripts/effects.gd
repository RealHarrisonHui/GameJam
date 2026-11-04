class_name Effects
extends Node2D
## All temporal presentation. It listens to the game's signals and writes shader
## globals, lights and particles. Nothing here writes to gameplay state, and F2 only
## changes what this node does.

const WASH_SPEED: float = 1500.0
const WASH_FULL: float = 1700.0
const TONE_NORMAL: float = 0.0
const TONE_FROZEN: float = 1.0
const TONE_REPAYING: float = 2.0
const TONE_SETTLED: float = 3.0
const TONE_FORECLOSED: float = 4.0
const LANTERN_NORMAL := Color(1.0, 0.8, 0.52)
const LANTERN_FROZEN := Color(0.62, 0.86, 1.0)
const LANTERN_REPAYING := Color(1.0, 0.56, 0.36)
const AMBIENT_FULL := Color(0.60, 0.59, 0.60)
const AMBIENT_REDUCED := Color(0.96, 0.96, 0.96)

var game: Game
var pclock: float = 0.0
var _reduced: bool = false
var _tone: float = 0.0
var _wash_radius: float = WASH_FULL
var _edge: float = 0.0
var _shake: float = 0.0
var _rng := RandomNumberGenerator.new()

@onready var ambient: CanvasModulate = $"../Ambient"
@onready var lights: Node2D = $"../Lamps"
@onready var dust: GPUParticles2D = $Dust
@onready var impact: GPUParticles2D = $Impact
@onready var camera: Camera2D = $"../../Camera"
@onready var world_environment: WorldEnvironment = $"../../WorldEnvironment"
@onready var player_visual: PlayerVisual = $"../Player/Visual"
@onready var exit_door: ExitDoor = $"../ExitDoor"
@onready var clockwork: Clockwork = $"../Clockwork"


func _ready() -> void:
	game = owner
	game.mode_changed.connect(_on_mode_changed)
	game.phase_changed.connect(_on_phase_changed)
	game.attempt_started.connect(_on_attempt_started)
	game.hazard_struck.connect(_on_hazard_struck)
	game.effects_mode_changed.connect(_on_effects_mode_changed)
	game.ticked.connect(_on_ticked)
	_rng.seed = 1926
	exit_door.global_position = ($"../Level" as Level).layout.exit_rect.get_center()
	($"../Lamps/VaultLight" as Node2D).global_position = exit_door.global_position
	_set_globals(TONE_NORMAL, TONE_NORMAL, Vector2.ZERO, WASH_FULL, 0.0)
	_apply_reduced(false)
	_on_ticked()


func _process(delta: float) -> void:
	# Decoration pauses explicitly with the game; the result screens keep it alive.
	var running: bool = game.phase != Game.Phase.PAUSED
	var dt: float = delta if running else 0.0
	pclock += dt
	exit_door.animate(pclock, dt)
	if _wash_radius < WASH_FULL:
		_wash_radius = minf(_wash_radius + WASH_SPEED * delta, WASH_FULL)
		_edge = clampf(1.0 - _wash_radius / WASH_FULL, 0.0, 1.0)
		if _wash_radius >= WASH_FULL:
			_set_globals(_tone, _tone, Vector2.ZERO, WASH_FULL, 0.0)
		else:
			RenderingServer.global_shader_parameter_set("td_wash_radius", _wash_radius)
			RenderingServer.global_shader_parameter_set("td_wash_edge", _edge if not _reduced else 0.0)
	if _shake > 0.0:
		_shake = maxf(_shake - delta * 3.2, 0.0)
		camera.offset = Vector2(_rng.randf_range(-1.0, 1.0), _rng.randf_range(-1.0, 1.0)) * 7.0 * _shake * _shake
	elif camera.offset != Vector2.ZERO:
		camera.offset = Vector2.ZERO


func _on_ticked() -> void:
	var clock: int = game.time.hazard_clock if game.phase != Game.Phase.TITLE else game.attract_clock
	clockwork.set_clock(clock)
	RenderingServer.global_shader_parameter_set("td_hazard_time", float(clock) / 60.0)
	player_visual.set_debt(float(game.time.debt_ticks) / float(game.time.max_debt_ticks), game.time.mode)


func _on_mode_changed(mode: TimeDebt.Mode, _previous: TimeDebt.Mode) -> void:
	for hazard: Hazard in game.hazards:
		hazard.set_mode_visual(mode)
	dust.speed_scale = _dust_rate(mode)
	var color: Color = LANTERN_NORMAL
	var target: float = TONE_NORMAL
	match mode:
		TimeDebt.Mode.FROZEN:
			color = LANTERN_FROZEN
			target = TONE_FROZEN
		TimeDebt.Mode.REPAYING:
			color = LANTERN_REPAYING
			target = TONE_REPAYING
	player_visual.light.color = color
	if game.phase == Game.Phase.PLAYING or game.phase == Game.Phase.PAUSED:
		_wash_to(target, game.player.global_position)


func _on_phase_changed(phase: Game.Phase, _previous: Game.Phase) -> void:
	dust.speed_scale = 0.0 if phase == Game.Phase.PAUSED else _dust_rate(game.time.mode)
	impact.speed_scale = 0.0 if phase == Game.Phase.PAUSED else 1.0
	for hazard: Hazard in game.hazards:
		hazard.sparks.speed_scale = 0.0 if phase == Game.Phase.PAUSED else 1.0
	match phase:
		Game.Phase.WON:
			exit_door.set_opened(true)
			_wash_to(TONE_SETTLED, exit_door.global_position)
			for hazard: Hazard in game.hazards:
				hazard.set_mode_visual(TimeDebt.Mode.NORMAL)
		Game.Phase.LOST:
			_wash_to(TONE_FORECLOSED, game.player.global_position)
			for hazard: Hazard in game.hazards:
				hazard.set_mode_visual(TimeDebt.Mode.NORMAL)


func _on_attempt_started() -> void:
	_wash_radius = WASH_FULL
	_tone = TONE_NORMAL
	_shake = 0.0
	camera.offset = Vector2.ZERO
	_set_globals(TONE_NORMAL, TONE_NORMAL, Vector2.ZERO, WASH_FULL, 0.0)
	exit_door.set_opened(false)
	impact.emitting = false
	player_visual.light.color = LANTERN_NORMAL
	player_visual.set_debt(0.0, TimeDebt.Mode.NORMAL)
	for hazard: Hazard in game.hazards:
		hazard.set_mode_visual(TimeDebt.Mode.NORMAL)
		hazard.clear_transients()
		hazard.set_effects_enabled(not _reduced)
	dust.speed_scale = 1.0


func _on_hazard_struck(_index: int, at: Vector2) -> void:
	_shake = 1.0 if not _reduced else 0.0
	if not _reduced:
		impact.global_position = at
		impact.restart()
		impact.emitting = true


func _on_effects_mode_changed(reduced: bool) -> void:
	_apply_reduced(reduced)


func _apply_reduced(reduced: bool) -> void:
	_reduced = reduced
	lights.visible = not reduced
	player_visual.light.visible = not reduced
	dust.visible = not reduced
	impact.visible = not reduced
	for hazard: Hazard in game.hazards:
		hazard.set_effects_enabled(not reduced)
	ambient.color = AMBIENT_REDUCED if reduced else AMBIENT_FULL
	world_environment.environment.glow_enabled = not reduced
	RenderingServer.global_shader_parameter_set("td_detail", 0.0 if reduced else 1.0)
	if reduced:
		_shake = 0.0
		if _wash_radius < WASH_FULL:
			_wash_radius = WASH_FULL
			_set_globals(_tone, _tone, Vector2.ZERO, WASH_FULL, 0.0)


func _wash_to(tone: float, center: Vector2) -> void:
	var from: float = _tone
	_tone = tone
	if _reduced:
		_wash_radius = WASH_FULL
		_set_globals(tone, tone, Vector2.ZERO, WASH_FULL, 0.0)
	else:
		_wash_radius = 0.0
		_edge = 1.0
		_set_globals(from, tone, center, 0.0, 1.0)


func _set_globals(from: float, to: float, center: Vector2, radius: float, edge: float) -> void:
	RenderingServer.global_shader_parameter_set("td_tone_from", from)
	RenderingServer.global_shader_parameter_set("td_tone_to", to)
	RenderingServer.global_shader_parameter_set("td_wash_center", center)
	RenderingServer.global_shader_parameter_set("td_wash_radius", radius)
	RenderingServer.global_shader_parameter_set("td_wash_edge", edge)


static func _dust_rate(mode: TimeDebt.Mode) -> float:
	match mode:
		TimeDebt.Mode.FROZEN:
			return 0.0
		TimeDebt.Mode.REPAYING:
			return 2.0
	return 1.0
