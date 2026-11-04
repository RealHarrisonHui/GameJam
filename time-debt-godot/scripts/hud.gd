class_name Hud
extends CanvasLayer
## The ledger strip and the game's screens. Reads gameplay state, sends only the
## same confirm the Enter key sends.

const MODE_TEXT: Dictionary[TimeDebt.Mode, String] = {
	TimeDebt.Mode.NORMAL: "NORMAL",
	TimeDebt.Mode.FROZEN: "FROZEN",
	TimeDebt.Mode.REPAYING: "REPAYING",
}
const MODE_NOTE: Dictionary[TimeDebt.Mode, String] = {
	TimeDebt.Mode.NORMAL: "Hold SPACE to borrow time",
	TimeDebt.Mode.FROZEN: "Wheels stopped. Release to repay",
	TimeDebt.Mode.REPAYING: "Wheels at double speed",
}
const MODE_COLOR: Dictionary[TimeDebt.Mode, Color] = {
	TimeDebt.Mode.NORMAL: Color(0.94, 0.87, 0.68),
	TimeDebt.Mode.FROZEN: Color(0.66, 0.92, 1.0),
	TimeDebt.Mode.REPAYING: Color(1.0, 0.45, 0.33),
}

var game: Game
var _pclock: float = 0.0

@onready var mode_label: Label = %ModeLabel
@onready var mode_note: Label = %ModeNote
@onready var meter: DebtMeter = %Meter
@onready var debt_value: Label = %DebtValue
@onready var keys_label: Label = %KeysLabel
@onready var strip: Control = %Strip
@onready var title_screen: Control = %TitleScreen
@onready var enter_hint: Button = %EnterButton
@onready var pause_screen: Control = %PauseScreen
@onready var pause_note: Label = %PauseNote
@onready var result_screen: Control = %ResultScreen
@onready var stamp: Label = %Stamp
@onready var result_line: Label = %ResultLine
@onready var result_time: Label = %ResultTime
@onready var result_borrowed: Label = %ResultBorrowed
@onready var retry_button: Button = %RetryButton
@onready var flash: ColorRect = %Flash


func _ready() -> void:
	game = owner
	game.phase_changed.connect(_on_phase_changed)
	game.effects_mode_changed.connect(_on_effects_mode_changed)
	game.ticked.connect(_refresh)
	game.hazard_struck.connect(_on_hazard_struck)
	enter_hint.pressed.connect(game.input_state.request_confirm)
	retry_button.pressed.connect(game.input_state.request_confirm)
	_on_effects_mode_changed(game.reduced_effects)
	_show_screens()
	_refresh()


func _process(delta: float) -> void:
	_pclock += delta
	if title_screen.visible:
		enter_hint.modulate.a = 0.82 + 0.18 * sin(_pclock * 2.4)
	if flash.color.a > 0.0:
		flash.color.a = maxf(flash.color.a - delta * 3.2, 0.0)
	var debt_near_cap: bool = game.time.mode == TimeDebt.Mode.FROZEN and game.time.debt_ticks >= game.time.max_debt_ticks - 45
	mode_label.modulate = Color.WHITE if not debt_near_cap or fmod(_pclock, 0.24) < 0.14 else Color(1.0, 0.45, 0.33)


func _refresh() -> void:
	var time: TimeDebt = game.time
	mode_label.text = MODE_TEXT[time.mode]
	mode_label.add_theme_color_override("font_color", MODE_COLOR[time.mode])
	mode_note.text = MODE_NOTE[time.mode]
	meter.set_state(time.debt_ticks, time.max_debt_ticks, time.mode)
	debt_value.text = "%.2f s" % (float(time.debt_ticks) / 60.0)
	debt_value.add_theme_color_override("font_color", MODE_COLOR[time.mode])


func _on_effects_mode_changed(reduced: bool) -> void:
	keys_label.text = "P  pause     R  restart\nF2  effects: %s" % ("reduced" if reduced else "full")


func _on_hazard_struck(_index: int, _at: Vector2) -> void:
	if not game.reduced_effects:
		flash.color = Color(1.0, 0.86, 0.7, 0.28)


func _on_phase_changed(phase: Game.Phase, _previous: Game.Phase) -> void:
	_show_screens()
	if phase == Game.Phase.PAUSED:
		var owed: bool = game.time.mode == TimeDebt.Mode.FROZEN and game.time.debt_ticks > 0
		pause_note.text = "Your debt falls due the moment you resume." if owed else "The wheels are waiting."
	elif phase == Game.Phase.WON or phase == Game.Phase.LOST:
		_show_result(phase == Game.Phase.WON)


func _show_screens() -> void:
	title_screen.visible = game.phase == Game.Phase.TITLE
	pause_screen.visible = game.phase == Game.Phase.PAUSED
	result_screen.visible = game.phase == Game.Phase.WON or game.phase == Game.Phase.LOST
	strip.visible = game.phase != Game.Phase.TITLE


func _show_result(won: bool) -> void:
	stamp.text = "PAID IN FULL" if won else "FORECLOSED"
	stamp.add_theme_color_override("font_color", Color(0.96, 0.8, 0.42) if won else Color(0.93, 0.27, 0.2))
	result_line.text = "The vault lets you go. Every second accounted for." if won else "A collector caught up with you. Frozen or not, they bite."
	result_time.text = "%.1f s" % (float(game.attempt_ticks) / 60.0)
	result_borrowed.text = "%.1f s" % (float(game.borrowed_ticks) / 60.0)
	retry_button.text = "ENTER   Play again" if won else "ENTER   Try again"
	# The stamp lands fast and the retry is live from the first frame.
	stamp.pivot_offset = stamp.size / 2.0
	stamp.rotation = -0.035
	stamp.scale = Vector2.ONE * 1.9
	stamp.modulate.a = 0.0
	var tween: Tween = create_tween().set_parallel(true)
	tween.tween_property(stamp, "scale", Vector2.ONE, 0.13).set_trans(Tween.TRANS_QUAD).set_ease(Tween.EASE_IN)
	tween.tween_property(stamp, "modulate:a", 1.0, 0.08)
