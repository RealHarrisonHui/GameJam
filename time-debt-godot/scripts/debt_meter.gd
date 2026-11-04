class_name DebtMeter
extends Control
## The three-second debt ledger: three cells that fill cold while borrowing and
## drain red while repaying. Drawn from integer debt ticks each time they change.

const COLD := Color(0.62, 0.9, 1.0)
const COLD_DEEP := Color(0.2, 0.46, 0.66)
const HOT := Color(0.96, 0.34, 0.24)
const HOT_DEEP := Color(0.5, 0.09, 0.08)
const IDLE := Color(0.075, 0.06, 0.05)
const LINE := Color(0.77, 0.6, 0.27)

var debt_ticks: int = 0
var max_ticks: int = 180
var mode: TimeDebt.Mode = TimeDebt.Mode.NORMAL
var warn_phase: float = 0.0


func set_state(p_debt: int, p_max: int, p_mode: TimeDebt.Mode) -> void:
	if p_debt == debt_ticks and p_mode == mode and p_max == max_ticks:
		return
	debt_ticks = p_debt
	max_ticks = p_max
	mode = p_mode
	queue_redraw()


func _draw() -> void:
	var cells: int = 3
	var gap: float = 5.0
	var w: float = (size.x - gap * float(cells - 1)) / float(cells)
	var fraction: float = float(debt_ticks) / float(max_ticks)
	var fill_px: float = fraction * float(cells)
	var frozen: bool = mode == TimeDebt.Mode.FROZEN
	for i: int in range(cells):
		var r := Rect2(float(i) * (w + gap), 0.0, w, size.y)
		draw_rect(r, IDLE)
		var amount: float = clampf(fill_px - float(i), 0.0, 1.0)
		if amount > 0.0:
			var filled := Rect2(r.position, Vector2(r.size.x * amount, r.size.y))
			draw_rect(filled, COLD_DEEP if frozen else HOT_DEEP)
			var bright := Rect2(filled.position + Vector2(0, 2), Vector2(filled.size.x, filled.size.y * 0.46))
			draw_rect(bright, COLD if frozen else HOT)
		draw_rect(r, LINE, false, 1.0)
