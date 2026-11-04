class_name TimeDebt
extends RefCounted
## The one authoritative temporal controller: the whole mechanic, with no
## rendering or scene dependency. Debt and the shared hazard clock are integers,
## so a borrow/repay cycle balances exactly and nothing ever teleports.

enum Mode { NORMAL, FROZEN, REPAYING }

var mode: Mode = Mode.NORMAL
var debt_ticks: int = 0
var hazard_clock: int = 0
var max_debt_ticks: int = 180


func _init(p_max_debt_ticks: int = 180) -> void:
	max_debt_ticks = p_max_debt_ticks


func reset() -> void:
	mode = Mode.NORMAL
	debt_ticks = 0
	hazard_clock = 0


## Advance one simulation tick. Input is applied first, then the resulting rate:
##   NORMAL    a fresh press enters FROZEN and accrues on this tick; else clock +1.
##   FROZEN    held: debt +1, clock still, the cap flips to REPAYING for the next tick.
##             released: repays on this same tick, or NORMAL if there is no debt.
##   REPAYING  clock +2, debt -1, whatever the key is doing; zero debt means NORMAL.
func step(freeze_held: bool, freeze_pressed: bool) -> void:
	match mode:
		Mode.NORMAL:
			if freeze_held and freeze_pressed:
				_accrue()
			else:
				_advance()
		Mode.FROZEN:
			if freeze_held:
				_accrue()
			elif debt_ticks > 0:
				_repay()
			else:
				_advance()
		Mode.REPAYING:
			if debt_ticks > 0:
				_repay()
			else:
				_advance()


## A freeze never survives a pause: the debt falls due on resume.
func resume_from_pause() -> void:
	if mode == Mode.FROZEN:
		mode = Mode.REPAYING if debt_ticks > 0 else Mode.NORMAL


func can_freeze() -> bool:
	return mode == Mode.NORMAL


func _advance() -> void:
	mode = Mode.NORMAL
	hazard_clock += 1


func _accrue() -> void:
	debt_ticks += 1
	mode = Mode.REPAYING if debt_ticks >= max_debt_ticks else Mode.FROZEN


func _repay() -> void:
	debt_ticks -= 1
	hazard_clock += 2
	mode = Mode.REPAYING if debt_ticks > 0 else Mode.NORMAL
