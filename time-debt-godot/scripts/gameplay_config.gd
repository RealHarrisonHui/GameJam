class_name GameplayConfig
extends Resource
## Every gameplay tunable, in the units the Haskell prototype uses.
## Presentation settings deliberately do not live here.

@export var max_debt_ticks: int = 180
@export var player_speed: float = 3.25          ## pixels per simulation tick
@export var player_radius: float = 13.0
@export var hazard_radius: float = 28.0         ## also the drawn tooth-tip radius
@export var contact_grace: float = 3.0          ## subtracted from the summed radii
@export var shuttle_one_way: int = 15           ## hazard ticks to cross the slot
@export var shuttle_phase: int = 0
@export var gallery_one_way: int = 90           ## hazard ticks to run the gallery
@export var gallery_phase: int = 40


func contact_reach() -> float:
	return player_radius + hazard_radius - contact_grace
