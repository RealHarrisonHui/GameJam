# Escapement

You hold the brake that stops a whole clock tower. Every second you borrow, the tower takes back at double speed.

A top-down puzzle in Haskell with Gloss: five floors, each adding one idea. The design, story and open questions are in [docs/DESIGN.md](docs/DESIGN.md).

## Run it

Needs GHC 9.6.7 and cabal-install 3.12.1.0 (`cabal.project` pins the compiler). On Windows, Gloss also needs `libfreeglut.dll` on `PATH`. With GHCup's MSYS2 that is:

```bash
pacman -S mingw-w64-x86_64-freeglut
```

```powershell
$env:PATH = "C:\ghcup\msys64\mingw64\bin;$env:PATH"; cabal run exe:escapement
```

## Controls

| Key | Action |
|---|---|
| WASD / arrows | Move |
| Hold Space | Brake: stops every wheel and builds debt, 3 seconds at most |
| P | Pause |
| R | Restart the floor |
| Enter | Continue |
| Esc | Quit |

## Development flags

| Flag | Effect |
|---|---|
| `--level=N` | Start at floor N's note |
| `--demo` | Watch the scripted run of every floor, then take over |
| `--demo-stop=N` | As `--demo`, holding the picture after N ticks |
| `--no-hints`, `--no-ghost` | Remove teaching or landing ghosts, for playtests |
| `--log` | Print one line per death, clear and pause |
| `--reduced-effects` | No trails, sparks or moving decoration |
| `--fps`, `--trace` | Print frame rate or per-frame state |

## Tests

```bash
cabal test spec --test-show-details=direct
```

```bash
cabal test probe --test-show-details=direct
```

`spec` checks the rules and replays a run of every floor through the real logic. `probe` is slower: it searches each floor for a route that avoids the brake, and checks that no single hold crosses two shafts on floor 3.

## How it fits together

```
src/Game/   pure core: no Gloss, no IO
  TimeDebt   the mechanic, in one function
  Logic      one tick of one floor
  Campaign   title, notes, floors, ending
  Level      the floors, as data
  Story      every word of story, in one file
  Solution   scripted runs and bots, for tests and --demo
  Input, Collision, Fx, Types
app/        Gloss only: Main (IO), Render, Art
test/       plain assertions and route search
```
