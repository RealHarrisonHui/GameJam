# One Clock, Two Bodies

A graybox gameplay prototype for testing one mechanic: there are two characters,
but only one receives time at any moment. The frozen character still exists, so
it can hold down a pressure plate while the other one moves.

There is no recording, replay, rewind or autonomous movement. This prototype is
kept separate from any jam submission. Check the competition's rules on AI
assistance, pre-event code and reuse before reusing any of it.

## Build, test and run

You need GHC and cabal-install. [GHCup](https://www.haskell.org/ghcup/) installs
both. This prototype was tested with GHC 9.6.7 and cabal-install 3.12.1.0.

```bash
ghcup install ghc 9.6.7
```

```bash
ghcup install cabal 3.12.1.0
```

```bash
cabal update
```

```bash
cabal test --test-show-details=direct
```

```bash
cabal run one-clock
```

The tests only depend on `base`, so they work even where graphics do not.

### Native dependencies

- **macOS:** nothing to install. GLUT comes with the operating system.
- **Windows:** not tested yet. Gloss needs a GLUT DLL (such as `freeglut.dll`)
  where the program can find it, and that DLL would need to ship with the game.
- **Linux:** not tested yet. Gloss needs freeglut and OpenGL system packages.

## Controls

| Key | Action |
| --- | --- |
| WASD or arrow keys | Move the character that has time, one tile per press |
| Tab | Transfer time to the other character |
| R | Restart |
| Esc | Quit |

On macOS the window's close button does nothing, because Apple's GLUT greys it
out. Use Esc there. The close button on Windows has not been tested yet.

## Rules

- Only the character with time can move. Switching moves nobody.
- A plate is active while either character stands on it, frozen or not.
- Plate P opens door 1 and plate Q opens door 2. Matching colours show the link.
- A door is passable while its plate is held, or while someone is standing in it.
- Walls, closed doors, the map edge and the other character all block movement.
- You win when A is on goal `a` and B is on goal `b`.

## Solution

1. Move A right 2 times, onto P.
2. Press Tab.
3. Move B right 8 times, through door 1 and onto Q.
4. Press Tab.
5. Move A right 9 times, through door 2 and onto goal `a`.
6. Press Tab.
7. Move B right 3 times, onto goal `b`.

## How the code is organised

| File | What it does | Uses Gloss? |
| --- | --- | --- |
| `src/Game/Types.hs` | The data types | No |
| `src/Game/Level.hs` | Levels, drawn as text | No |
| `src/Game/Logic.hs` | The rules | No |
| `src/Game/Input.hs` | One action per key press | No |
| `app/Game/Render.hs` | Draws a game state | Yes |
| `app/Main.hs` | Connects Gloss to the game | Yes |
| `test/Main.hs` | Pure-logic tests | No |

The whole game is one pure function:

```haskell
applyAction :: Level -> Action -> Game -> Game
```

The state holds only positions, who has time, and whether the game is won. Doors
and plates are not stored. `doorOpen` and `plateOccupied` work them out from the
positions whenever they are needed, so they can never get out of step with where
the characters are. A whole playthrough is a fold over a list of actions, which
is how the tests replay the solution.

## Making changes

- **Change the puzzle:** edit the text map in `src/Game/Level.hs`.
- **Change a rule:** edit `src/Game/Logic.hs`, then add a case to `test/Main.hs`.
- **Change the look:** edit `app/Game/Render.hs`.

## Art test: "The Stopped Hour"

A separate, throwaway program for judging how far Haskell art can go. It does not
change the game. It plays the level's real solution as a demo, and the first game
key you press hands the controls to you.

```bash
cabal run art-test
```

To skip the demo and start playing straight away:

```bash
cabal run art-test -- --play
```

```bash
cabal run art-test -- --plain
```

```bash
cabal run art-test -- --measure
```

`--plain` shows the same scene using only Gloss's built-in shapes, as a baseline.
`--measure` prints the window's frame rate, for comparing machines.

To export a video of the demo (this needs `ffmpeg`):

```bash
cabal run -v0 art-test -- --stream | ffmpeg -y -f image2pipe -c:v ppm -framerate 30 -i - -c:v libx264 -crf 16 -pix_fmt yuv420p stopped-hour.mp4
```

Gloss has no anti-aliasing, gradients or shaders, so `art/Art/Raster.hs` paints
images pixel by pixel, once at startup, and Gloss only draws the results.
`art/Art/Scene.hs` holds the artwork, and `art/Art/Visual.hs` holds the animation.
