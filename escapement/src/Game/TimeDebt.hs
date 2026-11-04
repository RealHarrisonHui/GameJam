-- | The time-debt state machine: the whole mechanic, in one pure function.
module Game.TimeDebt
  ( stepTime
  , resumeFromPause
  , canFreeze
  ) where

import Game.Types

-- | Advance one simulation tick.
--
-- Input is applied first, then the resulting mode's rate:
--
-- [Normal]   A fresh press enters Frozen and accrues on this same tick.
--            Otherwise the clock moves 1. A held key without a press does nothing.
-- [Frozen]   Still held: debt +1, clock still; hitting the cap flips to Repaying.
--            Released: repays on this same tick (or Normal if debt is zero).
-- [Repaying] Clock +2, debt -1, whatever the key is doing; zero debt returns to Normal.
--
-- N frozen ticks plus N repaying ticks therefore move the clock exactly 2N
-- over 2N ticks, the same as never having frozen. Nothing ever snaps: the clock
-- only moves by 0, 1 or 2.
stepTime :: Config -> TimeInput -> TimeState -> TimeState
stepTime cfg (TimeInput held pressed) ts = case tsMode ts of
  Normal
    | held && pressed -> accrue
    | otherwise       -> advance
  Frozen
    | held            -> accrue
    | tsDebt ts > 0   -> repay
    | otherwise       -> advance
  Repaying
    | tsDebt ts > 0   -> repay
    | otherwise       -> advance
  where
    advance = ts { tsMode = Normal, tsClock = tsClock ts + 1 }
    accrue =
      let debt = tsDebt ts + 1
      in ts { tsMode = if debt >= cfgMaxDebt cfg then Repaying else Frozen, tsDebt = debt }
    repay =
      let debt = tsDebt ts - 1
      in ts { tsMode = if debt <= 0 then Normal else Repaying
            , tsDebt = max 0 debt
            , tsClock = tsClock ts + 2 }

-- | A freeze never survives a pause: the debt falls due on resume.
resumeFromPause :: TimeState -> TimeState
resumeFromPause ts = case tsMode ts of
  Frozen | tsDebt ts > 0 -> ts { tsMode = Repaying }
         | otherwise     -> ts { tsMode = Normal }
  _ -> ts

-- | Would a fresh press start a freeze right now?
canFreeze :: TimeState -> Bool
canFreeze ts = tsMode ts == Normal
