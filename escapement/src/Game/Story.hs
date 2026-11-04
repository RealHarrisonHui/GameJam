-- | Every word of story, in one place, so it can be rewritten to fit a jam theme
-- without touching a rule. Lettering has no question mark: avoid that character.
module Game.Story
  ( gameTitle
  , gameTagline
  , NoteText (..)
  , floorUndercroft
  , floorShafts
  , floorCountingRoom
  , floorGallery
  , floorMainspring
  , floorEscapeWheel
  , endingLines
  , endingSignOff
  ) where

gameTitle :: String
gameTitle = "ESCAPEMENT"

gameTagline :: [String]
gameTagline =
  [ "YOU CAN STOP DANGER NOW,"
  , "BUT THE WORLD RECLAIMS EVERY SECOND YOU BORROW."
  ]

-- | One floor's words: its name, the note before it, and a hint for the stuck.
data NoteText = NoteText
  { noteTitle :: String
  , noteBrief :: [String]
  , noteHint  :: [String]
  }

floorUndercroft :: NoteText
floorUndercroft = NoteText
  { noteTitle = "THE UNDERCROFT"
  , noteBrief =
      [ "MIDNIGHT. ST. ORRIN'S TOWER."
      , "THE GREAT CLOCK HAS SLIPPED, AND EVERY WHEEL RUNS WILD."
      , "YOU ARE THE ONLY APPRENTICE LEFT INSIDE."
      , "THE FOREMAN'S BRAKE IS IN YOUR HAND. IT HOLDS THE WHOLE TOWER STILL."
      ]
  , noteHint =
      [ "TOO FAST TO DODGE. HOLD SPACE BEFORE YOU REACH IT."
      , "IT STAYS STOPPED FOR AS LONG AS YOU KEEP HOLDING."
      ]
  }

floorShafts :: NoteText
floorShafts = NoteText
  { noteTitle = "THE SHAFTS"
  , noteBrief =
      [ "A NOTE IN THE FOREMAN'S HAND, PINNED BESIDE THE DOOR:"
      , "THE BRAKE STOPS THE CLOCK. IT DOES NOT STOP THE TEETH."
      , "A STOPPED WHEEL STILL CUTS, AND IT STOPS WHEREVER YOU CATCH IT."
      ]
  , noteHint =
      [ "CATCH THE WHEEL NEAR THE END OF ITS TRACK."
      , "CAUGHT IN THE MIDDLE, IT BLOCKS THE WAY. LET GO AND TRY AGAIN."
      ]
  }

floorCountingRoom :: NoteText
floorCountingRoom = NoteText
  { noteTitle = "THE COUNTING ROOM"
  , noteBrief =
      [ "THE BRAKE IS NOT A GIFT. IT IS A LOAN."
      , "LET GO AND THE CLOCK RUNS DOUBLE UNTIL IT HAS IT ALL BACK."
      , "UNTIL THEN THE BRAKE WILL NOT ANSWER. DO NOT ARGUE WITH IT."
      ]
  , noteHint =
      [ "NO SINGLE HOLD CROSSES TWO SHAFTS."
      , "CROSS ONE, LET GO, AND WAIT FOR THE GREEN PULSE."
      ]
  }

floorGallery :: NoteText
floorGallery = NoteText
  { noteTitle = "THE GALLERY"
  , noteBrief =
      [ "THE FERRY THAT SHOULD HAVE LEFT. THE DOCTOR AT THE WRONG DOOR."
      , "THREE SECONDS HERE, THREE THERE. THE TOWN NEVER NOTICED."
      , "THIRTY YEARS I BORROWED, AND I LEARNED TO PUT OFF THE PAYING."
      , "TONIGHT IT ALL FELL DUE."
      ]
  , noteHint =
      [ "NO ONE PASSES A WHEEL IN THE GALLERY. LET IT PASS YOU."
      , "STOP IT BEYOND THE GREEN ALCOVE, THEN LET GO INSIDE."
      ]
  }

floorMainspring :: NoteText
floorMainspring = NoteText
  { noteTitle = "THE MAINSPRING"
  , noteBrief =
      [ "THE MAINSPRING IS AT THE FAR END OF THIS FLOOR."
      , "YOU NEVER GAIN A SECOND, APPRENTICE."
      , "YOU ONLY CHOOSE WHEN TO MEET WHAT IS COMING."
      , "REACH THE SPRING. THEN DO THE ONE THING I NEVER COULD."
      ]
  , noteHint =
      [ "ONE CLOCK RULES BOTH WHEELS. WATCH THE GHOSTS BEFORE YOU LET GO."
      , "PAY THE SHAFT'S DEBT WHILE YOU WALK, THEN STOP THE LONG WHEEL."
      ]
  }

floorEscapeWheel :: NoteText
floorEscapeWheel = NoteText
  { noteTitle = "THE ESCAPE WHEEL"
  , noteBrief =
      [ "THREE PINS HOLD THE ESCAPE WHEEL. PULL ALL THREE AND THE WAY UP OPENS."
      , "TAKE THEM IN ANY ORDER YOU LIKE."
      , "EVERY DOOR ANSWERS TO THE CLOCK. STOP THE CLOCK AND IT STAYS AS IT IS."
      , "BORROW LITTLE. I NEVER MANAGED IT."
      ]
  , noteHint =
      [ "BRAKE WHILE A DOOR IS OPEN AND IT STAYS OPEN."
      , "THE LONG CORRIDOR COSTS THE MOST. LET THE WHEEL GET FAR AWAY FIRST."
      ]
  }

endingLines :: [String]
endingLines =
  [ "YOU LET GO."
  , "THE TOWER RAN DOUBLE UNTIL DAWN, AND STRUCK EVERY HOUR IT WAS OWED."
  , "THE TOWN WOKE A LITTLE OLDER, AND THE CLOCK KEPT TRUE TIME."
  ]

endingSignOff :: String
endingSignOff = "ON THE WORKBENCH: THE FOREMAN'S KEYS, AND A LEDGER MARKED PAID."
