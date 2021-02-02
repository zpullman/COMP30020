-- Zac Pullman, 695145
--
-- This project implements functions for "composer" and  an agent for
-- "performer" in "The Game of Musician"
--
-- Musician game, one player is the composer and the other is the performer.
-- The composer begins by selecting a three-pitch musical chord, where each
-- pitch comprises a musical note, one of A to G, and an octave, one of 1, 2,
-- or 3. This chord will be the target for the game. The order of pitches in the
-- target is irrelevant, and no pitch may appear more than once.
-- This game does not include sharps or flats, and no more or less than three
-- notes may be included in the target.


module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Data.Ord
data Pitch = Pitch Char Char deriving (Eq)

-- A chord is 3 distinct pitches
type Chord = [Pitch]
type GameState = [Chord]

-- showPitch defines how we display a Pitch
-- converts a Pitch to a String output
instance Show Pitch where show = showPitch
showPitch :: Pitch -> String
showPitch (Pitch note octave) = [note]++[octave]

-- toPitch converts a String to a pitch if it's in the form of a pitch
-- it outputs Just pitch, otherwise it outputs Nothing
toPitch :: String -> Maybe Pitch
toPitch pitch
    | validPitch pitch = Just (Pitch (pitch!!0) (pitch!!1))
    | otherwise = Nothing

-- validPitch checks if a string is a Pitch, i.e. has length 2, a note
-- A to G and octave 1 to 3.
validPitch :: String -> Bool
validPitch pitch =
    length pitch == 2 && pitch!!0 `elem`['A'..'G'] && pitch!!1 `elem`['1'..'3']

-------------------- PERFORMER -------------------------------------------------

-- initialGuess function sets up the game state and gives an initial guess to
-- start the game for the performer.
-- The initial guess used can be any guess that has 3 distinct notes and 2
-- octaves. This gives the most information on average.
-- The game state is made by getting all the distinct chords minus the initial
-- guess
initialGuess :: ([Pitch],GameState)
initialGuess = (guess, initialState)
    where
        pitchs = [ Pitch note octave | note <- ['A'..'G'], octave <- ['1'..'3']]
        chords = [ chord | chord <- subsequences pitchs, length chord == 3]
        guess = [Pitch 'A' '1', Pitch 'B' '2', Pitch 'C' '2']
        initialState = chords \\ [guess]

-- Input: [pitch] is the last guess, and GameState is a list of possible chords
-- that we have been keeping track of. (Int,Int,Int) is the feedback from the
-- last guess.
-- Output: [Pitch] is our next guess and Gamestate is the updated state that has
-- been trimmed
-- Trimming done by only keeping chords that would give the same score as our
-- previous guess i.e. scores that match the score of the target
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (lastGuess, state) score = (nextGuess, state')
    where
        state' = delete lastGuess [chord | chord <- state
                                  ,feedback chord lastGuess == score]
        nextGuess = bestGuess state'

-- bestGuess: makes a tuple of (Possible target, Average remaining candidates),
-- which is then sorted by average remaining candidates. We select the target
-- that leaves the least amount of candidates on average. This will reduce the
-- size of the solution space by the most on average.
bestGuess :: GameState -> Chord
bestGuess state = fst (head guess)
    where
        guess = [ (target, avgRemCands)| target <- state
                ,let avgRemCands = avgRemCandidates target (state \\ [target])]
        bestguess = sortBy (comparing snd) guess

-- avgRemCandidates: takes a guess and gamestate and calculates how many
-- candidates will be left in the solution space on average if that guess is
-- incorrect.
avgRemCandidates :: Chord -> GameState -> Double
avgRemCandidates target state = sum [(numOutcomes / totalOutcomes)*numOutcomes |
                                    outcomes <- possOutcomes
                                    ,let numOutcomes = fromIntegral (length outcomes)
                                    ]
    where
        scores = [ score | guess <- state, let score = feedback target guess]
        possOutcomes = group (sort scores)
        totalOutcomes = fromIntegral (length scores)



--------------------- COMPOSER -------------------------------------------------

-- feedback takes a target and a guess and returns a tuple of
-- (correct, notes, octaves)
-- correct: number of correct pitches in guess
-- notes: number of correct notes in guess
-- octaves: number of correct octaves
-- In counting correct notes and octaves, multiple occurrences in the guess are
-- only counted as correct if they also appear repeatedly in the target.
-- Correct pitches are not also counted as correct notes and octaves.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess  = (correct, rightNotes, rightOctaves)
    where
        guess' = nub guess
        correct = length $ intersect guess' target
        numPitches = length guess'
        rightNotes = numPitches
                    - length (deleteFirstsBy noteEq guess' target)
                    - correct
        rightOctaves = numPitches
                    - length (deleteFirstsBy octaveEq guess' target)
                    - correct

-- NoteEq checks if the notes of two pitches are equal, returns true when
-- two pitches have the same note
noteEq :: Pitch -> Pitch -> Bool
noteEq (Pitch note1 _) (Pitch note2 _) = note1 == note2

-- octaveEq checks if the octaves of two pitches are equal, returns true when
-- two pitches have the same octave
octaveEq :: Pitch -> Pitch -> Bool
octaveEq (Pitch _ oct1) (Pitch _ oct2) = oct1 == oct2
