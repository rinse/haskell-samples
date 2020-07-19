{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module MusicComposer where

import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable           (fold)
import           Data.Functor.Identity
import           System.Process          (callProcess)

-- basic tools
type Seconds = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float
type BPM = Float
type Volume = Float

pitchStandard :: Hz
pitchStandard = 440.0

sampleRate :: Hz
sampleRate = 44100.0

frequency :: Volume -> Hz -> Seconds -> [Pulse]
frequency volume hz duration = (* volume) <$> zipWith3 (\x y -> (x * y *)) attack release freq
    where
    step = (hz * 2 * pi) / sampleRate
    freq :: [Pulse]
    freq = sin . (* step) <$> [0.0 .. sampleRate * duration]
    attack :: [Pulse]
    attack = min 1.0 <$> [0, 0.001 ..]
    release :: [Pulse]
    release = reverse $ take (length freq) attack

beatsToSeconds :: BPM -> Beats -> Seconds
beatsToSeconds bpm = (60.0 / bpm *)

semitonesToHz :: Semitones -> Hz
semitonesToHz n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Volume -> BPM -> Semitones -> Beats -> [Pulse]
note volume bpm n = frequency volume (semitonesToHz n) . beatsToSeconds bpm

chord :: [Pulse] -> [Pulse] -> [Pulse]
chord = zipWith (+)

chord' :: [[Pulse]] -> [Pulse]
chord' = foldr chord [0.0, 0.0 ..]

-- ScoreT monad and its actions
newtype ScoreT m a = ScoreT
    { unScoreT :: ReaderT Volume (ReaderT BPM (WriterT [Pulse] m)) a
    } deriving (Functor, Applicative, Monad)
type Score = ScoreT Identity

runScoreT :: ScoreT m a -> Volume -> BPM -> m (a, [Pulse])
runScoreT (ScoreT score) volume bpm = runWriterT $ runReaderT (runReaderT score volume) bpm

runScore :: Score a -> Volume -> BPM -> (a, [Pulse])
runScore = (fmap . fmap) runIdentity . runScoreT

instance MonadTrans ScoreT where
    lift = ScoreT . lift . lift . lift

askVolume :: Monad m => ScoreT m Volume
askVolume = ScoreT ask

askBPM :: Monad m => ScoreT m BPM
askBPM = ScoreT $ lift ask

tellPulse :: Monad m => [Pulse] -> ScoreT m ()
tellPulse = ScoreT . lift . lift . tell

noteT :: Monad m => Semitones -> Beats -> ScoreT m ()
noteT semitones beats = do
    v <- askVolume
    bpm <- askBPM
    tellPulse $ note v bpm semitones beats

notesT :: Monad m => [Semitones] -> Beats -> ScoreT m ()
notesT semitones beats = do
    v <- askVolume
    bpm <- askBPM
    tellPulse . chord' $ (flip (note v bpm) beats <$> semitones)

localVolume :: Monad m => (Volume -> Volume) -> ScoreT m a -> ScoreT m a
localVolume mapper = ScoreT . local mapper . unScoreT

localBPM :: Monad m => (BPM -> BPM) -> ScoreT m a -> ScoreT m a
localBPM mapper = ScoreT . ReaderT . fmap (local mapper) . runReaderT . unScoreT

mergeTracks :: Monad m => ScoreT m a -> ScoreT m b -> ScoreT m (a, b)
mergeTracks t1 t2 = ScoreT . ReaderT $ \volume -> ReaderT $ \bpm -> WriterT $ do
    (x, track1) <- runScoreT t1 volume bpm
    (y, track2) <- runScoreT t2 volume bpm
    let merged = chord track1 track2
    return ((x, y), merged)

-- Labeled semitones, C major/A minor
a :: Float -> Semitones
a x = 12 * x - 48.0 -- -48.0 == A0

b :: Float -> Semitones
b x = a x + 2.0 -- +2.0 == B4

c :: Float -> Semitones
c x = a x - 9.0 -- -9.0 == C4

d :: Float -> Semitones
d x = a x - 7.0 -- -7.0 == D4

e :: Float -> Semitones
e x = a x - 5.0 -- -5.0 == E4

f :: Float -> Semitones
f x = a x - 4.0 -- -4.0 == F4

g :: Float -> Semitones
g x = a x - 2.0 -- -2.0 == G4

-- Theme of Tetris
{-# ANN melody ("HLint: ignore Reduce duplication") #-}
melody :: Score ()
melody = do
    noteT (e 5) 1.0
    noteT (b 4) 0.5
    noteT (c 5) 0.5
    noteT (d 5) 1.0
    noteT (c 5) 0.5
    noteT (b 4) 0.5

    noteT (a 4) 1.0
    noteT (a 4) 0.5
    noteT (c 5) 0.5
    noteT (e 5) 1.0
    noteT (d 5) 0.5
    noteT (c 5) 0.5

    noteT (b 4) 1.0
    noteT (b 4) 0.5
    noteT (c 5) 0.5
    noteT (d 5) 1.0
    noteT (e 5) 1.0

    noteT (c 5) 1.0
    noteT (a 4) 1.0
    noteT (a 4) 2.0

bass :: Score ()
bass = do
    replicateM_ 4 $ do
        noteT (e 2) 0.5
        noteT (e 3) 0.5

    replicateM_ 4 $ do
        noteT (a 2) 0.5
        noteT (a 3) 0.5

    replicateM_ 2 $ do
        noteT (a 2 - 0.5) 0.5
        noteT (a 3 - 0.5) 0.5
    replicateM_ 2 $ do
        noteT (e 2) 0.5
        noteT (e 3) 0.5

    replicateM_ 4 $ do
        noteT (a 2) 0.5
        noteT (a 3) 0.5

-- Wave to play
wave :: [Pulse]
wave = snd $ runScore (mergeTracks melody bass) volume bpm
    where
    volume = 0.5
    bpm = 120.0

-- Other utilities
outputFilePath :: FilePath
outputFilePath = "output"

save :: FilePath -> IO ()
save filePath = B.writeFile filePath . B.toLazyByteString . fold $ B.floatLE <$> wave

play :: FilePath -> IO ()
play filepath = callProcess "ffplay" ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", filepath]

saveAndPlay :: IO ()
saveAndPlay = do
    save outputFilePath
    play outputFilePath
