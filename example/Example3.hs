import LambdaSound

main :: IO ()
main = do
    nachtmusik <- loadWav "sample-sounds/mozart-kleine-nachtmusik.wav"
    play 44100 0.4 $ takeSound 10 $ dropSound 10 nachtmusik
    
