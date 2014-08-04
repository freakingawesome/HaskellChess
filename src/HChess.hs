import System.Environment

main :: IO()
main = getArgs >>= print . derp . head

derp s = "DERP! " ++ s

