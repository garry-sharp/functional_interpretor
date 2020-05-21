import System.IO.Unsafe

foo :: Int -> Maybe Int
foo x
	| x < 5 = Just x
	| otherwise = Nothing

foo2 :: Int -> IO Int
foo2 x = do
	putStrLn ("Hello my value is " ++ (show  x))
	return x

times2 :: IO Int -> Int
times2 x = (unsafePerformIO x) * 2
