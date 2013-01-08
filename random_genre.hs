import Control.Monad.Random

choice l = do
	i <- getRandomR (0,length l-1)
	return $ l !! i

a +++ b = do
	x <- a
	y <- b
	return (x ++ " " ++ y)

genre = do
	r <- choice [prim, prim +++ genre, prefix +++ genre]
	r

primitives = [ "pop",
  "blues",
  "classical",
  "core",
  "country",
  "dance",
  "disco",
  "electro",
  "folk",
  "funk",
  "gangsta",
  "grunge",
  "hip hop",
  "industral",
  "metal", 
  "opera", 
  "punk", 
  "regae", 
  "rock" ]

relations = ["alternative",
  "avant-garde", 
  "classic",  
  "indie", 
  "new",  
  "nu", 
  "post", 
  "progressive"]
styles = ["acid",
  "celtic", 
  "christian", 
  "dark", 
  "garage", 
  "gothic", 
	"gypsy", 
	"hard", 
	"melodic", 
	"pagan", 
	"psychadelic", 
  "speed"]
nationality = ["brit", "J", "K"]

prefix = choice (nationality ++ styles ++ relations)

prim = choice primitives

main = do
	x <- evalRandIO genre
	putStrLn x
