--define or data structure for interval of integers (horizontal 'stripes of the space')
type Interval = (Integer, Integer)
type Row = [Interval]
--define a 'row' of the space as a list of horizontal intervals
--define the space shape by a list of rows going up and down
--define the space overall by assigning a patch to each valid index in the shape
--create a Gen and an Arbitrary instance for the spaces
--create a function to generate a diagram from a space

main :: IO ()
main = do
  let i1 = (5, 6) :: Interval
  let i2 = (8, 10) :: Interval
  putStrLn "First Interval:"
  print i1
  putStrLn "Second Interval:"
  print i2
  let r1 = [i1, i2]
  putStrLn "Row 1 (Bottom row): "
  print r1
