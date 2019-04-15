-- The diagram to be drawn, with features tagged by strings.
prettyHouse :: QDiagram Cairo V2 Double [String]
prettyHouse = house
  where
    roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
    door    = rect 0.2 0.4 # fc red
    handle  = circle 0.02  # fc black
    wall    = square 1     # fc yellow
    chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
            -- closeTrail # strokeT # fc green
            -- centerX
            -- named "chimney"
    smoke = mconcat
      [ circle 0.05 # translate v
      | v <- [ zero, 0.05 ^& 0.15 ]
      ]
      -- fc grey
