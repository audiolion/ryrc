# ryrc
Haskell IRC bot that handles unit conversions.

Currently supports metric unit conversions from one base to another base.

ex./
  !convert 100 millimeters to meters
    0.1m
  !convert -v 100 millimeters to centimeters
    0.1 centimeters

-v is a verbose flag to fully display the unit prefix and unit base.
