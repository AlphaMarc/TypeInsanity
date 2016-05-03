module Solution (cubes, rot, solutions) where

import Prelude hiding (flip)

cubes = ["BGWGBR", "WGBWRR", "GWRBRR", "BRGGWW"]

cubes2 = ["BGWGBR", "WGBWRR"]
-- Rotate a cube 90 degrees over its Z-axis, leaving up and down in place.
rot [u, f, r, b, l, d] = [u, r, b, l, f, d]
-- Twist a cube around the axis running from the upper-front-right
-- corner to the back-left-down corner.
twist [u, f, r, b, l, d] = [f, r, u, l, d, b]
-- Exchange up and down, front and left, back and right.
flip [u, f, r, b, l, d] = [d, l, b, r, f, u]
-- Compute all 24 ways to orient a cube.
orientations c = [c3 | c1 <- [c, rot c, rot (rot c), rot (rot (rot c))],
  c2 <- [c1, twist c1, twist (twist c1)],
  c3 <- [c2, flip c2]]
-- Compute which faces of a cube are visible when placed in a pile.
visible [u, f, r, b, l, d] = [f, r, b, l]
-- Two cubes are compatible if they have different colours on every
-- visible face.
compatible c c1 = and [x /= x1 | (x, x1) <- zip (visible c) (visible c1)]
-- Determine whether a cube can be added to pile of cubes, without
-- invalidating the solution.
allowed c cs = and [compatible c c1 | c1 <- cs]
-- Return a list of all ways of orienting each cube such that no side of
-- the pile has two faces the same.
solutions [] = [[]]
solutions (c:cs) = [c1 : cs1 | cs1 <- solutions cs,
  c1 <- orientations c,
  allowed c1 cs1]
