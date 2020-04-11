# algorithm: aligned. human: aligned.
call(
  x   = 1, kdd  =  2,
  xy  = 2, n    = 33,
)

# without trailing comma
call(
  x   = 1, kdd  =  2,
  xy  = 2, n    = 33
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, kdd  =  2,
  xy = 2, n    = 33,
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, kdd  =  2,
  xy = 2, n    = 33,
)

# algorithm: not aligned (spacing around =). human: aligned (fix: spacing around =).
call(
  x = 1, kdd = 2,
  xy = 2, n = 33,
)

# algorithm: not aligned. human: not aligned.
call(
  x = 1, kdd = 2,
  xy = 2, n = 33,
)

# algorithm: not aligned. human: not aligned.
call(
  x = 1, kdd = 2,
  xy = 22, n = 33,
)

# algorithm: not aligned. human: not aligned.
call(
  x = 1, d = 2,
  xy = 22, n = 33,
)


# algorithm: aligned. human: aligned.
call(
  x  = 1,   kdd  =  2, k = "abc",
  xy = 2,   n    = 33, z = "333"
)


# algorithm: aligned. human: aligned.
call(
  x  = 1,
  xy = 2,   n    = 33, z = "333"
)

# algorithm: aligned. human: aligned.
call(
  x  = 1, n = 33, z = "333",
  xy = 2,
)

# aligned. when spaces are spread accross different nests
call(
  k =  ff("pk"), k  = 3,
  b = f(-g),     22 + 1,
  44,               323
)

# aligned. when spaces are spread accross different nests
call(
  k =  ff("pk"), k  = 3,
  b = f(-g),     22 + 1,
  44,               323,
)

# no trailing
call(
  k =  ff("pk"), k  = 3,
  b = f(-g),     22 + 1,
  44
)

# aligned: fewest arguments not on last line
call(
  44,
  k =  ff("pk"), k  = 3,
  b = f(-g),     22 + 1,
)

# aligned: fewest arguments not on last line
call(
  k =  ff("pk"), k  = 3,
  44,
  b = f(-g),     22 + 1,
)



# if all col1 arguments are named, col1 must also be aligned
# not aligned
fell(
  x = 1,
  y = 23,
  zz = NULL
)

# aligned
fell(
  x =     1,
  y =    23,
  zz = NULL
)

# aligned but comma in the wrong line
call(
  a  =  2,
  bb =  3,
)


# aligned (comments)
call(
  a  =  2, x = 111,
  # another
  bb =  3, # hi
)

# aligned (comments)
call(
  a  =  2, x = 111,
  bb =  3, # hi
)

# aligned (comments)
call(
  # another one
  a  =  2, x = 111,
  bb =  3, # hi
)

# aligned (comments)
call(
  # another one
  a  =  2, x = 111,
  bb =  3 # hi
)

# not aligned (comments)
call(
  a = 2, x = 111,
  bb = 3, # hi
)

# not aligned (comments)
call(
  # another one
  a = 2, x = 111,
  bb = 3,
  # hi
)

# If a call is mult-line, it can't be aligned (also, it would not currently
# not be ideopotent because first bace would be moved up without alignment and
# in the second step, because all arguments are named and there is no alignment,
# the extra spaces before `=` as of 29a010064257fa1a9caf32d182e7ee62008de98a.
call(
  x  = 95232,
  y  = f(),
)
