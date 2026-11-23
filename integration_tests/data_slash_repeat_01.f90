program data_slash_repeat_01
  integer a(3) /1, 2, 3/
  integer b(3) /3*1/
  integer c(5) /2*0, 5, 2*7/
  integer, parameter :: n = 4
  integer d(5) /n*0, 1/
  if (any(a /= [1, 2, 3])) error stop
  if (any(b /= [1, 1, 1])) error stop
  if (any(c /= [0, 0, 5, 7, 7])) error stop
  if (any(d /= [0, 0, 0, 0, 1])) error stop
end program
