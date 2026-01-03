program test
implicit none
integer :: x(2, 2) = reshape([1, 2, &
                              3, 4], [2, 2])

where(x == 1)
  x = 2
else where(x == 2)
  x = 3
else where
  x = x * 2
endwhere

if (all(x == reshape([2, 3, &
                      6, 8], [2, 2]))) then
  print *, 'pass'
else
  print *, 'fail'
end if

where(x == 2)
  x = 3
elsewhere(x == 3)
  x = 4
elsewhere(x == 6)
  x = x * 2
end where

if (all(x == reshape([3,  4, &
                      12, 8], [2, 2]))) then
  print *, 'pass'
else
  print *, 'fail'
end if
end program test