program format_22
  implicit none
  integer :: x, y, z
  integer(8) :: x8, y8, z8
  integer(1) :: x1, y1, z1
  integer(2) :: x2, y2, z2
  x = 21, y = 100, z = 0
  x8 = 31, y8 = 10, z8 = 0
  x1 = 11, y1 = 10, z1 = 0
  x2 = 9, y2 = 42, z2 = 0
  print *, "b format for integer(4)"
  print '(B0)', x
  print '(B1)', x
  print '(B2)', x
  print '(B3)', x
  print '(B4)', x
  print '(B5)', x
  print '(B6)', x
  print '(B7)', x
  print '(B8)', x
  print '(B9)', x
  print '(B10)', x
  print "(a,b32,a)",'-1 in b32 format: "', -1,'"'
  print "(a,b0 ,a)",'-1 in b0  format: "', -1,'"'
  print "(a,b32 ,a)",'10 in b32  format: "', 10,'"'
  print "(a,b0 ,a)",'11 in b0  format: "', 11,'"'
  print '(B0)', y
  print '(B4)', y
  print '(B5)', y
  print '(B10)', y
  print '(B0)', z
  print '(B4)', z
  print *, "b format for integer(8)"
  print '(B0)', x8
  print '(B7)', x8
  print '(B4)', y8
  print '(B5)', y8
  print '(B10)', z8
  print '(B0)', z8
  print *, "b format for integer(1)"
  print '(B4)', x1
  print '(B5)', x1
  print '(B10)', y1
  print '(B0)', y1
  print '(B4)', z1
  print '(B5)', z1
  print *, "b format for integer(2)"
  print '(B10)', x2
  print '(B0)', x2
  print '(B4)', y2
  print '(B5)', y2
  print '(B10)', z2
  print '(B0)', z2
end program format_22