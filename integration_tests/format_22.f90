program format_22
  implicit none
  integer :: x, y, z
  x = 21
  y = 100
  z = 0
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
end program format_22