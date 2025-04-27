program format_22
  implicit none
  integer :: x
  x = 21
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
end program format_22