PROGRAM test_verify
  print*, VERIFY("FORTRAN", ["AF"], .true.)           ! 1, found 'F'
  print*, VERIFY("FORTRAN", "FOO", .true.)          ! 3, found 'R'
  print*, VERIFY("FORTRAN", "C++", .true.)          ! 1, found 'F'
  print*, VERIFY("FORTR", "N", .true.)  ! 7, found 'N'
  print*, VERIFY("FORTRAN", "FORTRAN", .true.)      ! 0' found none
END PROGRAM
