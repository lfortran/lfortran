! TRIM of character array elements in an implied-DO output list, with T and
! colon editing in the format (#11408).
program format_120
  implicit none
  real(kind(1e0)) :: h1 = huge(1e0)
  real(kind(1d0)) :: h2 = huge(1d0)
  integer :: j
  character :: ninfkinds(2)*11
  character(80) :: line
  write(ninfkinds, "(ES11.3E3)") h1, h2
  if (ninfkinds(1) /= " 3.403E+038") error stop
  if (ninfkinds(2) /= " 1.798E+308") error stop
  print "(A,T36,2(A11,:,1X))", " Single and double precision huge:", &
       (trim(ninfkinds(j)), j=1,2)
  write(line, "(A,T36,2(A11,:,1X))") " Single and double precision huge:", &
       (trim(ninfkinds(j)), j=1,2)
  print "(A)", trim(line)
  if (line /= " Single and double precision huge:  3.403E+038  1.798E+308") error stop
end program format_120
