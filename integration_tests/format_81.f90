program format_81
  implicit none

  character(10) :: s

  ! Integer overflow: i5.5 requires 5 digits + sign = 6 chars, exceeds width 5
  write (s, '(s,i5.5)') -12
  if (s /= '*****') error stop

  ! Iw.0 with value 0 must produce all blanks (Fortran standard 13.7.2.1)
  write (s, '(i5.0)') 0
  if (s /= '     ') error stop

  write (s, '(s,i5.0)') 0
  if (s /= '     ') error stop

  write (s, '(sp,i5.0)') 0
  if (s /= '     ') error stop

end program format_81
