program char_fn2
  implicit none

! Compile with --implicit-interface

  character(2) :: cfn, s

  s = 'xx'
  s = cfn (-1)  ! Not using substring notation, and cfn is not an array.  So cfn is a function.
  print *, 's = ', s, merge (' : pass', ' : fail', s == 'ng')
  if (s /= 'ng') error stop

  s = 'xx'
  s = cfn (0)
  print *, 's = ', s, merge (' : pass', ' : fail', s == 'zr')
  if (s /= 'zr') error stop

  s = 'xx'
  s = cfn (1)
  print *, 's = ', s, merge (' : pass', ' : fail', s == 'pl')
  if (s /= 'pl') error stop

 print *, 'all tests passed'

end program char_fn2

function cfn (i)
  character(2) :: cfn
  integer :: i

  print *, 'cfn: i =', i

  select case (i)
  case (:-1)
    cfn = 'ng'
  case (1:)
    cfn = 'pl'
  case default
    cfn = 'zr'
  end select

end function