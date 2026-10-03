program write_26
  implicit none
  integer :: u
  character(len=10) :: line
  character(len=3) :: adv_yes, adv_no

  open(newunit=u, status='scratch', form='formatted', action='readwrite')

  ! Test 1: advance=variable containing 'YES' should advance the record
  adv_yes = 'YES'
  write(u, '(A)', advance=adv_yes) 'AB'
  write(u, '(A)', advance=adv_yes) 'CD'
  rewind(u)
  read(u, '(A)') line
  if (trim(line) /= 'AB') error stop
  read(u, '(A)') line
  if (trim(line) /= 'CD') error stop

  ! Test 2: advance=variable containing 'NO' should not advance the record
  rewind(u)
  adv_no = 'NO'
  write(u, '(A)', advance=adv_no) 'EF'
  write(u, '(A)', advance=adv_no) 'GH'
  write(u, '(A)') ''
  rewind(u)
  read(u, '(A)') line
  if (trim(line) /= 'EFGH') error stop

  close(u)
  print *, "PASS"
end program
