program format_69
  implicit none

  character(1) :: ca, ca_init(60)
  common // ca(60)

  integer :: i

  ca_init = [(achar(i+39), i=1, 60)]

  open (7, file='fort.7', form='formatted', status='unknown')
  write (7, 77751) ca_init
  rewind (7)

  read (7, 77751) ca

  if (.not. all(ca == ca_init)) error stop

77751 format (60a1)

end program format_69