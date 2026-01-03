program main
  implicit none
  character(len=28),allocatable :: textblock(:)
  integer :: i, n
  n = 7
  allocate(textblock(n))
  textblock = [character(len=28) :: &
     '!'//repeat(' ', 27), &
     's=&'//repeat(' ', 24), &
     '& !'//repeat(' ', 24), &
     ' & ! 225'//repeat(' ', 19), &
     '( 0.0,-1.0 ) ! 270'//repeat(' ', 13), &
     '       do i=1,size(vals)'//repeat(' ', 3), &
     ''//repeat(' ', 28) ]

  do i = 1, n
     print *, textblock(i)
  end do
  deallocate(textblock)

end program main
