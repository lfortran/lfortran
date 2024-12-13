program main
  implicit none
  integer, parameter :: str_len = 256 
  character(len=str_len), allocatable :: textblock(:)
  textblock = [character(len=str_len) :: &
      '!', &
      's=&', &
      '& !', &
      ' & ! 225', &
      '( 0.0,-1.0 )]   ! 270', &
      '       do i=1,size(vals)', &
       '' ]

end program main
