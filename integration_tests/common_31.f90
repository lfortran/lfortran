subroutine cmdo(s)
    common /block/ t
    integer :: t
    integer :: s
    integer :: i
  
    s = 0
    do, i = 1, t
      s = s + t
    end do
  end subroutine
  
program common_31
    common /block/ t
    integer :: t
    integer :: s
  
    t = 3
    call cmdo(s)
    if (s /= 9) error stop
end program common_31