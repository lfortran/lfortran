program main
  integer :: x(2)
  call sub(x)
  contains
  subroutine sub(x)
    integer :: x(2)
    integer :: arr(sum(kind(x)))
  end subroutine sub
end program main