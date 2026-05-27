module monotonicmod
  implicit none
  interface monotonic
     module procedure monotonic_dble
  end interface monotonic
contains
  function monotonic_dble(x, n)
    logical :: monotonic_dble
    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    monotonic_dble = .true.
    if (n == 1) return
    if (any(x(2:n) <= x(1:n-1))) monotonic_dble = .false.
  end function monotonic_dble
end module monotonicmod

module m
  use monotonicmod
  implicit none
  type :: bdrypt
     real(8) :: x(2)
  end type
  type(bdrypt), allocatable :: top(:)
contains
  subroutine test()
    if (.not. monotonic(top%x(1), size(top))) then
       print *, 'not monotonic'
    end if
  end subroutine
end module m

program main
  use m
  implicit none
  
  allocate(top(2))
  top(1)%x = [1.0d0, 0.0d0]
  top(2)%x = [2.0d0, 0.0d0]
  
  call test()
  print *, "OK"
end program main