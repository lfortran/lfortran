! With --implicit-interface, a generic procedure called with a derived-type
! component array section as the actual argument (#11668).
module implicit_interface_122_mono
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
end module implicit_interface_122_mono

module implicit_interface_122_mod
  use implicit_interface_122_mono
  implicit none
  type :: bdrypt
     real(8) :: x(2)
  end type
  type(bdrypt), allocatable :: top(:)
  logical :: is_mono
contains
  subroutine test()
    is_mono = .true.
    if (.not. monotonic(top%x(1), size(top))) then
       print *, 'not monotonic'
       is_mono = .false.
    end if
  end subroutine
end module implicit_interface_122_mod

program implicit_interface_122
  use implicit_interface_122_mod
  implicit none
  allocate(top(2))
  top(1)%x = [1.0d0, 0.0d0]
  top(2)%x = [2.0d0, 0.0d0]
  call test()
  if (.not. is_mono) error stop
  top(2)%x(1) = 0.5d0
  call test()
  if (is_mono) error stop
end program implicit_interface_122
