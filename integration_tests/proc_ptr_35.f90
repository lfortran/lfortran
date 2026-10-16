! An internal procedure of a main program is a valid procedure pointer target
! only at run time (C1033), also when it is the interface (#14968). A module
! or external procedure is a valid initial target (Fortran 2023 C1519).
module proc_ptr_35_mod
  implicit none
contains
  subroutine twice(i, r)
    integer, intent(in) :: i
    integer, intent(out) :: r
    r = 2*i
  end subroutine twice
end module proc_ptr_35_mod

program proc_ptr_35
  use proc_ptr_35_mod, only: twice
  implicit none
  interface
    subroutine thrice(i, r)
      integer, intent(in) :: i
      integer, intent(out) :: r
    end subroutine thrice
  end interface
  type :: callback_a
    procedure(a), pointer, nopass :: f
  end type
  procedure(twice), pointer :: p => thrice
  type(callback_a) :: y
  integer :: r, n

  call p(3, r)
  if (r /= 9) error stop 1
  p => twice
  call p(3, r)
  if (r /= 6) error stop 2

  n = 0
  y = callback_a(a)
  call y%f(7, r)
  if (r /= 7 .or. n /= 1) error stop 3
  y%f => a
  call y%f(6, r)
  if (r /= 6 .or. n /= 2) error stop 4
  print *, r, n

contains

  subroutine a(i, r)
    integer, intent(in) :: i
    integer, intent(out) :: r
    n = n + 1
    r = i
  end subroutine a

end program proc_ptr_35

subroutine thrice(i, r)
  integer, intent(in) :: i
  integer, intent(out) :: r
  r = 3*i
end subroutine thrice
