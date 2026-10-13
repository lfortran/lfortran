! Initial targets of procedure pointers and of procedure pointer components in
! the named constants and initializers of a main program: module and external
! procedures (Fortran 2023 C1519). An internal procedure is a valid target
! only at run time (C1033), also when it is the interface (#14968).
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
  type :: callback
    procedure(twice), pointer, nopass :: f
  end type
  type :: callback_a
    procedure(a), pointer, nopass :: f
  end type
  type(callback), parameter :: by_module = callback(twice)
  type(callback), parameter :: by_external = callback(thrice)
  type(callback) :: x = callback(f=thrice)
  procedure(twice), pointer :: p => thrice
  type(callback_a) :: y
  integer :: r, n

  call by_module%f(5, r)
  if (r /= 10) error stop 1
  call by_external%f(5, r)
  if (r /= 15) error stop 2
  call x%f(4, r)
  if (r /= 12) error stop 3
  call p(3, r)
  if (r /= 9) error stop 4
  x = by_module
  call x%f(4, r)
  if (r /= 8) error stop 5

  n = 0
  y = callback_a(a)
  call y%f(7, r)
  if (r /= 7 .or. n /= 1) error stop 6
  y%f => a
  call y%f(6, r)
  if (r /= 6 .or. n /= 2) error stop 7
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
