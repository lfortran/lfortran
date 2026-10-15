! Module and external procedures as procedure pointer component targets in
! the structure constructors of a main program's named constants and
! initializers (Fortran 2023 10.1.12 (3)(b), C1519).
module proc_ptr_36_mod
  implicit none
contains
  subroutine twice(i, r)
    integer, intent(in) :: i
    integer, intent(out) :: r
    r = 2*i
  end subroutine twice
end module proc_ptr_36_mod

program proc_ptr_36
  use proc_ptr_36_mod, only: twice
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
  type(callback), parameter :: by_module = callback(twice)
  type(callback), parameter :: by_external = callback(thrice)
  type(callback) :: x = callback(f=thrice)
  integer :: r

  call by_module%f(5, r)
  if (r /= 10) error stop 1
  call by_external%f(5, r)
  if (r /= 15) error stop 2
  call x%f(4, r)
  if (r /= 12) error stop 3
  x = by_module
  call x%f(4, r)
  if (r /= 8) error stop 4
  print *, r
end program proc_ptr_36

subroutine thrice(i, r)
  integer, intent(in) :: i
  integer, intent(out) :: r
  r = 3*i
end subroutine thrice
