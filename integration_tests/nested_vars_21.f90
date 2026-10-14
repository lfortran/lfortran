! Derived types of a main program with procedure-pointer components, and a
! procedure pointer, whose interfaces are declared in the program, used
! through host association in internal procedures (#14968, #13630).
program nested_vars_21
  implicit none
  abstract interface
    subroutine ffunc(i)
      integer, intent(in) :: i
    end subroutine ffunc
    integer function vfunc(v)
      integer, intent(in) :: v(:)
    end function vfunc
  end interface
  type :: callback
    procedure(ffunc), pointer, nopass :: f => null()
  end type
  type :: callback_a
    procedure(a), pointer, nopass :: f
  end type
  type :: reducer
    procedure(vfunc), pointer, nopass :: g => null()
  end type
  type :: holder
    type(callback) :: c(2)
  end type
  type(callback) :: my_f
  type(callback_a) :: my_a
  type(reducer) :: r(2)
  type(holder) :: h
  procedure(a), pointer :: p => null()
  integer :: trace(6), n

  if (associated(my_f%f)) error stop 1
  call check_unset()

  n = 0
  my_f = callback(a)
  call my_f%f(5)
  if (n /= 6) error stop 2
  if (any(trace /= [5, 4, 3, 2, 1, 0])) error stop 3

  n = 0
  my_a = callback_a(b)
  call my_a%f(5)
  if (n /= 6) error stop 4
  if (any(trace /= [5, 4, 3, 2, 1, 0])) error stop 5

  r(2)%g => total
  if (sum_with(2, [1, 2, 3]) /= 6) error stop 6

  n = 0
  h%c(2)%f => c
  call h%c(2)%f(5)
  if (n /= 6) error stop 7
  if (any(trace /= [5, 4, 3, 2, 1, 0])) error stop 8

  n = 0
  p => d
  call p(5)
  if (n /= 6) error stop 12
  if (any(trace /= [5, 4, 3, 2, 1, 0])) error stop 13
  print *, trace

contains

  subroutine check_unset()
    if (associated(my_f%f)) error stop 9
    if (associated(r(1)%g)) error stop 10
    if (associated(h%c(1)%f)) error stop 11
    if (associated(p)) error stop 14
  end subroutine check_unset

  recursive subroutine a(i)
    integer, intent(in) :: i
    n = n + 1
    trace(n) = i
    if (i == 0) return
    call my_f%f(i - 1)
  end subroutine a

  recursive subroutine b(i)
    integer, intent(in) :: i
    n = n + 1
    trace(n) = i
    if (i == 0) return
    call my_a%f(i - 1)
  end subroutine b

  recursive subroutine c(i)
    integer, intent(in) :: i
    n = n + 1
    trace(n) = i
    if (i == 0) return
    call h%c(2)%f(i - 1)
  end subroutine c

  recursive subroutine d(i)
    integer, intent(in) :: i
    n = n + 1
    trace(n) = i
    if (i == 0) return
    call p(i - 1)
  end subroutine d

  integer function total(v)
    integer, intent(in) :: v(:)
    total = sum(v)
  end function total

  integer function sum_with(k, v)
    integer, intent(in) :: k
    integer, intent(in) :: v(:)
    sum_with = r(k)%g(v)
  end function sum_with

end program nested_vars_21
