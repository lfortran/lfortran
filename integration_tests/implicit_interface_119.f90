module implicit_interface_119_m
implicit none
contains
real function f(x)
  real :: x
  f = x
end function
end module

! Each specific's only `f(...)` is a nested entity, so the host dummy stays
! data and a real actual is accepted.
program implicit_interface_119
implicit none
interface g_use
  procedure use_one
end interface
interface g_assoc
  procedure assoc_one
end interface
interface g_type
  procedure type_one
end interface
call g_use(1.0)
call g_assoc(1.0)
call g_type(1.0)
contains
subroutine use_one(f)
  real :: f
  block
    use implicit_interface_119_m, only: f
    if (f(2.0) /= 2.0) error stop 1
  end block
  if (f /= 1.0) error stop 2
end subroutine
subroutine assoc_one(f)
  real :: f
  real :: arr(1)
  arr(1) = 3.0
  associate (f => arr)
    if (f(1) /= 3.0) error stop 3
  end associate
  if (f /= 1.0) error stop 4
end subroutine
subroutine type_one(f)
  real :: f
  block
    type :: f
      real :: x
    end type
    type(f) :: y
    y = f(2.0)
    if (y%x /= 2.0) error stop 5
  end block
  if (f /= 1.0) error stop 6
end subroutine
end program
