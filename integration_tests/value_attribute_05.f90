! VALUE dummy arguments of non-bind(c) procedures are passed by value,
! compatible with GFortran's calling convention (checked from C).
module value_attribute_05_mod
implicit none

abstract interface
    integer(4) function value_f_iface(a4, a8, r4, r8, l4)
    integer(4), value :: a4
    integer(8), value :: a8
    real(4), value :: r4
    real(8), value :: r8
    logical(4), value :: l4
    end function
end interface

interface
    integer(4) function value_c_check(a1, a2, a4, a8, r4, r8, l1, l4)
    integer(1), value :: a1
    integer(2), value :: a2
    integer(4), value :: a4
    integer(8), value :: a8
    real(4), value :: r4
    real(8), value :: r8
    logical(1), value :: l1
    logical(4), value :: l4
    end function

    integer(4) function value_c_call(f)
    import :: value_f_iface
    procedure(value_f_iface) :: f
    end function
end interface

contains

    integer(4) function value_f_check(a4, a8, r4, r8, l4) result(r)
    integer(4), value :: a4
    integer(8), value :: a8
    real(4), value :: r4
    real(8), value :: r8
    logical(4), value :: l4
    r = 0
    if (a4 /= 7) r = 1
    if (a8 /= 123456789012_8) r = 2
    if (r4 /= 1.5) r = 3
    if (r8 /= -2.25d0) r = 4
    if (.not. l4) r = 5
    a4 = -1
    a8 = -1
    r4 = -1
    r8 = -1
    l4 = .false.
    end function

end module

program value_attribute_05
use value_attribute_05_mod
implicit none
integer(1) :: a1
integer(2) :: a2
integer(4) :: a4
integer(8) :: a8
real(4) :: r4
real(8) :: r8
logical(1) :: l1
logical(4) :: l4
integer :: ierr

ierr = value_c_check(-3_1, 1234_2, 7, 123456789012_8, 1.5, -2.25d0, &
    .false._1, .true.)
print *, ierr
if (ierr /= 0) error stop

a1 = -3
a2 = 1234
a4 = 7
a8 = 123456789012_8
r4 = 1.5
r8 = -2.25d0
l1 = .false.
l4 = .true.
ierr = value_c_check(a1, a2, a4, a8, r4, r8, l1, l4)
print *, ierr
if (ierr /= 0) error stop

ierr = value_c_check(a1 + 0_1, a2 + 0_2, a4 * 1, a8 - 0_8, r4 * 1.0, &
    r8 + 0.0d0, logical(l1 .and. l4, 1), a4 == 7)
print *, ierr
if (ierr /= 0) error stop

ierr = value_c_call(value_f_check)
print *, ierr
if (ierr /= 0) error stop

ierr = value_f_check(a4, a8, r4, r8, l4)
print *, ierr
if (ierr /= 0) error stop
if (a4 /= 7 .or. a8 /= 123456789012_8 .or. r4 /= 1.5 .or. r8 /= -2.25d0 &
    .or. .not. l4) error stop
end program
