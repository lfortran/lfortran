program bindc_iso_fb_16
! The C descriptor of a nonallocatable nonpointer bind(c) dummy has
! CFI_attribute_other, whatever the actual argument is: a whole array,
! an array section, a pointer or an allocatable array.
use iso_c_binding, only: c_int, c_char
implicit none

type, bind(c) :: pair
    integer(c_int) :: i, j
end type

interface
    integer(c_int) function attr_int_shape(x, extent) bind(c)
    import :: c_int
    integer(c_int), intent(in) :: x(:)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_int_rank(x, extent) bind(c)
    import :: c_int
    integer(c_int), intent(in) :: x(..)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_char_shape(x, extent) bind(c)
    import :: c_int, c_char
    character(kind=c_char, len=*), intent(in) :: x(:)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_char_rank(x, extent) bind(c)
    import :: c_int, c_char
    character(kind=c_char, len=*), intent(in) :: x(..)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_struct_shape(x, extent) bind(c)
    import :: c_int, pair
    type(pair), intent(in) :: x(:)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_struct_rank(x, extent) bind(c)
    import :: c_int, pair
    type(pair), intent(in) :: x(..)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_int_pointer(x, extent) bind(c)
    import :: c_int
    integer(c_int), pointer, intent(in) :: x(:)
    integer(c_int), value :: extent
    end function

    integer(c_int) function attr_int_allocatable(x, extent) bind(c)
    import :: c_int
    integer(c_int), allocatable, intent(in) :: x(:)
    integer(c_int), value :: extent
    end function

    integer(c_int) function sum_int_shape(x) bind(c)
    import :: c_int
    integer(c_int), intent(in) :: x(:)
    end function
end interface

integer(c_int), target :: a(6) = [8, 0, 9, 0, 10, 0]
integer(c_int) :: m(4, 3)
integer(c_int), pointer :: p(:)
integer(c_int), allocatable :: al(:)
character(kind=c_char, len=2) :: c(6)
type(pair) :: s(6)
integer :: i

m = reshape([(i, i = 1, 12)], [4, 3])
c = "ab"
s = pair(1, 2)
p => a(2:6:2)
allocate(al(3:6))
al = 3

! whole arrays
if (attr_int_shape(a, 6) /= 0) error stop 1
if (attr_int_rank(a, 6) /= 0) error stop 2

! array sections
if (attr_int_shape(a(1:5:2), 3) /= 0) error stop 3
if (attr_int_rank(a(1:5:2), 3) /= 0) error stop 4
if (attr_int_shape(a(2:4), 3) /= 0) error stop 5
if (attr_int_rank(a(2:4), 3) /= 0) error stop 6
if (attr_int_shape(m(2, :), 3) /= 0) error stop 7
if (attr_int_rank(m(:, 3), 4) /= 0) error stop 8
if (attr_char_shape(c(1:5:2), 3) /= 0) error stop 9
if (attr_char_rank(c(1:5:2), 3) /= 0) error stop 10
if (attr_struct_shape(s(1:5:2), 3) /= 0) error stop 11
if (attr_struct_rank(s(1:5:2), 3) /= 0) error stop 12

! the section is still described correctly
if (sum_int_shape(a(1:5:2)) /= 27) error stop 13
if (sum_int_shape(m(2, :)) /= 18) error stop 14

! pointer and allocatable actual arguments, and sections of them
if (attr_int_shape(p, 3) /= 0) error stop 15
if (attr_int_rank(p, 3) /= 0) error stop 16
if (attr_int_shape(p(1:3:2), 2) /= 0) error stop 17
if (attr_int_shape(al, 4) /= 0) error stop 18
if (attr_int_rank(al, 4) /= 0) error stop 19
if (attr_int_rank(al(4:5), 2) /= 0) error stop 20

! pointer and allocatable dummy arguments keep their attribute
if (attr_int_pointer(p, 3) /= 1) error stop 21
if (attr_int_allocatable(al, 4) /= 2) error stop 22

print *, "ok"
end program
