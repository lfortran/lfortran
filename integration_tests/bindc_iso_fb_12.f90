program bindc_iso_fb_12
use iso_c_binding, only: c_int
implicit none

interface
    integer(c_int) function get_row(a, i, r) bind(c, name="get_row")
    import :: c_int
    integer(c_int), intent(in), target :: a(:, :)
    integer(c_int), value :: i
    integer(c_int), pointer, intent(out) :: r(:)
    end function

    integer(c_int) function alloc_with_bad_elem_len(x, n) &
            bind(c, name="alloc_with_bad_elem_len")
    import :: c_int
    integer(c_int), allocatable, intent(out) :: x(:)
    integer(c_int), value :: n
    end function

    integer(c_int) function is_contig(a) bind(c, name="is_contig")
    import :: c_int
    integer(c_int), intent(in) :: a(:, :)
    end function
end interface

integer(c_int), target :: a(4, 3)
integer(c_int), pointer :: row(:)
integer(c_int), allocatable :: x(:)
integer :: i

a = reshape([(i, i = 1, 12)], [4, 3])

! CFI_section with a zero stride: row 2 of a
if (get_row(a, 2, row) /= 0) error stop 1
if (size(row) /= 3) error stop 2
if (any(row /= [2, 6, 10])) error stop 3

! CFI_allocate ignores elem_len for a non-character type
if (alloc_with_bad_elem_len(x, 5) /= 4) error stop 4
if (size(x) /= 5) error stop 5
x = [1, 2, 3, 4, 5]
if (sum(x) /= 15) error stop 6

! CFI_is_contiguous skips the stride of an extent-1 dimension
if (is_contig(a) /= 1) error stop 7
if (is_contig(a(:, 2:2:2)) /= 1) error stop 8
if (is_contig(a(2:2, :)) /= 0) error stop 9

print *, "ok"
end program
