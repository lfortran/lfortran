program bindc_iso_fb_13
use iso_c_binding, only: c_int
implicit none

interface
    integer(c_int) function is_contig(a) bind(c, name="is_contig")
    import :: c_int
    integer(c_int), intent(in) :: a(:, :)
    end function
end interface

integer(c_int) :: a(4, 3)

a = 0

! CFI_is_contiguous skips the stride of an extent-1 dimension
if (is_contig(a) /= 1) error stop 1
if (is_contig(a(:, 2:2:2)) /= 1) error stop 2
if (is_contig(a(2:2, :)) /= 0) error stop 3

print *, "ok"
end program
