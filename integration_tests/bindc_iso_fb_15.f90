! A bind(C) procedure deallocates and allocates a character-array pointer
! dummy whose target C made with CFI_allocate.
module bindc_iso_fb_15_mod
use iso_c_binding, only: c_int
implicit none
contains
function bindc_iso_fb_15_realloc(p) bind(c) result(r)
    character(len=:), pointer, intent(inout) :: p(:)
    integer(c_int) :: r
    r = 0
    if (.not. associated(p)) return
    if (size(p) /= 3 .or. len(p) /= 2) return
    if (p(2) /= 'cd') return
    deallocate(p)
    allocate(character(len=2) :: p(2))
    p = ['xy', 'zw']
    r = 1
end function
end module

program bindc_iso_fb_15
use bindc_iso_fb_15_mod
implicit none
interface
    function bindc_iso_fb_15_drive() bind(c) result(r)
        import :: c_int
        integer(c_int) :: r
    end function
end interface
if (bindc_iso_fb_15_drive() /= 1) error stop 1
print *, 'ok'
end program
