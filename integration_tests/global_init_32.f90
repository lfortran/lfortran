! A bind(c) procedure whose automatic local takes its extent from module state
! that startup code sets up, and which contains an internal procedure that
! uses that local by host association. Splitting the procedure so that the
! initialization runs before the extent is evaluated has to keep the internal
! procedure one level deep, as Fortran requires; an internal bind(c)
! procedure, which can contain nothing, is split within its host.
module global_init_32_m
use iso_c_binding, only: c_int
implicit none
type :: cfg
    integer :: n = 4
end type
type(cfg) :: cfgs(2)
contains
integer(c_int) function module_probe() bind(c)
    integer :: w(cfgs(1)%n)
    module_probe = 1
    call fill()
    if (size(w) /= 4) return
    module_probe = 2
    if (sum(w) /= 4) return
    module_probe = 0
contains
    subroutine fill()
        w = 1
    end subroutine fill
end function module_probe
end module global_init_32_m

program global_init_32
use iso_c_binding, only: c_int
use global_init_32_m, only: module_probe
implicit none
if (module_probe() /= 0) error stop 1
if (internal_probe() /= 0) error stop 2
print *, "ok"
contains
integer(c_int) function internal_probe() bind(c)
    use global_init_32_m, only: cfgs
    integer :: w(cfgs(2)%n)
    w = 3
    internal_probe = 1
    if (sum(w) /= 12) return
    internal_probe = 0
end function internal_probe
end program global_init_32
