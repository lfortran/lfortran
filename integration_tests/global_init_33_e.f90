! The same for an external bind(c) procedure: see global_init_32.f90 and global_init_33.f90.
integer(c_int) function global_init_33_external() bind(c)
    use iso_c_binding, only: c_int
    use global_init_33_m, only: cfgs
    implicit none
    integer :: w(cfgs(2)%n)
    global_init_33_external = 1
    call fill()
    if (size(w) /= 4) return
    global_init_33_external = 2
    if (sum(w) /= 8) return
    global_init_33_external = 0
contains
    subroutine fill()
        w = 2
    end subroutine fill
end function global_init_33_external
