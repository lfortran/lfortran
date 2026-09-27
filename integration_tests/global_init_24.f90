! A C-callable procedure whose automatic objects take their bounds and
! length from module state, called from a C constructor that runs before the
! startup hook of the module's object file (see global_init_24c.c). The
! module has to be initialized before those specification expressions are
! evaluated on entry, not only before the first statement of the body.
integer(c_int) function global_init_24_probe() bind(c)
    use iso_c_binding, only: c_int
    use global_init_24_m, only: cfgs, nm
    implicit none
    integer :: w(cfgs(1)%n)
    character(len=len_trim(nm%s)) :: buf
    global_init_24_probe = 1
    if (size(w) /= 4) return
    global_init_24_probe = 2
    if (len(buf) /= 3) return
    w = 1
    buf = nm%s
    global_init_24_probe = 3
    if (sum(w) /= 4 .or. buf /= "abc") return
    global_init_24_probe = 0
end function global_init_24_probe
