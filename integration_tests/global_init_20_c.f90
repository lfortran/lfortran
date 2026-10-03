! The module of the shared library global_init_23c.c loads, unloads and
! loads again. Its pointer is initially associated with storage of
! global_init_20_a, which was initialized, and changed, before this library
! was first loaded.
module global_init_20_c
    use iso_c_binding, only: c_int
    use global_init_20_a, only: h
    implicit none
    character(len=3), pointer :: pc => h%s
    integer(c_int) :: n = 5
    integer(c_int), allocatable :: carr(:)
contains
    ! 0, or the number of the first thing that is wrong. `fresh` says
    ! whether this library's own storage is expected at its initial state.
    integer(c_int) function check(fresh) bind(c, name="global_init_20_c_check")
        integer(c_int), value :: fresh
        check = 1
        if (.not. associated(pc)) return
        check = 2
        if (len(pc) /= 3 .or. pc /= "xyz") return
        check = 3
        pc = "qrs"
        if (h%s /= "qrs") return
        pc = "xyz"
        check = 4
        if (fresh /= 0) then
            if (n /= 5 .or. allocated(carr)) return
        else
            if (n /= 6 .or. .not. allocated(carr)) return
            if (sum(carr) /= 6) return
        end if
        check = 0
    end function check

    subroutine mutate() bind(c, name="global_init_20_c_mutate")
        n = 6
        if (allocated(carr)) deallocate(carr)
        allocate(carr(2))
        carr = 3
    end subroutine mutate
end module global_init_20_c
