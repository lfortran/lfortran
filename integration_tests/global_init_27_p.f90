! A plugin that global_init_27c.c loads with dlopen into a C program linked
! with nothing but the dynamic loader; see global_init_native/CMakeLists.txt.
! Each check returns 0, or the number of the first thing that is wrong.
module global_init_27_p
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n = 5
    integer(c_int), allocatable :: a(:)
    character(len=3), target :: s = "abc"
    character(len=:), pointer :: q => s
contains
    integer(c_int) function check(fresh) bind(c, name="global_init_27_p_check")
        integer(c_int), value :: fresh
        check = 1
        if (.not. associated(q)) return
        check = 2
        if (len(q) /= 3 .or. q /= s) return
        check = 3
        if (fresh /= 0) then
            if (n /= 5 .or. allocated(a) .or. s /= "abc") return
        else
            if (n /= 5 + 1 .or. .not. allocated(a) .or. s /= "zzz") return
            if (sum(a) /= 6) return
        end if
        check = 0
    end function check

    subroutine mutate() bind(c, name="global_init_27_p_mutate")
        n = n + 1
        if (allocated(a)) deallocate(a)
        allocate(a(3))
        a = 2
        q = "zzz"
    end subroutine mutate
end module global_init_27_p
