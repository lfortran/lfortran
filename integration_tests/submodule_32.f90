module submodule_32_mod
    use iso_c_binding, only: c_int, c_funptr, c_f_procpointer, c_funloc, c_associated
    implicit none

    type :: handle_t
        integer :: id
        type(c_funptr) :: cleanup_func
    end type

    interface
        module subroutine deallocate_handle(h, stat, errmsg)
            type(handle_t), intent(inout) :: h
            integer(c_int), intent(out), optional :: stat
            character(len=*), intent(inout), optional :: errmsg
        end subroutine
    end interface
end module

submodule(submodule_32_mod) submodule_32_middle
    implicit none
end submodule

submodule(submodule_32_mod:submodule_32_middle) submodule_32_impl
    implicit none
contains
    module procedure deallocate_handle
        abstract interface
            subroutine cleanup_i(handle, stat, errmsg)
                import c_int, handle_t
                implicit none
                type(handle_t), pointer, intent(in) :: handle
                integer(c_int), intent(out) :: stat
                character(len=:), intent(out), allocatable :: errmsg
            end subroutine
        end interface
        procedure(cleanup_i), pointer :: cleanup_proc
        integer(c_int) :: local_stat
        character(len=:), allocatable :: local_errmsg

        local_stat = 0

        call c_f_procpointer(h%cleanup_func, cleanup_proc)
        if (associated(cleanup_proc)) then
            call cleanup_proc(h, local_stat, local_errmsg)
            if (local_stat /= 0) then
                if (present(stat)) stat = local_stat
                if (present(errmsg) .and. allocated(local_errmsg)) errmsg = local_errmsg
                return
            end if
        end if

        h%id = 0
        if (present(stat)) stat = 0
    end procedure
end submodule

subroutine my_cleanup(handle, stat, errmsg)
    use submodule_32_mod, only: handle_t
    use iso_c_binding, only: c_int
    implicit none
    type(handle_t), pointer, intent(in) :: handle
    integer(c_int), intent(out) :: stat
    character(len=:), intent(out), allocatable :: errmsg
    stat = 42
    errmsg = "cleanup done"
    handle%id = -1
end subroutine

program submodule_32
    use submodule_32_mod
    use iso_c_binding, only: c_funloc, c_null_funptr, c_associated
    implicit none
    integer(c_int) :: s
    character(len=100) :: emsg
    type(handle_t), target :: h

    interface
        subroutine my_cleanup(handle, stat, errmsg)
            import c_int, handle_t
            type(handle_t), pointer, intent(in) :: handle
            integer(c_int), intent(out) :: stat
            character(len=:), intent(out), allocatable :: errmsg
        end subroutine
    end interface

    h%id = 10
    h%cleanup_func = c_funloc(my_cleanup)
    s = -1
    emsg = ""
    call deallocate_handle(h, s, emsg)
    if (s /= 42) error stop
    if (trim(emsg) /= "cleanup done") error stop
    if (h%id /= -1) error stop

    h%id = 20
    h%cleanup_func = c_null_funptr
    call deallocate_handle(h, s, emsg)
    if (s /= 0) error stop
    if (h%id /= 0) error stop

    print *, "ok"
end program
