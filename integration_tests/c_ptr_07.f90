module cptr_07_mod
    use iso_c_binding
    implicit none
contains
    function f_string(cstr) result(fstr)
        character(len=1), pointer :: cstr(:)
        character(:), allocatable :: fstr
        integer :: i, n

        n = 0
        do i = 1, size(cstr)
            if (cstr(i) == c_null_char) exit
            n = n + 1
        end do

        allocate(character(len=n) :: fstr)
        do i = 1, n
            fstr(i:i) = cstr(i)
        end do
    end function f_string
end module cptr_07_mod


module cptr_07_mod_2
    use iso_c_binding
    use cptr_07_mod
    implicit none

contains

    function get_temp_filename() result(tempfile)
        integer, parameter :: MAX_FILENAME_LENGTH = 32768
        character(:), allocatable :: tempfile

        type(c_ptr) :: c_tempfile_ptr
        character(len=1), pointer :: c_tempfile(:)

        interface
            function c_tempnam(dir, pfx) bind(C, name="tempnam")
                import :: c_ptr
                type(c_ptr), value :: dir
                type(c_ptr), value :: pfx
                type(c_ptr) :: c_tempnam
            end function c_tempnam

            subroutine c_free(ptr) bind(C, name="free")
                import :: c_ptr
                type(c_ptr), value :: ptr
            end subroutine c_free
        end interface

        c_tempfile_ptr = c_tempnam(C_NULL_PTR, C_NULL_PTR)
        if (.not. c_associated(c_tempfile_ptr)) then
            error stop "tempnam returned NULL"
        end if
        call c_f_pointer(c_tempfile_ptr, c_tempfile, [MAX_FILENAME_LENGTH])
        tempfile = f_string(c_tempfile)
        call c_free(c_tempfile_ptr)

    end function get_temp_filename

end module cptr_07_mod_2


program cptr_07
    use cptr_07_mod_2
    implicit none
    character(:), allocatable :: name
    allocate(name, source=get_temp_filename())
    print *, "Temp filename =", trim(name)
end program cptr_07
