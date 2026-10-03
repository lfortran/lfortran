module external_14_mod_1
    use iso_c_binding
    implicit none

    interface f_string
        module procedure f_string_cptr
    end interface

contains

    function f_string_cptr(cptr) result(s)
        type(c_ptr), intent(in), value :: cptr
        character(len=:), allocatable :: s

        character(kind=c_char), pointer :: p(:)
        integer :: n, i

        if (.not. c_associated(cptr)) then
            error stop "f_string: NULL c_ptr"
        end if

        call c_f_pointer(cptr, p, [64])

        n = 0
        do while (p(n+1) /= c_null_char)
            n = n + 1
        end do

        allocate(character(len=n) :: s)

        do i = 1, n
            s(i:i) = achar(iachar(p(i)))
        end do
    end function f_string_cptr

end module external_14_mod_1