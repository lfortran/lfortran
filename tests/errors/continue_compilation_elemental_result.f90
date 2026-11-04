! The result of an elemental function must be scalar, not allocatable and
! not a pointer (C15101). Its length may use a dummy argument only as the
! argument of an inquiry function such as len (C15104).
module continue_compilation_elemental_result
contains
    elemental function array_result(x) result(r)
        integer, intent(in) :: x
        integer :: r(2)  ! {Error} result of elemental function 'array_result' must be scalar
        r = x
    end function
    elemental function allocatable_result(x) result(r)
        integer, intent(in) :: x
        integer, allocatable :: r  ! {Error} result of elemental function 'allocatable_result' cannot be allocatable
        r = x
    end function
    elemental function pointer_result(x) result(r)
        integer, intent(in) :: x
        integer, pointer :: r  ! {Error} result of elemental function 'pointer_result' cannot be a pointer
        r => null()
    end function
    elemental function len_trim_result(x) result(r)
        character(len=*), intent(in) :: x
        character(len=len_trim(x)) :: r  ! {Error} result length of elemental function 'len_trim_result' can use dummy argument 'x' only as the argument of an inquiry function such as len
        r = x
    end function
end module
