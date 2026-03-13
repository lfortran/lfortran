module test_struct_kind_shadow_char
    implicit none
    integer, parameter :: c = selected_char_kind('ASCII')
    type ty
        character(kind=c, len=5) :: c = 'hello'
    end type
end module
program test
    use test_struct_kind_shadow_char
    implicit none
    type(ty) :: t
    if (t%c /= 'hello') error stop "value incorrect"
    print *, "PASS"
end program test
