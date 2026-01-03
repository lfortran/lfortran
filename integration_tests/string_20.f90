program string_20
    use, intrinsic :: iso_c_binding, only : c_char, c_null_char
    implicit none
    print *, "working ok"
    contains

    subroutine f_c_character(rhs, lhs, len)
        character(kind=c_char), intent(out) :: lhs(*)
        character(len=*), intent(in) :: rhs
        integer, intent(in) :: len
        integer :: length
        length = min(len-1, len_trim(rhs))

        lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
        lhs(length+1:length+1) = c_null_char

    end subroutine f_c_character

end program
