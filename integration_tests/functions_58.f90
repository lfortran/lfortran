program functions_58
    implicit none

    character(len=:), allocatable :: temp
    character(len=1) :: sep
    integer :: i

    temp = "abc/def/ghi"
    sep = "/"

    i = find_char_char(reverse(temp), sep)

    if (i /= 4) error stop 1
    if (reverse(temp) /= "ihg/fed/cba") error stop 2

contains

    function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: j, n

        n = len(string)
        do j = 1, n
            reverse_string(n-j+1:n-j+1) = string(j:j)
        end do
    end function reverse

    function find_char_char(str, sep) result(index)
        character(len=*), intent(in) :: str
        character(len=1), intent(in) :: sep
        integer :: index
        integer :: j

        index = 0
        do j = 1, len(str)
            if (str(j:j) == sep) then
                index = j
                return
            end if
        end do
    end function find_char_char

end program functions_58
