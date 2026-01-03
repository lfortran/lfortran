!Test StringSection
program string_60
    character(10):: ss
    ss = "ABCDEFGHIJ"
    ss = reverse(ss)
    print *, ss
    if(ss /= "JIHGFEDCBA") error stop
    contains
    function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n

        n = len(string)
        do i = 1, n
            reverse_string(n-i+1:n-i+1) = string(i:i)
        end do

    end function reverse
end program