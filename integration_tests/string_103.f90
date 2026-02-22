! Test allocatable character return with array implied-do (issue #6725)
program string_103
    implicit none
    call notstring_test()
contains
    subroutine notstring_test()
        character(7), parameter :: notstring_dataa(7) = &
            ["candy  ", 'x      ', "not bad", 'bad    ', 'not    ', &
             'is not ', 'no     ']
        character(10), parameter :: notstring_expected(7) = &
            ['not candy ', 'not x     ', 'not bad   ', 'not bad   ', 'not       ', &
             'not is not', 'not no    ']
        character(len(notstring_expected)) :: notstring_results(size(notstring_dataa))
        integer :: i

        notstring_results = [(notstring(notstring_dataa(i)), i=1, size(notstring_dataa))]
        if (.not. all(notstring_results == notstring_expected)) error stop
        print *, 'passed'
    end subroutine

    function notstring(str)
        character(*), intent(in) :: str
        character(:), allocatable :: notstring

        if (len(str) < 3) then
            notstring = 'not ' // str
        else if (str(1:3) /= 'not') then
            notstring = 'not ' // str
        else
            notstring = str
        end if
    end function
end program string_103
