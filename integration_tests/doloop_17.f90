module doloop_17_mod
    implicit none

contains
    function test_func(input) result(output)
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output

        output = 'k'
    end function test_func

end module doloop_17_mod

program doloop_17
    use doloop_17_mod
    implicit none

    integer :: i = 1

    do i = 1, 5
        print *, test_func('a')
    end do

end program doloop_17
