module gg
    contains
    function func_arr3(inp_ref) result(ret_arr)
        integer, intent(in) :: inp_ref
        integer,allocatable :: ret_arr(:)
        allocate(ret_arr(1))
        ret_arr = [inp_ref]
        print *, "worked", inp_ref
    end function
end module


program p
    use gg
    implicit none
    integer, allocatable :: inp(:)
    integer :: i =77

    do i= 1, 3
        inp = func_arr3(i)
    end do
end program
