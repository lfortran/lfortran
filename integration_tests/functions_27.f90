module functions_27_mod
    contains
    function func_arr3() result(ret_arr)
        integer,allocatable :: ret_arr(:)
        print *, allocated(ret_arr)
        ! ret_arr should always be not allocated by the begining of the function call.
        if(allocated(ret_arr) .eqv. .true.) error stop
        allocate(ret_arr(1))
    end function
end module


program functions_27
    use functions_27_mod
    implicit none
    integer, allocatable :: inp(:)
    integer :: i
    do i= 1, 3
        inp = func_arr3()
    end do 
end program  