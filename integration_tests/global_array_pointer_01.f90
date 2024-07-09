module global_array_pointer_01_mod
    implicit none
    integer, dimension(:), pointer :: ptr_in
    integer, target :: i(1)
    contains 
    subroutine sub_arr_pointer1
        ptr_in => i
        i = [343]
        print *, ptr_in(1)
        print *, i(1)
        if(ptr_in(1) /= 343) error stop 
    end subroutine sub_arr_pointer1

    subroutine sub_arr_pointer2
        ptr_in(1) = 0
        print *, ptr_in(1)
        print *, i(1)
        if(i(1) /= 0) error stop
    end subroutine sub_arr_pointer2
end module global_array_pointer_01_mod

program global_array_pointer_01
    use global_array_pointer_01_mod
    implicit none
    call sub_arr_pointer1
    call sub_arr_pointer2
end program global_array_pointer_01

