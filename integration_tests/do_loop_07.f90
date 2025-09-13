program do_loop_07
    integer :: i, s
    do i = 1, 3
        if (i == 1) then
            ! There should be an ImplicitDeallocate for __libasr_created_string_format here
            cycle
        end if

        print *, new_array(i)
        ! There should be an ImplicitDeallocate for __libasr_created_string_format here
    end do

    contains
        function new_array(i) result(t)
            integer, intent(in) :: i
            integer, allocatable :: t(:)
            allocate(t(i))

            t = 1
        end function
end program
