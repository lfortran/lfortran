program select_type_11
    implicit none
    integer, save :: arr(3) = [1, 2, 3]

    print *, "Before:", arr
    call update_any(arr)
    print *, "After: ", arr
    if(arr(1) /= 10) then
        print *, "Test failed: arr(1) should be 10, but is ", arr(1)
    else
        print *, "Test passed: arr(1) is ", arr(1)
    end if
contains

    subroutine update_any(generic)
        class(*) :: generic(:)
    integer, pointer :: xx(:)

        select type(generic)
        type is (integer)
            xx => generic
            xx(1) = 10
        end select
    end subroutine update_any

end program select_type_11