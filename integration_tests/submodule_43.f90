! Test: pass_array_by_data with nested internal subroutines in a
! submodule that call each other with derived-type assumed-shape arrays.
module submodule_43_m
    implicit none
    type :: t
        integer :: val
    end type
    interface
        module subroutine my_sort(array)
            type(t), intent(inout) :: array(0:)
        end subroutine
    end interface
end module

submodule(submodule_43_m) submodule_43_s
    implicit none
contains
    module subroutine my_sort(array)
        type(t), intent(inout) :: array(0:)
        call b(array)
    contains
        subroutine a(array)
            type(t), intent(inout) :: array(0:)
            integer :: tmp
            tmp = array(0)%val
            array(0)%val = array(1)%val
            array(1)%val = tmp
        end subroutine
        subroutine b(array)
            type(t), intent(inout) :: array(0:)
            call a(array)
        end subroutine
    end subroutine
end submodule

program submodule_43
    use submodule_43_m
    implicit none
    type(t) :: arr(0:1)
    arr(0)%val = 10
    arr(1)%val = 20
    call my_sort(arr)
    if (arr(0)%val /= 20) error stop
    if (arr(1)%val /= 10) error stop
    print *, "PASS"
end program
