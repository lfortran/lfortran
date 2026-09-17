! Test derived type declared inside procedures with component default initializers (Issue #13108)
module derived_type_with_default_init_08_mod
    implicit none
contains

    function test_func() result(res)
        integer :: res
        type :: local_t
            integer :: val = 42
        end type
        type(local_t) :: obj
        res = obj%val
        obj%val = obj%val + 1
    end function test_func

end module derived_type_with_default_init_08_mod

subroutine test_sub(res)
    implicit none
    integer, intent(out) :: res
    type :: sub_t
        integer :: count = 100
    end type
    type(sub_t) :: obj
    res = obj%count
    obj%count = obj%count + 10
end subroutine test_sub

program derived_type_with_default_init_08
    use derived_type_with_default_init_08_mod
    implicit none

    interface
        subroutine test_sub(res)
            integer, intent(out) :: res
        end subroutine test_sub
    end interface

    integer :: v1, v2

    ! 1. External subroutine
    call test_sub(v1)
    if (v1 /= 100) error stop 1
    call test_sub(v2)
    if (v2 /= 100) error stop 2

    ! 2. Module function
    v1 = test_func()
    if (v1 /= 42) error stop 3
    v2 = test_func()
    if (v2 /= 42) error stop 4

    ! 3. Internal subroutine
    call test_internal(v1)
    if (v1 /= 7) error stop 5
    call test_internal(v2)
    if (v2 /= 7) error stop 6

contains

    subroutine test_internal(res)
        integer, intent(out) :: res
        type :: internal_t
            integer :: num = 7
        end type
        type(internal_t) :: obj
        res = obj%num
        obj%num = obj%num + 1
    end subroutine test_internal

end program derived_type_with_default_init_08
