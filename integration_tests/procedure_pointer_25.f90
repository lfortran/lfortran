! Test calling procedure(), nopass, pointer struct members with arguments.
! Regression test for ICE: AssertFailed: arg_idx < func_subrout->n_args
! when a procedure() pointer (no explicit interface) is called with arguments
! that exceed the declared (empty) formal parameter count.
program procedure_pointer_25
    implicit none

    type :: runner
        procedure(), nopass, pointer :: caller => null()
    end type

    type(runner) :: br
    integer :: result

    result = 0

    ! Test 1: Call procedure pointer with one integer argument
    br%caller => set_value
    call br%caller(result)
    if (result /= 42) error stop

    ! Test 2: Reassign and call with a different procedure (same signature)
    br%caller => set_seven
    call br%caller(result)
    if (result /= 7) error stop

    ! Test 3: Reassign and call with yet another procedure
    br%caller => double_value
    result = 10
    call br%caller(result)
    if (result /= 20) error stop

    print *, "procedure_pointer_25: PASS"

contains

    subroutine set_value(v)
        integer, intent(out) :: v
        v = 42
    end subroutine

    subroutine set_seven(v)
        integer, intent(out) :: v
        v = 7
    end subroutine

    subroutine double_value(v)
        integer, intent(inout) :: v
        v = v * 2
    end subroutine

end program procedure_pointer_25
