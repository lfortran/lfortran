! Test: namelist defined locally in a nested subroutine (contains block)
! This previously caused an ICE (AssertFailed: scope.find(name) == scope.end())
! in the nested_vars pass because the pass incorrectly tried to move a
! locally-defined namelist into the parent's context module.
program nested_namelist_02
    implicit none

    call test_foo()

contains

    subroutine test_foo()
        real :: y
        namelist /n/ y

        y = 3.14
        write(*, n)

        if (abs(y - 3.14) > 1e-5) error stop
    end subroutine test_foo

end program nested_namelist_02
