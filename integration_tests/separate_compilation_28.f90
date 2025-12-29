! Test: passing string literal to CHARACTER(*) parameter in separate compilation
! with common block containing CHARACTER variable.
! This tests the fix for a bug where common block struct members caused
! "Non-allocatable string isn't allocated" error when the struct was
! initialized with zeroinitializer (NULL pointer in string descriptor).
program separate_compilation_28
    implicit none
    call caller_one()
    call caller_two()
    call caller_one()
    call caller_two()
    print *, "All tests passed"
end program
