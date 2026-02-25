! Test: intrinsic assignment of a derived type should not use defined
! assignment for components when the RHS type does not match.
! (inner_t has assignment(=) from logical, not from inner_t)
program derived_types_115
    use derived_types_115_module
    implicit none

    type(outer_t) :: a, b

    a%component%test_passed = .true.
    a%val = 42

    ! Intrinsic assignment of outer_t: component should be copied
    ! via default (bitwise) copy, not via assign_logical
    b = a

    if (.not. b%component%test_passed) error stop
    if (b%val /= 42) error stop

    print *, "PASS"
end program
