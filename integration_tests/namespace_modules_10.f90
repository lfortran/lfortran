! Combining a namespace import with ordinary USE statements of the same
! module, and importing one module under several namespace names. All
! routes designate the same entities.
module namespace_modules_10_state
    implicit none
    integer :: value = 1
    integer :: other = 2
contains
    subroutine bump()
        value = value + 1
    end subroutine
end module

program namespace_modules_10
    use, namespace :: namespace_modules_10_state
    use, namespace :: s1 => namespace_modules_10_state
    use, namespace :: s2 => namespace_modules_10_state
    ! Repeating an identical namespace import is allowed (and redundant)
    use, namespace :: s2 => namespace_modules_10_state
    use namespace_modules_10_state, only: value, local_other => other
    implicit none

    if (value /= 1) error stop
    value = 10
    if (s1%value /= 10) error stop
    if (s2%value /= 10) error stop
    if (namespace_modules_10_state%value /= 10) error stop

    call s1%bump()
    if (value /= 11) error stop
    call namespace_modules_10_state%bump()
    if (s2%value /= 12) error stop

    s2%other = 5
    if (local_other /= 5) error stop
    ! The original name of a renamed entity is still reachable through the
    ! namespace, independently of the rename in the ordinary USE.
    if (s1%other /= 5) error stop
    print *, value, s1%value, s2%other, local_other
end program
