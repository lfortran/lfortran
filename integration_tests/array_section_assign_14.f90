! An array assignment whose target shares storage with a differently laid
! out view of the same COMMON block, which the value reads shifted against
! the target, must evaluate the value before defining the target (issue
! #16105). GFortran 16 gets these wrong (it misses the aliasing, see
! #16234), so this test is not run with it.
module array_section_assign_14_m1
    implicit none
    real :: a(4)
    common /asa14/ a
end module

module array_section_assign_14_m2
    implicit none
    ! c(1:3) shares the storage of a(2:4)
    real :: w, c(3)
    common /asa14/ w, c
end module

program array_section_assign_14
    use array_section_assign_14_m1
    implicit none
    real :: h(4)
    common /asa14h/ h

    a = [1.0, 10.0, 100.0, 1000.0]
    call use_views()
    if (any(a /= [1.0, 12.0, 111.0, 1101.0])) error stop 1

    h = [1.0, 10.0, 100.0, 1000.0]
    call host_views()
    if (any(h /= [1.0, 12.0, 111.0, 1101.0])) error stop 2
    print *, sum(a), sum(h)

contains

    subroutine use_views()
        use array_section_assign_14_m2
        c = c + a(1:3) + 1.0
    end subroutine

    subroutine host_views()
        ! g(1:3) shares the storage of the host's h(2:4)
        real :: u, g(3)
        common /asa14h/ u, g
        g = g + h(1:3) + 1.0
    end subroutine

end program
