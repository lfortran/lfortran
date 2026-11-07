module same_type_as_02_m
    implicit none

    type :: t
        integer :: x = 0
    end type

    type, extends(t) :: e
        integer :: y = 0
    end type

contains

    ! same_type_as/extends_type_of of an assumed-rank dummy must use the
    ! dynamic type of the actual argument
    subroutine check_star(a, is_e)
        class(*), intent(in) :: a(..)
        logical, intent(in) :: is_e
        type(t) :: mt
        type(e) :: me
        if (same_type_as(a, mt) .neqv. .not. is_e) error stop 1
        if (same_type_as(a, me) .neqv. is_e) error stop 2
        if (.not. extends_type_of(a, mt)) error stop 3
        if (extends_type_of(a, me) .neqv. is_e) error stop 4
    end subroutine

    subroutine check_t(a, is_e)
        class(t), intent(in) :: a(..)
        logical, intent(in) :: is_e
        type(t) :: mt
        type(e) :: me
        if (same_type_as(a, mt) .neqv. .not. is_e) error stop 11
        if (same_type_as(a, me) .neqv. is_e) error stop 12
        if (.not. extends_type_of(a, mt)) error stop 13
        if (extends_type_of(a, me) .neqv. is_e) error stop 14
    end subroutine

end module

program same_type_as_02
    use same_type_as_02_m
    implicit none
    class(*), allocatable :: u, ua(:)
    class(t), allocatable :: ca(:)

    u = t(7)
    call check_star(u, .false.)
    deallocate(u)
    u = e(1, 2)
    call check_star(u, .true.)
    allocate(ua(2), source=e(1, 2))
    call check_star(ua, .true.)
    deallocate(ua)
    allocate(ua(3), source=t(4))
    call check_star(ua, .false.)

    allocate(ca(2), source=e(1, 2))
    call check_t(ca, .true.)
    deallocate(ca)
    allocate(ca(2), mold=t(5))
    call check_t(ca, .false.)
    print *, "ok"
end program
