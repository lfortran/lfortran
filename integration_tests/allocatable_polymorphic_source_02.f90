module allocatable_polymorphic_source_02_m
    implicit none
contains
    subroutine add(x, total)
        class(*), intent(in) :: x
        real, intent(inout) :: total
        select type (x)
        type is (real)
            total = total + x
        end select
    end subroutine add
end module allocatable_polymorphic_source_02_m

program allocatable_polymorphic_source_02
    use allocatable_polymorphic_source_02_m, only: add
    implicit none
    integer, parameter :: n = 2000000
    real :: a(n)
    real :: total
    integer :: i
    class(*), pointer :: t(:)
    class(*), allocatable :: u(:)

    a = 2.5
    allocate(t, source=a)
    select type (t)
    type is (real)
        if (t(1) /= 2.5 .or. t(n) /= 2.5) error stop
    class default
        error stop
    end select

    a = 1.0
    allocate(u, source=a)
    total = 0
    do i = 1, n
        call add(u(i), total)
    end do
    if (total /= real(n)) error stop
    deallocate(t, u)
    print *, "ok"
end program allocatable_polymorphic_source_02
