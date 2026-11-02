module allocatable_polymorphic_mold_04_m
    implicit none
    type :: t
        integer :: x = 3
    end type t
end module allocatable_polymorphic_mold_04_m

program allocatable_polymorphic_mold_04
    use allocatable_polymorphic_mold_04_m
    implicit none
    type(t) :: p(4)
    type(t), allocatable :: v(:)
    real :: a(5)
    real, allocatable :: r(:)

    p%x = 10
    allocate(v(4), mold=p)
    if (size(v) /= 4) error stop
    if (any(v%x /= 3)) error stop

    a = 1.0
    allocate(r(5), mold=a)
    if (size(r) /= 5) error stop
    print *, "ok"
end program allocatable_polymorphic_mold_04
