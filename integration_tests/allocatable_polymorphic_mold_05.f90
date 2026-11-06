module allocatable_polymorphic_mold_05_m
    implicit none
    type :: t
        integer, allocatable :: a(:)
    end type t
end module allocatable_polymorphic_mold_05_m

program allocatable_polymorphic_mold_05
    use allocatable_polymorphic_mold_05_m
    implicit none
    type(t) :: p(2)
    type(t), allocatable :: v(:)

    allocate(p(1)%a(3), p(2)%a(5))
    allocate(v(2), mold=p)
    if (size(v) /= 2) error stop
    if (allocated(v(1)%a) .or. allocated(v(2)%a)) error stop
    print *, "ok"
end program allocatable_polymorphic_mold_05
