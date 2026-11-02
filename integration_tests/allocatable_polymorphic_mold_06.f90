module allocatable_polymorphic_mold_06_m
    implicit none
    type :: base
        integer :: x = 3
    end type base
    type, extends(base) :: child
        integer :: y = 5
    end type child
    type(child) :: hidden(2)
end module allocatable_polymorphic_mold_06_m

module allocatable_polymorphic_mold_06_n
    use allocatable_polymorphic_mold_06_m, only: base, hidden
    implicit none
contains
    subroutine mold_without_type_import()
        class(base), allocatable :: d(:)
        type(base) :: e
        allocate(d(2), mold=hidden)
        if (size(d) /= 2) error stop
        if (.not. same_type_as(d(1), hidden(1))) error stop
        if (.not. same_type_as(d(2), hidden(2))) error stop
        if (same_type_as(d(1), e)) error stop
    end subroutine mold_without_type_import
end module allocatable_polymorphic_mold_06_n

program allocatable_polymorphic_mold_06
    use allocatable_polymorphic_mold_06_m, only: base, child
    use allocatable_polymorphic_mold_06_n, only: mold_without_type_import
    implicit none
    type(child) :: pc(3), sc
    type(base) :: pb(2)
    class(base), allocatable :: a(:), b(:), c(:)

    pc%x = 10
    pc%y = 10
    allocate(a(3), mold=pc)
    if (size(a) /= 3) error stop
    select type (a)
    type is (child)
        if (any(a%x /= 3) .or. any(a%y /= 5)) error stop
    class default
        error stop
    end select

    allocate(b(2), mold=sc)
    if (size(b) /= 2) error stop
    select type (b)
    type is (child)
        if (any(b%x /= 3) .or. any(b%y /= 5)) error stop
    class default
        error stop
    end select

    pb%x = 10
    allocate(c(2), mold=pb)
    select type (c)
    type is (base)
        if (any(c%x /= 3)) error stop
    class default
        error stop
    end select

    call mold_without_type_import()
    print *, "ok"
end program allocatable_polymorphic_mold_06
