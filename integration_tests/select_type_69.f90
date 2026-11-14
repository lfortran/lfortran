! select type inside rank (0) of an allocatable or pointer class(*)
! assumed-rank dummy
module select_type_69_m
    implicit none
    type :: pt
        integer :: i = 1, j = 2
    end type
contains
    integer function star_alloc(x) result(r)
        class(*), allocatable, intent(in) :: x(..)
        r = 0
        select rank (x)
        rank (0)
            select type (x)
            type is (integer)
                r = x
            type is (real)
                if (abs(x - 1.5) > 1e-6) error stop 10
                r = -1
            type is (pt)
                r = x%i + x%j
            class default
                r = -2
            end select
        rank default
            r = -3
        end select
    end function

    integer function star_ptr(x) result(r)
        class(*), pointer, intent(in) :: x(..)
        r = 0
        select rank (x)
        rank (0)
            select type (x)
            type is (integer)
                r = x
            type is (pt)
                r = x%i * x%j
            class default
                r = -2
            end select
        rank default
            r = -3
        end select
    end function
end module

program select_type_69
    use select_type_69_m
    implicit none
    class(*), allocatable :: a
    class(*), pointer :: p
    integer, target :: k
    logical, target :: l
    type(pt), target :: v

    allocate(a, source=11)
    if (star_alloc(a) /= 11) error stop 1
    deallocate(a)
    allocate(a, source=1.5)
    if (star_alloc(a) /= -1) error stop 2
    deallocate(a)
    allocate(a, source=.true.)
    if (star_alloc(a) /= -2) error stop 3
    deallocate(a)
    allocate(a, source=pt(3, 4))
    if (star_alloc(a) /= 7) error stop 6

    k = 7
    p => k
    if (star_ptr(p) /= 7) error stop 4
    l = .false.
    p => l
    if (star_ptr(p) /= -2) error stop 5
    v = pt(5, 6)
    p => v
    if (star_ptr(p) /= 30) error stop 7
    print *, "ok"
end program
