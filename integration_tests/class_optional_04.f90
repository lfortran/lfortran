module class_optional_04_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: big_item_t
        integer :: w = 0
    end type
    type, abstract :: base_t
        integer :: v = 0
    end type
    type, extends(base_t) :: impl_t
    end type
contains
    integer function count_rank(item) result(r)
        class(item_t), optional :: item(..)
        r = -1
        if (present(item)) r = 100*rank(item) + size(item)
    end function

    integer function count_rank_abstract(item) result(r)
        class(base_t), optional :: item(..)
        r = -1
        if (present(item)) r = 100*rank(item) + size(item)
    end function

    integer function count_any(item) result(r)
        class(*), optional :: item(:)
        r = -1
        if (present(item)) r = size(item)
    end function

    subroutine use_local()
        class(item_t), allocatable :: b(:)
        class(base_t), allocatable :: d(:)
        if (count_rank(b) /= -1) error stop
        if (count_any(b) /= -1) error stop
        if (count_rank_abstract(d) /= -1) error stop
        if (count_any(d) /= -1) error stop
        allocate(big_item_t :: b(3))
        allocate(impl_t :: d(4))
        if (count_rank(b) /= 103) error stop
        if (count_any(b) /= 3) error stop
        if (count_rank_abstract(d) /= 104) error stop
        if (count_any(d) /= 4) error stop
    end subroutine
end module

program class_optional_04
    use class_optional_04_mod
    implicit none
    class(item_t), allocatable :: a(:)
    class(base_t), allocatable :: c(:)
    integer :: i
    do i = 1, 2
        if (count_rank(a) /= -1) error stop
        if (count_any(a) /= -1) error stop
        if (count_rank_abstract(c) /= -1) error stop
        if (count_any(c) /= -1) error stop
        allocate(item_t :: a(2))
        allocate(impl_t :: c(2))
        if (count_rank(a) /= 102) error stop
        if (count_any(a) /= 2) error stop
        if (count_rank_abstract(c) /= 102) error stop
        if (count_any(c) /= 2) error stop
        deallocate(a, c)
        call use_local()
    end do
    print *, "ok"
end program
