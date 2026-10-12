! move_alloc from a nonpolymorphic allocatable array to a polymorphic one
module move_alloc_class_01_mod
    implicit none

    type :: base_t
        integer :: a = 0
    contains
        procedure :: get => base_get
    end type

    type, extends(base_t) :: child_t
        integer :: b = 0
    contains
        procedure :: get => child_get
    end type

contains

    integer function base_get(self)
        class(base_t), intent(in) :: self
        base_get = -1
    end function

    integer function child_get(self)
        class(child_t), intent(in) :: self
        child_get = self%a + self%b
    end function

    subroutine test()
        class(base_t), allocatable :: target_items(:)
        type(child_t), allocatable :: items(:)

        call move_alloc(items, target_items)
        if (allocated(target_items)) error stop 1
    end subroutine

end module

program move_alloc_class_01
    use move_alloc_class_01_mod
    implicit none
    class(base_t), allocatable :: t(:), t2(:), tm(:,:)
    type(child_t), allocatable :: items(:), m(:,:)
    class(*), allocatable :: u(:)
    integer, allocatable :: ints(:)
    integer :: i

    call test()

    allocate(t(5))
    allocate(items(-1:1))
    do i = -1, 1
        items(i)%a = i
        items(i)%b = 100
    end do
    call move_alloc(items, t)
    if (allocated(items)) error stop 2
    if (.not. allocated(t)) error stop 3
    if (lbound(t, 1) /= -1 .or. ubound(t, 1) /= 1) error stop 4
    do i = -1, 1
        if (t(i)%a /= i) error stop 5
        if (t(i)%get() /= i + 100) error stop 6
    end do
    select type (t)
    type is (child_t)
        if (any(t%b /= 100)) error stop 7
    class default
        error stop 8
    end select

    call move_alloc(t, t2)
    if (allocated(t)) error stop 9
    if (t2(0)%get() /= 100) error stop 10

    allocate(items(2))
    items%a = 1
    items%b = 3
    call move_alloc(items, t2)
    if (size(t2) /= 2) error stop 11
    if (t2(2)%get() /= 4) error stop 12
    deallocate(t2)

    allocate(m(2,3))
    m%a = 1
    m%b = 2
    call move_alloc(m, tm)
    if (allocated(m)) error stop 13
    if (any(shape(tm) /= [2, 3])) error stop 14
    if (tm(2,3)%get() /= 3) error stop 15

    allocate(items(3))
    items%b = 7
    call move_alloc(items, u)
    if (allocated(items)) error stop 16
    select type (u)
    type is (child_t)
        if (any(u%b /= 7)) error stop 17
    class default
        error stop 18
    end select

    allocate(ints(3))
    ints = [1, 2, 3]
    call move_alloc(ints, u)
    if (allocated(ints)) error stop 19
    select type (u)
    type is (integer)
        if (any(u /= [1, 2, 3])) error stop 20
    class default
        error stop 21
    end select

    print *, "ok"
end program
