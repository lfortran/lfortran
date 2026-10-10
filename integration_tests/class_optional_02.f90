module class_optional_02_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: big_item_t
        integer :: w = 0
    end type
    type, abstract :: base_t
        integer :: v = 0
    contains
        procedure(get_i), deferred :: get
    end type
    abstract interface
        integer function get_i(self)
            import :: base_t
            class(base_t), intent(in) :: self
        end function
    end interface
    type, extends(base_t) :: impl_t
    contains
        procedure :: get => impl_get
    end type
contains
    integer function impl_get(self)
        class(impl_t), intent(in) :: self
        impl_get = 10*self%v
    end function

    subroutine consumea(r, item)
        integer, intent(out) :: r
        class(item_t), optional :: item(:)
        r = -1
        if (present(item)) r = item(1)%v + item(2)%v
    end subroutine

    subroutine consumeb(r, item)
        integer, intent(out) :: r
        class(base_t), optional :: item(:)
        r = -1
        if (present(item)) r = item(1)%get() + item(2)%get()
    end subroutine

    subroutine use_local(r)
        integer, intent(out) :: r
        class(item_t), allocatable :: b(:)
        call consumea(r, b)
        if (r /= -1) error stop
        allocate(item_t :: b(2))
        b(1)%v = 4
        b(2)%v = 5
        call consumea(r, b)
    end subroutine
end module

program class_optional_02
    use class_optional_02_mod
    implicit none
    integer :: r
    class(item_t), allocatable :: a(:)
    class(base_t), allocatable :: c(:)

    call consumea(r, a)
    if (r /= -1) error stop

    allocate(item_t :: a(2))
    a(1)%v = 3
    a(2)%v = 3
    call consumea(r, a)
    if (r /= 6) error stop

    call use_local(r)
    if (r /= 9) error stop

    call consumeb(r, c)
    if (r /= -1) error stop
    allocate(impl_t :: c(2))
    c(1)%v = 1
    c(2)%v = 2
    call consumeb(r, c)
    if (r /= 30) error stop
    print *, "ok"
end program
