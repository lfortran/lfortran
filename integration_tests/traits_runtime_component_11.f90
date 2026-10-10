module traits_runtime_component_11_m
    implicit none
    integer :: finals = 0, final_sum = 0
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Payload
        integer :: n = 7
    contains
        final :: finish_payload
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
    type, extends(Holder) :: Child
        integer :: extra = 0
    end type
    type :: Wrapper
        type(Holder) :: h = Holder()
        integer :: tag = 2
    end type
contains
    pure integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -900
    end subroutine
    function make(n) result(object)
        integer, intent(in) :: n
        type(Holder) :: object
        type(Payload) :: source
        source%n = n
        object%item = source
    end function
    function empty() result(object)
        type(Holder) :: object
    end function
    integer function observe(object)
        type(Holder), intent(in) :: object
        observe = -1
        if (allocated(object%item)) observe = object%item%value()
    end function
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_component_11
    use traits_runtime_component_11_m
    implicit none
    type(Holder) :: x
    type(Holder), allocatable :: a, b, c, d
    type(Holder), pointer :: p
    class(Holder), allocatable :: q, r
    type(Wrapper) :: w
    type(Payload) :: seed
    integer :: n

    ! F2023 9.7.3.2 deallocates the allocated components of a function result
    ! after the statement; make finalizes its own local source on return.
    x = make(1)
    call check(finals == 2 .and. final_sum == 2, 1)
    call check(observe(x) == 1, 2)
    n = observe(make(2))
    call check(n == 2 .and. finals == 4 .and. final_sum == 6, 3)
    associate (y => make(3))
        call check(finals == 5 .and. y%item%value() == 3, 4)
    end associate
    call check(finals == 6 .and. final_sum == 12, 5)
    x = empty()
    call check(.not. allocated(x%item), 6)
    call check(finals == 7 .and. final_sum == 13, 7)
    call check(observe(empty()) == -1, 8)

    allocate(a)
    seed%n = 5
    a%item = seed
    b = a
    call check(allocated(b), 9)
    call check(b%item%value() == 5 .and. finals == 7, 10)
    seed%n = 6
    a%item = seed
    call check(finals == 8 .and. final_sum == 18, 11)
    b = a
    call check(b%item%value() == 6, 12)
    call check(finals == 9 .and. final_sum == 23, 13)
    allocate(c, source=a)
    allocate(d, mold=a)
    allocate(p, source=a)
    call check(c%item%value() == 6 .and. p%item%value() == 6, 14)
    call check(.not. allocated(d%item), 15)
    deallocate(a, b, c, d, p)
    call check(finals == 13 .and. final_sum == 47, 16)

    allocate(Child :: q)
    seed%n = 3
    q%item = seed
    r = q
    select type (r)
    type is (Child)
    class default
        error stop 17
    end select
    call check(r%item%value() == 3, 18)
    deallocate(q)
    call check(finals == 14 .and. r%item%value() == 3, 19)
    deallocate(r)
    call check(finals == 15 .and. final_sum == 53, 20)

    x%item = seed
    x = Holder()
    call check(.not. allocated(x%item) .and. finals == 16, 21)
    x%item = seed
    x = Holder(null())
    call check(.not. allocated(x%item) .and. finals == 17, 22)
    call check(.not. allocated(w%h%item) .and. w%tag == 2, 23)
    w%h%item = seed
    w = Wrapper()
    call check(.not. allocated(w%h%item) .and. w%tag == 2, 24)
    call check(finals == 18 .and. final_sum == 62, 25)
end program
