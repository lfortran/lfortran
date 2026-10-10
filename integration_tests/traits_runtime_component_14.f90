module traits_runtime_component_14_types_m
    implicit none
    integer :: finals = 0, final_sum = 0
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    abstract interface :: ITagged
        integer function tag()
        end function
    end interface
    type :: Payload
        integer :: n = 0
    contains
        final :: finish
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
contains
    pure integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -900
    end subroutine
    subroutine fill(object, n)
        class(Holder), intent(inout) :: object
        integer, intent(in) :: n
        type(Payload) :: source
        source%n = n
        object%item = source
    end subroutine
    integer function read(object)
        class(Holder), intent(in) :: object
        read = -1
        if (allocated(object%item)) read = object%item%value()
    end function
end module

! Neither of these modules nor the program can see IValue: each one reaches
! the owning component only through the containing type.
module traits_runtime_component_14_adopt_m
    use traits_runtime_component_14_types_m, only: Holder, ITagged, read
    implicit none
    implements ITagged :: Holder
        procedure :: tag => holder_tag
    end implements
contains
    integer function holder_tag(self)
        type(Holder), intent(in) :: self
        holder_tag = read(self)
    end function
    subroutine swap{ITagged :: T}(x, y)
        type(T), intent(inout) :: x, y
        type(T) :: saved
        saved = x
        x = y
        y = saved
    end subroutine
end module

module traits_runtime_component_14_template_m
    implicit none
    requirement copyable {t}
        deferred type :: t
    end requirement
    template exchange_tmpl {t}
        require :: copyable {t}
        private
        public :: exchange
    contains
        subroutine exchange(x, y)
            type(t), intent(inout) :: x, y
            type(t) :: saved
            saved = x
            x = y
            y = saved
        end subroutine
    end template
end module

program traits_runtime_component_14
    use traits_runtime_component_14_types_m, only: Holder, Child, fill, read, &
        finals, final_sum
    use traits_runtime_component_14_adopt_m
    use traits_runtime_component_14_template_m, only: exchange_tmpl
    implicit none
    instantiate exchange_tmpl {Holder}, only: exchange_holder => exchange
    type(Holder) :: a, b
    type(Holder), allocatable :: list(:)
    class(Holder), allocatable :: c
    class(*), allocatable :: u
    integer :: n

    ! Each fill finalizes its local source; each exchange finalizes the two
    ! replaced components and then its saved local.
    call fill(a, 1)
    call fill(b, 2)
    if (a%tag() /= 1) error stop 1
    call swap(a, b)
    if (read(a) /= 2 .or. read(b) /= 1) error stop 2
    call exchange_holder(a, b)
    if (read(a) /= 1 .or. read(b) /= 2) error stop 3
    if (finals /= 8 .or. final_sum /= 12) error stop 4

    list = [Holder :: a, b, Holder()]
    if (size(list) /= 3 .or. read(list(2)) /= 2 .or. read(list(3)) /= -1) error stop 5
    n = finals
    a = Holder()
    if (allocated(a%item) .or. finals /= n + 1) error stop 6

    allocate(Child :: c)
    call fill(c, 6)
    u = c
    n = finals
    select type (v => u)
    class is (Holder)
        if (read(v) /= 6) error stop 7
        deallocate(v%item)
    class default
        error stop 8
    end select
    select type (c)
    type is (Child)
        if (read(c) /= 6) error stop 9
    class default
        error stop 10
    end select
    if (finals /= n + 1) error stop 11
    deallocate(c, u, list)
    deallocate(b%item)
    if (finals /= n + 5) error stop 12
end program
