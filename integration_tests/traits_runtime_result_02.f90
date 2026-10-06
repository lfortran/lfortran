module traits_runtime_result_02_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Payload
        integer, allocatable :: data(:)
        integer, pointer :: link => null()
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure, pass :: value => get
    end implements
    type(Payload) :: source
    integer :: calls = 0, finals = 0, finalized_values = 0
contains
    integer function get(self)
        class(Payload), intent(in) :: self
        get = self%data(1) + self%link
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        if (.not. allocated(self%data)) error stop 1
        finals = finals + 1
        finalized_values = finalized_values + self%data(1)
        self%data = -777
        self%link = self%link + 1
    end subroutine
    function make(n, link) result(object)
        integer, intent(in) :: n
        integer, target, intent(in) :: link
        class(IValue), allocatable :: object
        calls = calls + 1
        if (allocated(object)) error stop 2
        if (n == 0) return
        if (.not. allocated(source%data)) allocate(source%data(2))
        source%data = [n, n + 1]
        source%link => link
        allocate(object, source=source)
        if (n < 0) return
        if (.not. allocated(object)) error stop 3
    end function
    function relay(n, link) result(object)
        integer, intent(in) :: n
        integer, target, intent(in) :: link
        class(IValue), allocatable :: object
        object = make(n, link)
    end function
    subroutine observe(object, expected)
        class(IValue), intent(in) :: object
        integer, intent(in) :: expected
        if (object%value() /= expected) error stop 4
    end subroutine
    subroutine observe_pair(first, second)
        class(IValue), intent(in) :: first, second
        if (finals /= 3 .or. calls /= 4) error stop 5
        call observe(first, 37)
        call observe(second, 43)
    end subroutine
    logical function matches(object, expected)
        class(IValue), intent(in) :: object
        integer, intent(in) :: expected
        matches = object%value() == expected
    end function
    logical function keep(value)
        logical, intent(in) :: value
        keep = value
    end function
    integer function iterations(object)
        class(IValue), intent(in) :: object
        if (object%value() /= 52) error stop 6
        iterations = 2
    end function
    subroutine early_return(link)
        integer, target, intent(in) :: link
        if (matches(make(-47, link), -37)) then
            if (finals /= 7 .or. link /= 10) error stop 7
            return
        end if
        error stop 8
    end subroutine
end module

program traits_runtime_result_02
    use traits_runtime_result_02_m
    implicit none
    class(IValue), allocatable :: owner
    integer, target :: link = 3
    integer :: i

    ! The native diagnostic gate executes each invalid use of an empty result.
    if (command_argument_count() /= 0) then
        select case (command_argument_count())
        case (1)
            call observe(make(0, link), 0)
        case (2)
            owner = make(0, link)
        case (3)
            allocate(owner, source=make(0, link))
        end select
        error stop 99
    end if

    call observe(make(17, link), 20)
    if (finals /= 1 .or. finalized_values /= 17 .or. link /= 4) error stop 9
    owner = make(23, link)
    source%data = [88, 89]
    call observe(owner, 28)
    if (finals /= 2 .or. finalized_values /= 40 .or. link /= 5) error stop 10
    deallocate(owner)
    if (finals /= 3 .or. finalized_values /= 63 .or. link /= 6) error stop 11

    call observe_pair(make(31, link), make(37, link))
    if (finals /= 5 .or. finalized_values /= 131 .or. link /= 8) error stop 12
    if (keep(matches(make(41, link), 49))) then
        if (finals /= 5 .or. link /= 8) error stop 13
    else
        error stop 14
    end if
    if (finals /= 6 .or. finalized_values /= 172 .or. link /= 9) error stop 15
    do i = 1, iterations(make(43, link))
        if (finals /= 6 .or. link /= 9) error stop 16
    end do
    if (finals /= 7 .or. finalized_values /= 215 .or. link /= 10) error stop 17
    call early_return(link)
    if (finals /= 8 .or. finalized_values /= 168 .or. link /= 11) error stop 18
    block
        class(IValue), allocatable :: local
        local = make(53, link)
        if (finals /= 9 .or. link /= 12) error stop 19
        call observe(local, 65)
    end block
    if (finals /= 10 .or. finalized_values /= 274 .or. link /= 13) error stop 20
    do i = 1, 3
        call observe(make(60 + i, link), 60 + i + link)
    end do
    if (finals /= 13 .or. finalized_values /= 460 .or. link /= 16) error stop 21
    owner = relay(67, link)
    call observe(owner, 85)
    if (finals /= 15 .or. finalized_values /= 594 .or. link /= 18) error stop 22
    deallocate(owner)
    if (finals /= 16 .or. finalized_values /= 661 .or. link /= 19) error stop 23
    associate (matched => keep(matches(make(71, link), 90)))
        if (.not. matched) error stop 24
        if (finals /= 16 .or. link /= 19) error stop 25
    end associate
    if (finals /= 17 .or. finalized_values /= 732 .or. link /= 20) error stop 26
    if (calls /= 13) error stop 27
    deallocate(source%data)
    nullify(source%link)
end program
