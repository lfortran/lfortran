module traits_runtime_result_04_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Payload
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    type :: OtherPayload
        real(8) :: padding(3) = [101.0_8, 202.0_8, 303.0_8]
        integer :: n
    contains
        final :: finish_other
    end type
    implements IValue :: Payload
        procedure, pass :: value => payload_value
    end implements
    implements IValue :: OtherPayload
        procedure, pass :: value => other_value
    end implements
    type(Payload) :: seed
    type(OtherPayload) :: other_seed
    integer :: calls = 0, finals = 0, total = 0
contains
    integer function payload_value(self)
        class(Payload), intent(in) :: self
        payload_value = self%data(1)
    end function
    integer function other_value(self)
        class(OtherPayload), intent(in) :: self
        other_value = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        if (.not. allocated(self%data)) error stop 1
        finals = finals + 1
        total = total + self%data(1)
        self%data = -777
    end subroutine
    subroutine finish_other(self)
        type(OtherPayload), intent(inout) :: self
        if (any(self%padding /= [101.0_8, 202.0_8, 303.0_8])) error stop 16
        finals = finals + 1
        total = total + self%n
        self%n = -777
    end subroutine
    function make(n) result(object)
        integer, intent(in) :: n
        class(IValue), allocatable :: object
        calls = calls + 1
        if (mod(n, 2) == 0) then
            other_seed%n = n
            allocate(object, source=other_seed)
        else
            if (.not. allocated(seed%data)) allocate(seed%data(2))
            seed%data = [n, n + 1]
            allocate(object, source=seed)
        end if
    end function
    integer function observe(view, expected_finals)
        class(IValue), intent(in) :: view
        integer, intent(in) :: expected_finals
        if (finals /= expected_finals) error stop 2
        observe = view%value()
    end function
    function copy_value(view) result(object)
        class(IValue), intent(in) :: view
        class(IValue), allocatable :: object
        calls = calls + 1
        object = view
    end function
    logical function positive(view)
        class(IValue), intent(in) :: view
        positive = view%value() > 0
    end function
    subroutine repeated(n)
        integer, intent(in) :: n
        integer :: values(n), i, round, before, sum_before, calls_before
        do round = 1, 3
            before = finals
            sum_before = total
            calls_before = calls
            values = [(observe(make(i), before), i=1,n)]
            do i = 1, n
                if (values(i) /= i) error stop 3
            end do
            if (finals /= before + n .or. calls /= calls_before + n) error stop 4
            if (total /= sum_before + n * (n + 1) / 2) error stop 5
            before = finals
            sum_before = total
            calls_before = calls
            values = [(observe(copy_value(make(i)), before), i=1,n)]
            do i = 1, n
                if (values(i) /= i) error stop 17
            end do
            if (finals /= before + 2 * n .or. calls /= calls_before + 2 * n) error stop 18
            if (total /= sum_before + n * (n + 1)) error stop 19
        end do
    end subroutine
    subroutine conditional(first, second)
        logical, intent(in) :: first, second
        integer :: before, sum_before, calls_before, expected
        before = finals
        sum_before = total
        calls_before = calls
        expected = 0
        if (first) then
            expected = 17
        else if (second) then
            expected = 29
        end if
        if ((first ? positive(make(17)) : (second ? positive(make(29)) : .false.))) then
            if (expected == 0 .or. finals /= before) error stop 6
            if (calls /= calls_before + 1) error stop 7
        else
            if (expected /= 0 .or. calls /= calls_before) error stop 8
        end if
        if (expected /= 0) before = before + 1
        if (finals /= before .or. total /= sum_before + expected) error stop 9
    end subroutine
    subroutine early_return()
        integer :: before
        before = finals
        if ((.true. ? positive(make(41)) : .false.)) then
            if (finals /= before) error stop 10
            return
        end if
        error stop 11
    end subroutine
    subroutine loop_exits()
        integer :: i, before, sum_before
        before = finals
        sum_before = total
        outer: do i = 1, 3
            if ((i > 0 ? positive(make(i)) : .false.)) then
                if (finals /= before + i - 1) error stop 12
                if (i == 1) cycle outer
                exit outer
            end if
        end do outer
        if (finals /= before + 2 .or. total /= sum_before + 3) error stop 13
    end subroutine
end module

program traits_runtime_result_04
    use traits_runtime_result_04_m
    implicit none
    integer :: n, before, sum_before
    n = command_argument_count()
    call repeated(n)
    call repeated(n + 3)
    call conditional(.false., .false.)
    call conditional(.true., .true.)
    call conditional(.false., .true.)
    before = finals
    sum_before = total
    call early_return()
    if (finals /= before + 1 .or. total /= sum_before + 41) error stop 14
    call loop_exits()
    if (finals /= calls) error stop 15
    deallocate(seed%data)
end program
