module traits_runtime_character_01_m
    implicit none
    integer :: bound_calls = 0
    abstract interface :: IString
        integer function measure(text)
            character(*), intent(in) :: text
        end function
        integer function next_bound()
        end function
    end interface
    abstract interface :: IForward
        function apply{IString :: T}(item, text) result(n)
            type(T), intent(in) :: item
            character(*), intent(in) :: text
            integer :: n
        end function
    end interface
    type :: Leaf
        integer :: offset = 5
    end type
    type :: Forwarder
    end type
    implements IString :: Leaf
        procedure, pass(self) :: measure => leaf_measure
        procedure, nopass :: next_bound => next_bound
    end implements
    implements IForward :: Forwarder
        procedure, nopass :: apply => apply
    end implements
contains
    integer function leaf_measure(text, self) result(n)
        character(*), intent(in) :: text
        type(Leaf), intent(in) :: self
        n = len(text) + self%offset
    end function
    integer function next_bound() result(n)
        bound_calls = bound_calls + 1
        n = 3
    end function
    function apply{IString :: U}(item, text) result(n)
        type(U), intent(in) :: item
        character(*), intent(in) :: text
        integer :: n
        n = item%measure(text)
    end function
    function read_text{IString :: T}(item, text) result(n)
        type(T), intent(in) :: item
        character(*), intent(in) :: text
        integer :: n
        n = item%measure(text)
    end function
    subroutine exercise(view, worker, item)
        class(IString), intent(in) :: view
        class(IForward), intent(in) :: worker
        type(Leaf), intent(in) :: item
        character(9) :: text
        text = "abcdefghi"
        if (view%measure(text(1:view%next_bound())) /= 8) error stop 1
        if (bound_calls /= 1) error stop 2
        if (view%measure("") /= 5) error stop 3
        if (worker%apply(item, text(1:4)) /= 9) error stop 4
        if (read_text(item, text(1:2)) /= 7) error stop 5
    end subroutine
end module
