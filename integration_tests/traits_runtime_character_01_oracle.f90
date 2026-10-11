module traits_runtime_character_01_oracle_m
    implicit none
    integer :: bound_calls = 0
    type :: Leaf
        integer :: offset = 5
    contains
        procedure, pass(self) :: measure => leaf_measure
        procedure, nopass :: next_bound => next_bound
    end type
    type :: Forwarder
    contains
        procedure, nopass :: apply => apply
    end type
contains
    integer function leaf_measure(text, self) result(n)
        character(*), intent(in) :: text
        class(Leaf), intent(in) :: self
        n = len(text) + self%offset
    end function
    integer function next_bound() result(n)
        bound_calls = bound_calls + 1
        n = 3
    end function
    function apply(item, text) result(n)
        class(Leaf), intent(in) :: item
        character(*), intent(in) :: text
        integer :: n
        n = item%measure(text)
    end function
    function read_text(item, text) result(n)
        type(Leaf), intent(in) :: item
        character(*), intent(in) :: text
        integer :: n
        n = item%measure(text)
    end function
    subroutine exercise(view, worker, item)
        class(Leaf), intent(in) :: view
        class(Forwarder), intent(in) :: worker
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

program traits_runtime_character_01_oracle
    use traits_runtime_character_01_oracle_m
    implicit none
    type(Leaf) :: item
    type(Forwarder) :: worker
    call exercise(item, worker, item)
end program
