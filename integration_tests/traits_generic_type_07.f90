! Clients of the generic types of traits_generic_type_07_lib, compiled
! against its module file. A module and the main program each own instances.
! They construct a `Box` through its initializer, inferred or with explicit
! type arguments, which reaches the private components through the
! definition's own procedures; they never give a private component a value.
module traits_generic_type_07_client
    use traits_generic_type_07_lib, only: Box
    implicit none
    private
    public :: client_count
contains
    integer function client_count(n)
        integer, intent(in) :: n
        type(Box{integer}) :: b
        b = Box{integer}(n)
        b = b%bumped()
        client_count = b%count_of()
    end function client_count
end module traits_generic_type_07_client

program traits_generic_type_07
    use traits_generic_type_07_lib
    use traits_generic_type_07_client
    implicit none
    type(Box{integer}) :: b
    type(Box{real}) :: c
    type(Stack{integer}) :: s
    type(Tagged{integer}) :: t

    b = Box{integer}(4)
    if (b%value_of() /= 4 .or. b%count_of() /= 1) error stop 1
    b = b%bumped()
    if (b%value_of() /= 4 .or. b%count_of() /= 2) error stop 2
    c = Box(2.5)
    if (c%value_of() /= 2.5 .or. c%count_of() /= 1) error stop 3
    c = c%bumped()
    if (c%value_of() /= 2.5 .or. c%count_of() /= 2) error stop 4

    s = Stack{integer}()
    if (s%size_of() /= 0) error stop 5

    t = Tagged{integer}(item=5)
    if (t%item /= 5 .or. t%tag_of() /= 7) error stop 6
    t = Tagged{integer}(6)
    if (t%item /= 6 .or. t%tag_of() /= 7) error stop 7

    if (client_count(3) /= 2) error stop 8
    print *, b%count_of(), c%value_of(), t%item
end program traits_generic_type_07
