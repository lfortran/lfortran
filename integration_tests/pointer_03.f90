module test_pointer_association
    implicit none
    integer, target :: i
    contains
    function mergei32() result(r)
        integer, pointer :: r
        i=5
        r=>i
    end function
end module
program pointer_03
    use test_pointer_association
    implicit none
    integer, pointer :: r
    r=>mergei32()
    if (r /= 5) then
        print *, "Test failed: r does not point to the expected value"
        error stop
    end if
end program