program test_nested_array_intrinsics
    implicit none

    character(len=1), parameter :: tokens(1) = ["A"]

    ! Test spread inside index and any with character arrays
    if (.not. hit("A")) error stop 1
    if (hit("B")) error stop 2

    ! Test spread inside sum with integer arrays
    if (sum(spread([1, 2], 1, 2)) /= 6) error stop 3

contains

    function hit(deferred_text) result(found)
        character(*), intent(in) :: deferred_text
        logical :: found
        found = any(index(tokens, spread(deferred_text, 1, 1)) /= 0)
    end function hit

end program test_nested_array_intrinsics
