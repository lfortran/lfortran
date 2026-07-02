! Test that pure functions require INTENT(IN) or VALUE for dummy arguments
program pure_intent_01
    implicit none
    integer :: x
    x = 5
    print *, add_one(x)
contains
    pure function add_one(n) result(res)
        integer, intent(in) :: n
        integer :: res
        res = n + 1
    end function
end program
