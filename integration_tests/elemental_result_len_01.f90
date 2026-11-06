! The result length of an elemental function may use a dummy argument
! as the argument of an inquiry function such as len (C15104).
module elemental_result_len_01_m
    implicit none
contains
    elemental function twice(x) result(r)
        character(len=*), intent(in) :: x
        character(len=2*len(x)) :: r
        r = x // x
    end function
end module

program elemental_result_len_01
    use elemental_result_len_01_m
    implicit none
    character(len=3) :: a(2)
    character(len=6) :: b(2)
    a = ["abc", "de "]
    b = twice(a)
    if (b(1) /= "abcabc") error stop
    if (b(2) /= "de de ") error stop
    if (len(twice(a(1)(1:2))) /= 4) error stop
    print *, b
end program
