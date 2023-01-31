! TODO: Modify this test to actually print something or perform some operation
module debug_1
    implicit none
    type :: t_1
        integer, private :: descriptor
    end type t_1
contains
    function f_1() result(self)
        type(t_1) :: self
    end function f_1

    function f_2() result(self)
        type(t_1) :: self
    end function f_2

end module debug_1

module debug_2
    use debug_1, only: f_2, f_1
    implicit none
contains
    subroutine sub_2()
        print*, f_2()
        print*, f_1()
    end subroutine sub_2

end module debug_2
