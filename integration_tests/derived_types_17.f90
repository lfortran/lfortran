module module_1
    implicit none
    private
    public :: add, subtract
    type :: t_1
        integer :: num = 100
    end type t_1
contains
    function add() result(self)
        type(t_1) :: self
        self%num = self%num + self%num
    end function add

    function subtract() result(self)
        type(t_1) :: self
        self%num = self%num * self%num
    end function subtract

end module module_1

module module_2
    use module_1, only: subtract, add
    implicit none
contains
    subroutine sub_2()
        print*, subtract()
        print*, add()
    end subroutine sub_2

end module module_2

program derived_types_14
    use module_2, only: sub_2
    implicit none
    call sub_2()
end program derived_types_14
