module module_call_subroutine_without_type_01

    type :: myType
        real :: r
        contains
            procedure, pass(self) :: get_i
    end type myType

contains
    subroutine get_i(self)
        class(myType), intent(inout) :: self
        call get_r()

        contains
            subroutine get_r()
                print *, "r = ", self % r
                if (self%r /= 1.0) error stop
            end subroutine get_r
    end subroutine get_i

end module module_call_subroutine_without_type_01

program call_subroutine_without_type_01
    use module_call_subroutine_without_type_01
    class(myType), allocatable :: obj
    allocate(obj)
    obj%r = 1.0
    call obj%get_i()
end program call_subroutine_without_type_01
