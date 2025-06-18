module module_call_subroutine_without_type

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
            end subroutine get_r
    end subroutine get_i

end module module_call_subroutine_without_type

program call_subroutine_without_type
    use module_call_subroutine_without_type
end program call_subroutine_without_type