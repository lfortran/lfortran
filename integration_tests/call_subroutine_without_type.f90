module module_call_subroutine_without_type

    type :: myType
        real :: r
        contains
            procedure, pass(self) :: get_i
    end type myType

contains
    subroutine get_i(self)
        class(myType), intent(inout) :: self
        call get_r(self)  ! pass self explicitly
        
        contains
            subroutine get_r(s)
                class(myType), intent(inout) :: s
                print *, "r = ", s % r
                if (s%r /= 1.0) error stop
            end subroutine get_r
    end subroutine get_i

end module module_call_subroutine_without_type

program call_subroutine_without_type
    use module_call_subroutine_without_type
    class(myType), allocatable :: obj
    allocate(obj)
    obj%r = 1.0
    call obj%get_i()
end program call_subroutine_without_type