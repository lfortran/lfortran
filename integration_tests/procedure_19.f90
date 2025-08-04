module mod1_procedure_19

   type :: my_type
    real :: value = 0.0
   contains
      procedure :: plus
      generic :: g_proc => plus
   end type my_type

contains

   subroutine plus(self, increment)
      class(my_type), intent(inout) :: self
      real, intent(in) :: increment
      self%value = self%value + increment
   end subroutine plus

end module mod1_procedure_19

module mod2_procedure_19
   use mod1_procedure_19
end module mod2_procedure_19

program procedure_19
    use mod2_procedure_19
    type(my_type) :: obj

    real :: test_increment = 5.0
   
    call obj%g_proc(test_increment)
    print *, 'After adding', test_increment, ':', obj%value
    if (abs(obj%value - 5.0) > 1.0e-6) error stop
   
    call obj%g_proc(3.0)
    print *, 'After adding 3.0:', obj%value
    if (abs(obj%value - 8.0) > 1.0e-6) error stop
end program procedure_19
