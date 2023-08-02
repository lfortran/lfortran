module class_08_module
   implicit none
   private

   public :: toml_datetime, toml_date
   type :: toml_date
      integer :: year = 0
      integer :: month = 0
      integer :: day = 0
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => date_to_string
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date), allocatable :: date
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => datetime_to_string
   end type


contains


subroutine date_to_string(lhs, rhs)
   character( len=:), allocatable, intent(out) :: lhs
   class(toml_date), intent(in) :: rhs
end subroutine date_to_string



subroutine datetime_to_string(lhs, rhs)
   character( len=:), allocatable, intent(out) :: lhs
   class(toml_datetime), intent(in) :: rhs
   call rhs%date%to_string(lhs)
end subroutine datetime_to_string


end module class_08_module


program class_08_program
   use class_08_module
   implicit none
   print *, "ok"
end program class_08_program
