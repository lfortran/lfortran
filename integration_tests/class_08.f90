module class_08_module
   implicit none
   private

   public :: toml_datetime, toml_date
   type :: toml_date
      integer :: year = 0
      integer :: month = 0
      integer :: day = 0
   contains
      procedure, pass(rhs) :: to_string => date_to_string
   end type

   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date), allocatable :: date
   contains
      procedure, pass(rhs) :: to_string => datetime_to_string
   end type

contains

    subroutine date_to_string(lhs, rhs)
        character(len=:), allocatable, intent(out) :: lhs
        class(toml_date), intent(in) :: rhs
        allocate(character(10) :: lhs)
        write(lhs, "(i0, '-', i0, '-', i0)") rhs%day, rhs%month, rhs%year
    end subroutine date_to_string

    subroutine datetime_to_string(lhs, rhs)
        character(len=:), allocatable, intent(out) :: lhs
        class(toml_datetime), intent(in) :: rhs
        call rhs%date%to_string(lhs)
    end subroutine datetime_to_string

end module class_08_module

program class_08_program
    use class_08_module
    implicit none
    ! TODO
    ! character(:), allocatable :: date
    ! type(toml_datetime) :: t_datetime
    ! allocate(t_datetime%date)
    ! t_datetime%date%year = 2023
    ! t_datetime%date%month = 08
    ! t_datetime%date%day = 28
    ! print *, t_datetime%date
    ! call t_datetime%to_string(date)
    ! if (date /= "28-8-2023") error stop
    print *, "ok"
end program class_08_program
