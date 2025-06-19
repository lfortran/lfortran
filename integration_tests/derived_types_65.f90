module derived_types_65_m
    implicit none
  
    type :: t_date
      integer :: year, month, day
    end type
  
    type :: t_address
      character(len=:), allocatable :: city, road_name
      integer :: house_number
    end type
  
    type :: t_person
      character(len=:), allocatable :: first_name, last_name, e_mail
    end type
  
    type, extends(t_person)  :: t_employee
      type(t_date) :: hired_date
      character(len=:), allocatable :: position
      real :: monthly_salary
    end type
  
end module derived_types_65_m
  
program derived_types_65
    use derived_types_65_m
    implicit none

    type(t_employee) :: employee

    employee%first_name = 'John'
    employee%last_name  = 'Doe'
  
    employee%hired_date%year  = 2020

    print *, 'Employee hired in year', employee%hired_date%year
    if (employee%hired_date%year /= 2020) error stop
end program derived_types_65