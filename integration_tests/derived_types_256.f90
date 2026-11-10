module derived_types_256_mod
    implicit none

    type :: t_person
      character(len=:), allocatable :: first_name, last_name
    end type

    type, extends(t_person)  :: t_employee
      integer :: day, month, year
    end type

end module derived_types_256_mod

program derived_types_256
    ! deferred-length allocatable character components inherited from the
    ! parent type, assigned through an extended type without allocatable
    ! components of its own
    use derived_types_256_mod
    implicit none

    type(t_employee) :: employee

    employee%first_name = 'John'
    employee%last_name  = 'Doe'

    employee%year  = 2020
    employee%month = 1
    employee%day   = 20

    print *, 'Employee hired on ', employee%day, '/', employee%month, '/', employee%year
    if (employee%first_name /= 'John') error stop
    if (len(employee%first_name) /= 4) error stop
    if (employee%last_name /= 'Doe') error stop
    if (len(employee%last_name) /= 3) error stop
    if (employee%t_person%first_name /= 'John') error stop
    if (employee%day /= 20 .or. employee%month /= 1 .or. employee%year /= 2020) error stop
end program derived_types_256
