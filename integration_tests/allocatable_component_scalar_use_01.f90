program allocatable_component_scalar_use_01
    implicit none
    type :: t
        integer, allocatable :: int_value
        real, allocatable :: dbl_value
        logical, allocatable :: log_value
    end type
    type(t) :: p
    integer :: i, ires
    real :: r
    logical :: l
    character(len=100) :: buf

    allocate(p%int_value)
    allocate(p%dbl_value)
    allocate(p%log_value)
    p%int_value = 42
    p%dbl_value = 3.5
    p%log_value = .true.

    ! Direct read of scalar allocatable members into non-allocatable
    ! locals.
    i = p%int_value
    r = p%dbl_value
    l = p%log_value

    if (i /= 42) error stop 1
    if (r /= 3.5) error stop 2
    if (.not. l) error stop 3

    ! Boolean-context read of a scalar allocatable logical member.
    if (p%log_value) then
        ires = 1
    else
        ires = 0
    end if
    if (ires /= 1) error stop 4

    p%log_value = .false.
    if (p%log_value) then
        ires = 1
    else
        ires = 0
    end if
    if (ires /= 0) error stop 5

    ! Pass scalar allocatable members to a subroutine expecting plain
    ! scalar dummy arguments.
    call print_int(p%int_value, buf)
    if (trim(buf) /= "42") error stop 6

    call print_real(p%dbl_value, buf)
    if (trim(buf) /= " 3.50") error stop 7

    print *, "ok"
contains
    subroutine print_int(x, s)
        integer, intent(in) :: x
        character(len=*), intent(out) :: s
        write(s, '(I0)') x
    end subroutine
    subroutine print_real(x, s)
        real, intent(in) :: x
        character(len=*), intent(out) :: s
        write(s, '(F5.2)') x
    end subroutine
end program
