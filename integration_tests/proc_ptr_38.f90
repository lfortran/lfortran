! A procedure pointer can take its interface from a module procedure defined
! later in the module and still be initialized to a procedure that is
! already known (Fortran 2018, 15.4.3.6; #14973).
module proc_ptr_38_a
    implicit none
contains
    integer function forty_two()
        forty_two = 42
    end function

    subroutine set_seven(x)
        integer, intent(out) :: x
        x = 7
    end subroutine
end module

module proc_ptr_38_m
    use proc_ptr_38_a
    implicit none
    procedure(iface_f), pointer :: q => forty_two
    procedure(iface_s), pointer :: r => set_seven
contains
    integer function local_call()
        procedure(iface_f), pointer :: p => forty_two
        local_call = p() + 1
    end function

    integer function iface_f()
        iface_f = 0
    end function

    subroutine iface_s(x)
        integer, intent(out) :: x
        x = 0
    end subroutine
end module

program proc_ptr_38
    use proc_ptr_38_m
    implicit none
    integer :: i
    if (q() /= 42) error stop
    i = 0
    call r(i)
    if (i /= 7) error stop
    if (local_call() /= 43) error stop
    q => iface_f
    if (q() /= 0) error stop
    print *, "ok"
end program
