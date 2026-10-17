! A derived type declared in a module procedure binds module procedures that
! are defined after that module procedure (#14762).
module class_procedure_nopass_03_m
    implicit none
contains
    integer function h()
        type :: t
        contains
            procedure, nopass :: get
        end type
        type(t) :: a
        h = a%get()
    end function

    subroutine s(r)
        integer, intent(out) :: r
        call inner(r)
    contains
        subroutine inner(r)
            integer, intent(out) :: r
            type :: u
            contains
                procedure, nopass :: set
                procedure, nopass :: twice => get_twice
            end type
            type(u) :: b
            call b%set(r)
            r = r + b%twice()
        end subroutine
    end subroutine

    integer function get()
        get = 3
    end function

    subroutine set(r)
        integer, intent(out) :: r
        r = 5
    end subroutine

    integer function get_twice()
        get_twice = 2*get()
    end function
end module

program class_procedure_nopass_03
    use class_procedure_nopass_03_m
    implicit none
    integer :: r
    if (h() /= 3) error stop 1
    call s(r)
    if (r /= 11) error stop 2
    print *, "ok"
end program
