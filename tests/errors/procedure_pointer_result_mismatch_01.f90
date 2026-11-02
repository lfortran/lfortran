! A procedure pointer with an implicit interface that is a function can only
! be associated with a function of the same type and kind (F2018 10.2.2.4
! p6), and a function referenced through an untyped one has its implicit type.
subroutine kind_mismatch()
    implicit none
    procedure(real), pointer :: p
    p => g  ! {Error} the result of the target is real(8), not real(4)
contains
    real(8) function g(x)
        real(8), intent(in) :: x
        g = x + 1
    end function
end subroutine

subroutine subroutine_target()
    implicit none
    procedure(integer), pointer :: ip
    ip => s  ! {Error} the target is a subroutine
contains
    subroutine s()
    end subroutine
end subroutine

subroutine component_mismatch()
    implicit none
    type :: holder
        procedure(real), pointer, nopass :: f => null()
    end type
    type(holder) :: h
    h%f => g  ! {Error} the result of the target is integer(4), not real(4)
contains
    integer function g(x)
        integer, intent(in) :: x
        g = x
    end function
end subroutine

subroutine untyped_function_reference()
    implicit none
    procedure(), pointer :: p
    integer :: y
    p => f
    y = p(1)  ! {Error} `p` has no implicit type
contains
    integer function f(x)
        integer, intent(in) :: x
        f = x
    end function
end subroutine

subroutine character_length_mismatch()
    implicit none
    procedure(character(len=5)), pointer :: cp
    cp => cf3  ! {Error} the result of the target is character(len=3), not character(len=5)
contains
    character(len=3) function cf3(x)
        integer, intent(in) :: x
        cf3 = "abc"
    end function
end subroutine

program untyped_function_reference_in_program
    implicit none
    procedure(), pointer :: p
    integer :: y
    p => f
    y = p(1)  ! {Error} `p` has no implicit type
contains
    integer function f(x)
        integer, intent(in) :: x
        f = x
    end function
end program

subroutine explicit_interface_required()
    implicit none
    procedure(), pointer :: p
    p => total  ! {Error} `x` of the target is assumed-shape
    p => opt  ! {Error} `y` of the target is optional
contains
    subroutine total(x, s)
        integer, intent(in) :: x(:)
        integer, intent(out) :: s
        s = sum(x)
    end subroutine
    subroutine opt(y)
        integer, intent(in), optional :: y
    end subroutine
end subroutine

subroutine explicit_interface_required_2()
    implicit none
    procedure(), pointer :: p
    p => vol  ! {Error} `x` of the target is volatile
    p => coa  ! {Error} `x` of the target is a coarray
    p => chn  ! {Error} result length is not a constant expression
contains
    subroutine vol(x)
        integer, volatile :: x
    end subroutine
    subroutine coa(x)
        integer :: x[*]
    end subroutine
    function chn(n)
        integer, intent(in) :: n
        character(len=n) :: chn
        chn = ""
    end function
end subroutine
