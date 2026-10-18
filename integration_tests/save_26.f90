module save_26_m
implicit none
integer :: nfinal = 0
abstract interface
    integer function iface(x)
        integer, intent(in) :: x
    end function
end interface
type :: tf
    integer :: n = 0
    procedure(iface), pointer, nopass :: p => null()
contains
    final :: fin
end type
contains
    subroutine fin(x)
        type(tf), intent(inout) :: x
        nfinal = nfinal + 1
    end subroutine

    integer function add_one(x)
        integer, intent(in) :: x
        add_one = x + 1
    end function

    ! A procedure target makes the initializer non-constant data, so it is
    ! assigned the first time control reaches the procedure. Initialization
    ! is not intrinsic assignment and does not finalize the variable.
    integer function implicit_save(x)
        integer, intent(in) :: x
        type(tf) :: s = tf(1, add_one)
        implicit_save = s%p(x) + s%n
        s%n = s%n + 1
    end function

    integer function explicit_save(x)
        integer, intent(in) :: x
        type(tf), save :: s = tf(10, add_one)
        explicit_save = s%p(x) + s%n
        s%n = s%n + 1
    end function
end module

program save_26
use save_26_m, only: implicit_save, explicit_save, nfinal
implicit none
if (implicit_save(1) /= 3) error stop 1
if (implicit_save(1) /= 4) error stop 2
if (implicit_save(1) /= 5) error stop 3
if (explicit_save(1) /= 12) error stop 4
if (explicit_save(1) /= 13) error stop 5
if (explicit_save(1) /= 14) error stop 6
print *, nfinal
if (nfinal /= 0) error stop 7
end program
