module pointer_section_10_mod
    implicit none
    integer, target, save :: buf(2, 3)

    type :: t
    contains
        procedure, nopass :: get
    end type t

    abstract interface
        function getter() result(r)
            integer, pointer, contiguous :: r(:, :)
        end function getter
    end interface

contains

    function get() result(r)
        integer, pointer, contiguous :: r(:, :)
        r => buf
    end function get

    subroutine remap_dummy_procedure(f)
        procedure(getter) :: f
        integer, pointer :: p(:)
        p(1:6) => f()
        if (p(1) /= 10 .or. p(6) /= 60) error stop 1
    end subroutine remap_dummy_procedure

end module pointer_section_10_mod

program pointer_section_10
    ! Pointer bounds-remapping onto simply contiguous data targets: a
    ! reference to a function with a CONTIGUOUS pointer result through a
    ! binding, a dummy procedure and a procedure pointer, and associate
    ! names for simply contiguous selectors
    use pointer_section_10_mod
    implicit none
    type(t) :: s
    procedure(getter), pointer :: fp
    integer, pointer :: p(:)
    integer :: i

    buf = reshape([(10*i, i = 1, 6)], [2, 3])

    p(1:6) => s%get()
    if (p(2) /= 20 .or. p(6) /= 60) error stop 2
    fp => get
    p(1:6) => fp()
    if (p(3) /= 30 .or. p(5) /= 50) error stop 3
    call remap_dummy_procedure(get)

    associate (y => buf)
        p(1:6) => y
        if (p(1) /= 10 .or. p(6) /= 60) error stop 4
    end associate
    associate (z => buf(:, 2:3))
        p(1:4) => z
        if (size(p) /= 4) error stop 5
        if (p(1) /= 30 .or. p(4) /= 60) error stop 6
    end associate

    print *, "passed"
end program pointer_section_10
