! gpu decline reason: ScalarNotNumeric, ArrayElementNotNumeric, StructAllocatableArrayMember, StructConstantComponent
! A deferred-length character variable that a loop reaches other than as a
! variable it names is declined as gpu_decline_50's are: a module variable a
! procedure the loop calls reads, and an allocatable array component of a
! derived type the loop reads an element of. A module variable of derived
! type that a procedure the loop calls reads, with a deferred-length
! character array in a component stored inline, is declined as one of a
! constant length is, whether or not the procedure reads the string. So is
! a named constant of such a type that a procedure the loop calls reads,
! through module or host association.
module gpu_decline_51_m
    implicit none
    type :: u
        integer :: k
        character(len=:), allocatable :: s(:)
    end type
    type :: v
        type(u) :: y
    end type
    character(len=:), allocatable :: g
    type(v) :: gx
    type(v), parameter :: cv = v(u(3, null()))
contains
    pure integer function f(i)
        integer, intent(in) :: i
        f = 0
        if (g(i:i) == "c") f = 1
    end function

    pure integer function f2(i)
        integer, intent(in) :: i
        f2 = 0
        if (gx%y%s(i)(1:1) == "e") f2 = 1
    end function

    pure integer function f3(i)
        integer, intent(in) :: i
        f3 = gx%y%k + i - i
    end function

    pure integer function f4(i)
        integer, intent(in) :: i
        f4 = cv%y%k + i - i
    end function
end module

program gpu_decline_51
    use gpu_decline_51_m, only: u, g, gx, f, f2, f3, f4
    implicit none
    type :: t
        character(len=:), allocatable :: s(:)
    end type
    type(t) :: x
    type(u), parameter :: cp = u(4, null())
    integer :: i
    integer :: z(5)

    g = "abcde"
    z = 0
    do concurrent (i = 1:5)
        z(i) = f(i)
    end do
    if (sum(z) /= 1 .or. z(3) /= 1) error stop "module variable"

    allocate(character(len=2) :: x%s(5))
    x%s = ["ab", "cd", "ef", "gh", "ij"]
    z = 0
    do concurrent (i = 1:5)
        if (x%s(i)(1:1) == "e") z(i) = len(x%s(i))
    end do
    if (sum(z) /= 2 .or. z(3) /= 2) error stop "component array"

    gx%y%k = 2
    allocate(character(len=2) :: gx%y%s(5))
    gx%y%s = ["ab", "cd", "ef", "gh", "ij"]
    z = 0
    do concurrent (i = 1:5)
        z(i) = f2(i)
    end do
    if (sum(z) /= 1 .or. z(3) /= 1) error stop "inline component"

    z = 0
    do concurrent (i = 1:5)
        z(i) = f3(i)
    end do
    if (sum(z) /= 10) error stop "unused inline component"

    z = 0
    do concurrent (i = 1:5)
        z(i) = f4(i)
    end do
    if (sum(z) /= 15) error stop "module constant"

    z = 0
    do concurrent (i = 1:5)
        z(i) = f5(i)
    end do
    if (sum(z) /= 20) error stop "host constant"
    print *, "ok"
contains
    pure integer function f5(i)
        integer, intent(in) :: i
        f5 = cp%k + i - i
    end function
end program
