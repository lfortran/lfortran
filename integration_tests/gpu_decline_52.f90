! gpu decline reason: StructAllocatableArrayMember, StructConstantComponent
! A derived type the loop reads as a whole, whose deferred-length character
! array component would reach the kernel as a buffer of its own, is declined
! as one with a component of a constant length is: the component is never
! named in the loop, or it is one of a component stored inline. So is a
! named constant of such a type that the loop names, whether it is local or
! a module's.
module gpu_decline_52_m
    implicit none
    type :: t
        integer :: n
        character(len=:), allocatable :: s(:)
    end type
    type(t), parameter :: cm = t(4, null())
end module

program gpu_decline_52
    use gpu_decline_52_m, only: t, cm
    implicit none
    type :: u
        type(t) :: y
    end type
    type(t) :: x
    type(u) :: w
    type(t), parameter :: c = t(3, null())
    integer :: i
    integer :: z(5)

    x%n = 2
    allocate(character(len=2) :: x%s(5))
    x%s = ["ab", "cd", "ef", "gh", "ij"]
    z = 0
    do concurrent (i = 1:5)
        z(i) = x%n
    end do
    if (sum(z) /= 10) error stop "unused component"

    allocate(character(len=2) :: w%y%s(5))
    w%y%s = ["ab", "cd", "ef", "gh", "ij"]
    z = 0
    do concurrent (i = 1:5)
        if (w%y%s(i)(1:1) == "e") z(i) = len(w%y%s(i))
    end do
    if (sum(z) /= 2 .or. z(3) /= 2) error stop "inline component"

    z = 0
    do concurrent (i = 1:5)
        z(i) = c%n
    end do
    if (sum(z) /= 15) error stop "local constant"

    z = 0
    do concurrent (i = 1:5)
        z(i) = cm%n
    end do
    if (sum(z) /= 20) error stop "module constant"
    print *, "ok"
end program
