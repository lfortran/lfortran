! A subscript of an operand read in place must not share storage with the
! assignment target through COMMON (#16215)
module common_61_m
implicit none
integer :: j, pad(2)
common /c61b/ j, pad
end module

program common_61
implicit none
type t
    integer :: x(3)
end type
type(t) :: arr(3)
integer :: i, pad(2)
common /c61a/ i, pad

arr(1)%x = [1, 2, 3]
arr(2)%x = [10, 20, 30]
arr(3)%x = [100, 200, 300]
i = 1
call host()
call use_module()

contains

! z(1) is the host's i
subroutine host()
    integer :: z(3)
    common /c61a/ z
    z = arr(i)%x + 1
    print *, z
    if (any(z /= [2, 3, 4])) error stop 1
    z(1) = 2
    z = arr(i)%x * 2
    print *, z
    if (any(z /= [20, 40, 60])) error stop 2
end subroutine

! z(1) is the module's j
subroutine use_module()
    use common_61_m, only: j
    integer :: z(3)
    common /c61b/ z
    j = 3
    z = arr(j)%x + 1
    print *, z
    if (any(z /= [101, 201, 301])) error stop 3
end subroutine
end program
