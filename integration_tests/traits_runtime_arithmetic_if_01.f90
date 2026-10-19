! The test of an arithmetic IF is evaluated once (F2008 8.2.4), also when
! it is a message sent through a runtime trait slot.
module traits_runtime_arithmetic_if_01_m
    implicit none
    abstract interface :: IV
        integer function v(self)
            type(itself), pass, intent(in) :: self
        end function
    end interface
    type :: A
    end type
    implements IV :: A
        procedure, pass :: v => a_v
    end implements
    integer :: n = 0
contains
    integer function a_v(self)
        class(A), intent(in) :: self
        n = n + 1
        a_v = 1
    end function
end module

program traits_runtime_arithmetic_if_01
    use traits_runtime_arithmetic_if_01_m
    implicit none
    class(IV), allocatable :: x
    allocate(x, source=A())
    if (x%v()) 10, 20, 30
10  print *, 'neg'
    goto 40
20  print *, 'zero'
    goto 40
30  print *, 'pos'
40  print *, n
    if (n /= 1) error stop 1
end program
