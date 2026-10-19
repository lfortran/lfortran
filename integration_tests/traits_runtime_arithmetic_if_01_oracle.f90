! The test of an arithmetic IF is evaluated once (F2008 8.2.4), also when
! it is a message sent through a runtime trait slot. Standard Fortran
! counterpart of traits_runtime_arithmetic_if_01.
module traits_runtime_arithmetic_if_01_oracle_m
    implicit none
    type, abstract :: IV
    contains
        procedure(v_i), deferred :: v
    end type
    abstract interface
        integer function v_i(self)
            import IV
            class(IV), intent(in) :: self
        end function
    end interface
    type, extends(IV) :: A
    contains
        procedure :: v => a_v
    end type
    integer :: n = 0
contains
    integer function a_v(self)
        class(A), intent(in) :: self
        n = n + 1
        a_v = 1
    end function
end module

program traits_runtime_arithmetic_if_01_oracle
    use traits_runtime_arithmetic_if_01_oracle_m
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
