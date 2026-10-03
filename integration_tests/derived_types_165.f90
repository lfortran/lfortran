! Initializers from a use-imported derived-type parameter whose type has a
! component of another derived type that the initializing module never
! imports. The nested type is imported under a name users cannot spell, in
! the scope around the component defaults, not inside the types.
module derived_types_165_a
    implicit none
    type :: inner
        integer :: k = 0
    end type
    type :: t
        integer :: i = 0
        type(inner) :: n
    end type
    type(inner), parameter :: zin = inner(3)
    type(t), parameter :: z = t(7, zin)
end module

module derived_types_165_b
    use derived_types_165_a
    private
    public :: t, z
end module

module derived_types_165_c
    use derived_types_165_b, only: u => t, z
    implicit none
    type :: h
        type(u) :: c = z
    end type
    type :: h2
        type(u) :: d = z
    end type
    type(u) :: mv = z
contains
    integer function g()
        use derived_types_165_b, only: z
        type(u) :: x = z
        g = x%i
    end function
end module

program derived_types_165
    use derived_types_165_c
    implicit none
    type(h) :: x
    type(h2) :: y
    if (x%c%i /= 7 .or. x%c%n%k /= 3) error stop 1
    if (y%d%i /= 7 .or. y%d%n%k /= 3) error stop 2
    if (mv%i /= 7 .or. mv%n%k /= 3) error stop 3
    if (g() /= 7) error stop 4
    print *, "ok"
end program
