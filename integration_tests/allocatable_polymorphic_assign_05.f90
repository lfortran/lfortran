module allocatable_polymorphic_assign_05_m
    implicit none
    type :: t
        integer :: b = 0
    end type
    type, extends(t) :: t2
        integer :: d = 0
    end type
    type :: holder_t
        class(*), allocatable :: u(:)
    end type
contains
    elemental function plus_ten(x) result(r)
        class(t), intent(in) :: x
        type(t) :: r
        r%b = x%b + 10
    end function

    elemental function to_int(x) result(r)
        class(*), intent(in) :: x
        integer :: r
        select type (x)
        type is (real)
            r = int(x) + 100
        type is (integer)
            r = x + 1
        class default
            r = -7
        end select
    end function
end module

program allocatable_polymorphic_assign_05
    ! F2018 10.2.1.3: the expression is evaluated before an allocated
    ! polymorphic variable of another dynamic type is deallocated and
    ! reallocated, also when the expression refers to the variable and is
    ! evaluated element by element. gfortran 13 evaluates these assignments
    ! after reallocating the variable, so this test is not run with it.
    use allocatable_polymorphic_assign_05_m
    implicit none
    class(t), allocatable :: v(:)
    class(*), allocatable :: u(:)
    type(t2) :: y(3)
    type(holder_t) :: h

    ! class(t) of dynamic type t2 assigned a type(t) elemental result
    y%b = [1, 2, 3]
    y%d = 5
    v = [t(1), t(2), t(3)]
    v = y
    v = plus_ten(v)
    select type (v)
    type is (t)
        if (size(v) /= 3) error stop 1
        if (any(v%b /= [11, 12, 13])) error stop 2
    class default
        error stop 3
    end select

    ! class(*) of type real assigned an integer elemental result
    u = [1.5, 2.5, 3.5]
    u = to_int(u)
    select type (u)
    type is (integer)
        if (size(u) /= 3) error stop 4
        if (any(u /= [101, 102, 103])) error stop 5
    class default
        error stop 6
    end select

    ! The same through an associate name of the variable
    u = [1.5, 2.5, 3.5]
    select type (q => u)
    type is (real)
        u = int(q) + 1
    end select
    select type (u)
    type is (integer)
        if (size(u) /= 3) error stop 7
        if (any(u /= [2, 3, 4])) error stop 8
    class default
        error stop 9
    end select

    ! The same for a component
    h%u = [1.5, 2.5, 3.5]
    h%u = to_int(h%u)
    select type (q => h%u)
    type is (integer)
        if (size(q) /= 3) error stop 10
        if (any(q /= [101, 102, 103])) error stop 11
    class default
        error stop 12
    end select
    print *, "PASS"
end program allocatable_polymorphic_assign_05
