! Standard Fortran counterpart of traits_runtime_polymorphic_source_03: the
! contract IOps is an abstract type that A extends.
module traits_runtime_polymorphic_source_03_oracle_m
    implicit none
    type, abstract :: IOps
    contains
        procedure(twice_interface), deferred, nopass :: twice
        procedure(put_interface), deferred, pass(self) :: put
        procedure(tag_interface), deferred :: tag
    end type
    abstract interface
        integer function twice_interface(k)
            integer, intent(in) :: k
        end function
        subroutine put_interface(k, self)
            import :: IOps
            integer, intent(in) :: k
            class(IOps), intent(inout) :: self
        end subroutine
        character(len=3) function tag_interface(self)
            import :: IOps
            class(IOps), intent(in) :: self
        end function
    end interface
    type, extends(IOps) :: A
        integer :: n = 0
    contains
        procedure, nopass :: twice => a_twice
        procedure, pass(self) :: put => a_put
        procedure :: tag => a_tag
    end type
    type, extends(A) :: B
    contains
        procedure, nopass :: twice => b_twice
        procedure, pass(self) :: put => b_put
        procedure :: tag => b_tag
    end type
contains
    integer function a_twice(k)
        integer, intent(in) :: k
        a_twice = 2*k
    end function
    integer function b_twice(k)
        integer, intent(in) :: k
        b_twice = 20*k
    end function
    subroutine a_put(k, self)
        integer, intent(in) :: k
        class(A), intent(inout) :: self
        self%n = k
    end subroutine
    subroutine b_put(k, self)
        integer, intent(in) :: k
        class(B), intent(inout) :: self
        self%n = -k
    end subroutine
    character(len=3) function a_tag(self)
        class(A), intent(in) :: self
        a_tag = "aaa"
    end function
    character(len=3) function b_tag(self)
        class(B), intent(in) :: self
        b_tag = "bbb"
    end function
    subroutine use_it(x, r, t)
        class(IOps), intent(inout) :: x
        integer, intent(out) :: r
        character(len=3), intent(out) :: t
        r = x%twice(3)
        call x%put(5)
        t = x%tag()
    end subroutine
    subroutine from(o, r, t)
        class(A), intent(inout) :: o
        integer, intent(out) :: r
        character(len=3), intent(out) :: t
        call use_it(o, r, t)
    end subroutine
end module
program traits_runtime_polymorphic_source_03_oracle
    use traits_runtime_polymorphic_source_03_oracle_m
    type(B) :: bb
    type(A) :: aa
    integer :: r
    character(len=3) :: t
    call from(bb, r, t)
    if (r /= 60 .or. bb%n /= -5 .or. t /= "bbb") error stop 1
    call from(aa, r, t)
    if (r /= 6 .or. aa%n /= 5 .or. t /= "aaa") error stop 2
    print *, "PASS"
end program
