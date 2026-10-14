! Ordinary Fortran around the TD-36 receiver marker: pass, nopass and itself
! remain ordinary names of variables, dummies and a derived type, and
! type-bound procedures and procedure pointer components keep the standard
! PASS, PASS(name) and NOPASS rules, including a passed object after other
! arguments, a passed-object dummy named pass and keyword arguments.
module traits_receiver_intent_08_m
    implicit none
    type :: itself
        integer :: n = 0
        procedure(bump_iface), pointer, pass(self) :: hook => null()
        procedure(peek_iface), pointer, nopass :: peeker => null()
    contains
        procedure, pass(self) :: add => itself_add
        procedure :: get => itself_get
        procedure, nopass :: zero => itself_zero
        procedure, pass(pass) :: twice => itself_twice
    end type itself
    abstract interface
        subroutine bump_iface(by, self)
            import :: itself
            integer, intent(in) :: by
            class(itself), intent(inout) :: self
        end subroutine
        integer function peek_iface(object)
            import :: itself
            type(itself), intent(in) :: object
        end function
    end interface
contains
    subroutine itself_add(by, self)
        integer, intent(in) :: by
        class(itself), intent(inout) :: self
        self%n = self%n + by
    end subroutine
    integer function itself_get(self)
        class(itself), intent(in) :: self
        itself_get = self%n
    end function
    integer function itself_zero()
        itself_zero = 0
    end function
    subroutine itself_twice(nopass, pass)
        integer, intent(in) :: nopass
        class(itself), intent(inout) :: pass
        pass%n = pass%n + 2 * nopass
    end subroutine
    integer function peek(object)
        type(itself), intent(in) :: object
        peek = object%n
    end function
end module traits_receiver_intent_08_m

program traits_receiver_intent_08
    use traits_receiver_intent_08_m
    implicit none
    integer :: pass, values(2)
    integer nopass
    type(itself) :: c
    pass = 1
    nopass = 2
    values = [pass, nopass]
    call c%add(pass)
    call c%add(by=nopass)
    if (c%get() /= 3) error stop 1
    c%hook => itself_add
    call c%hook(3)
    call c%hook(by=values(2))
    if (c%n /= 8) error stop 2
    c%peeker => peek
    if (c%peeker(c) /= 8 .or. c%peeker(object=c) /= 8) error stop 3
    call c%twice(nopass=5)
    call c%twice(pass)
    if (c%get() /= 20 .or. c%zero() /= 0) error stop 4
    call itself_twice(pass=c, nopass=values(1))
    if (c%get() /= 22) error stop 5
    print '(a)', 'traits_receiver_intent_08 ok'
end program
