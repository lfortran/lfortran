! Explicit trait receivers (TD-36): declaration and conformance errors. A
! message marks its receiver with a bare PASS attribute on exactly one
! scalar, nonpointer, nonallocatable, non-VALUE type(itself) dummy; a message
! without one is receiverless. Each module whose name contains _bad_ has
! exactly one error, so that an abandoned unit does not hide another case.
! R01-R14 are declarations, R15-R17 the PASS attribute outside a trait
! message, R18-R27 implementations of valid traits, and R28-R34 attribute
! statements that name the receiver: every statement of a message that names
! its receiver gives it attributes, not only the one with PASS. R35-R36, the
! POINTER statements, which need --implicit-interface, are in
! continue_compilation_traits_receiver_03.f90.

! R01: PASS marks a dummy of another type.
module cc_traits_receiver_bad_r01
    implicit none
    abstract interface :: IR01
        integer function f(n)
            integer, intent(in), pass :: n
        end function
    end interface
end module cc_traits_receiver_bad_r01

! R02: a message has at most one receiver.
module cc_traits_receiver_bad_r02
    implicit none
    abstract interface :: IR02
        subroutine s(a, b)
            type(itself), intent(in), pass :: a
            type(itself), intent(inout), pass :: b
        end subroutine
    end interface
end module cc_traits_receiver_bad_r02

! R03: a function result is not a dummy.
module cc_traits_receiver_bad_r03
    implicit none
    abstract interface :: IR03
        function f(self) result(r)
            type(itself), intent(in), pass :: self
            integer, pass :: r
        end function
    end interface
end module cc_traits_receiver_bad_r03

! R04: the receiver is scalar.
module cc_traits_receiver_bad_r04
    implicit none
    abstract interface :: IR04
        integer function f(self)
            type(itself), intent(in), pass :: self(:)
        end function
    end interface
end module cc_traits_receiver_bad_r04

! R05: the receiver is not a pointer.
module cc_traits_receiver_bad_r05
    implicit none
    abstract interface :: IR05
        integer function f(self)
            type(itself), pointer, intent(in), pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r05

! R06: the receiver is not allocatable.
module cc_traits_receiver_bad_r06
    implicit none
    abstract interface :: IR06
        subroutine s(self)
            type(itself), allocatable, intent(inout), pass :: self
        end subroutine
    end interface
end module cc_traits_receiver_bad_r06

! R07: the receiver does not have the VALUE attribute.
module cc_traits_receiver_bad_r07
    implicit none
    abstract interface :: IR07
        integer function f(self)
            type(itself), value, pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r07

! R08: an OPTIONAL receiver is not implemented yet.
module cc_traits_receiver_bad_r08
    implicit none
    abstract interface :: IR08
        integer function f(self)
            type(itself), optional, intent(in), pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r08

! R09: a TARGET receiver is not implemented yet.
module cc_traits_receiver_bad_r09
    implicit none
    abstract interface :: IR09
        integer function f(self)
            type(itself), target, intent(in), pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r09

! R10: a VOLATILE receiver is not implemented yet.
module cc_traits_receiver_bad_r10
    implicit none
    abstract interface :: IR10
        subroutine s(self)
            type(itself), volatile, intent(inout), pass :: self
        end subroutine
    end interface
end module cc_traits_receiver_bad_r10

! R11: the receiver marker is a bare PASS; PASS(name) belongs to bindings.
module cc_traits_receiver_bad_r11
    implicit none
    abstract interface :: IR11
        integer function f(self)
            type(itself), intent(in), pass(self) :: self
        end function
    end interface
end module cc_traits_receiver_bad_r11

! R12: NOPASS belongs to bindings, not to dummy declarations.
module cc_traits_receiver_bad_r12
    implicit none
    abstract interface :: IR12
        integer function f(n)
            integer, intent(in), nopass :: n
        end function
    end interface
end module cc_traits_receiver_bad_r12

! R13: an operator requirement has no receiver.
module cc_traits_receiver_bad_r13
    implicit none
    abstract interface :: IR13
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in), pass :: lhs
            type(itself), intent(in) :: rhs
            type(itself) :: res
        end function
    end interface
end module cc_traits_receiver_bad_r13

! R14: nor does an initializer requirement.
module cc_traits_receiver_bad_r14
    implicit none
    abstract interface :: IR14
        initial(x)
            type(itself), intent(in), pass :: x
        end initial
    end interface
end module cc_traits_receiver_bad_r14

! R15: PASS on a dummy of an ordinary procedure.
module cc_traits_receiver_bad_r15
    implicit none
    type :: Box
        integer :: n = 0
    end type
contains
    subroutine take(self)
        type(Box), intent(inout), pass :: self
        self%n = 1
    end subroutine
end module cc_traits_receiver_bad_r15

! R16: NOPASS on a dummy of an ordinary procedure.
module cc_traits_receiver_bad_r16
    implicit none
contains
    subroutine take(k)
        integer, intent(in), nopass :: k
        print *, k
    end subroutine
end module cc_traits_receiver_bad_r16

! R17: PASS(name) on a local variable.
module cc_traits_receiver_bad_r17
    implicit none
contains
    subroutine take(k)
        integer, intent(in) :: k
        integer, pass(k) :: local
        local = k
        print *, local
    end subroutine
end module cc_traits_receiver_bad_r17

! Valid traits implemented by R18-R27.
module cc_traits_receiver_contracts
    implicit none
    abstract interface :: IRead
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IWrite
        subroutine put(v, self)
            integer, intent(in) :: v
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
    abstract interface :: IClear
        subroutine clear(self)
            type(itself), intent(out), pass :: self
        end subroutine
    end interface
    abstract interface :: ITouch
        subroutine touch(self)
            type(itself), pass :: self
        end subroutine
    end interface
    abstract interface :: IMake
        integer function make(n)
            integer, intent(in) :: n
        end function
    end interface
    abstract interface :: ILeft
        subroutine bump(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    abstract interface :: IRightFree
        subroutine bump(by)
            integer, intent(in) :: by
        end subroutine
    end interface
    abstract interface :: IRightIn
        subroutine bump(by, it)
            integer, intent(in) :: by
            type(itself), intent(in), pass :: it
        end subroutine
    end interface
end module cc_traits_receiver_contracts

! R18: a receiver message needs a passed object, not NOPASS.
module cc_traits_receiver_bad_r18
    use cc_traits_receiver_contracts, only: IRead
    implicit none
    type :: T18
        integer :: n = 0
    end type
    implements IRead :: T18
        procedure, nopass :: get => get18
    end implements
contains
    integer function get18()
        get18 = 18
    end function
end module cc_traits_receiver_bad_r18

! R19: nor a static binding.
module cc_traits_receiver_bad_r19
    use cc_traits_receiver_contracts, only: IRead
    implicit none
    type :: T19
        integer :: n = 0
    end type
    implements IRead :: T19
        procedure, static :: get => get19
    end implements
contains
    integer function get19()
        get19 = 19
    end function
end module cc_traits_receiver_bad_r19

! R20: a receiverless message needs NOPASS, not a passed object.
module cc_traits_receiver_bad_r20
    use cc_traits_receiver_contracts, only: IMake
    implicit none
    type :: T20
        integer :: n = 0
    end type
    implements IMake :: T20
        procedure, pass :: make => make20
    end implements
contains
    integer function make20(self, n)
        class(T20), intent(in) :: self
        integer, intent(in) :: n
        make20 = self%n + n
    end function
end module cc_traits_receiver_bad_r20

! R21: the passed object of an intent(in) receiver is intent(in), not
! intent(inout).
module cc_traits_receiver_bad_r21
    use cc_traits_receiver_contracts, only: IRead
    implicit none
    type :: T21
        integer :: n = 0
    end type
    implements IRead :: T21
        procedure, pass :: get => get21
    end implements
contains
    integer function get21(self)
        class(T21), intent(inout) :: self
        get21 = self%n
    end function
end module cc_traits_receiver_bad_r21

! R22: an intent(inout) receiver is not implemented by intent(in).
module cc_traits_receiver_bad_r22
    use cc_traits_receiver_contracts, only: IWrite
    implicit none
    type :: T22
        integer :: n = 0
    end type
    implements IWrite :: T22
        procedure, pass(self) :: put => put22
    end implements
contains
    subroutine put22(self, v)
        class(T22), intent(in) :: self
        integer, intent(in) :: v
        print *, self%n + v
    end subroutine
end module cc_traits_receiver_bad_r22

! R23: an intent(out) receiver is not implemented by intent(inout).
module cc_traits_receiver_bad_r23
    use cc_traits_receiver_contracts, only: IClear
    implicit none
    type :: T23
        integer :: n = 0
    end type
    implements IClear :: T23
        procedure, pass :: clear => clear23
    end implements
contains
    subroutine clear23(self)
        class(T23), intent(inout) :: self
        self%n = 0
    end subroutine
end module cc_traits_receiver_bad_r23

! R24: a receiver without INTENT is implemented by a passed object without
! INTENT, not by intent(in).
module cc_traits_receiver_bad_r24
    use cc_traits_receiver_contracts, only: ITouch
    implicit none
    type :: T24
        integer :: n = 0
    end type
    implements ITouch :: T24
        procedure, pass :: touch => touch24
    end implements
contains
    subroutine touch24(self)
        class(T24), intent(in) :: self
        print *, self%n
    end subroutine
end module cc_traits_receiver_bad_r24

! R25: an intent(in) receiver is not implemented by a passed object without
! INTENT.
module cc_traits_receiver_bad_r25
    use cc_traits_receiver_contracts, only: IRead
    implicit none
    type :: T25
        integer :: n = 0
    end type
    implements IRead :: T25
        procedure, pass :: get => get25
    end implements
contains
    integer function get25(self)
        class(T25) :: self
        get25 = self%n
    end function
end module cc_traits_receiver_bad_r25

! R26: same-named messages of combined traits agree on having a receiver.
module cc_traits_receiver_bad_r26
    use cc_traits_receiver_contracts, only: ILeft, IRightFree
    implicit none
    type :: T26
        integer :: n = 0
    end type
    implements (ILeft + IRightFree) :: T26
        procedure, pass :: bump => bump26
    end implements
contains
    subroutine bump26(self, by)
        class(T26), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
end module cc_traits_receiver_bad_r26

! R27: and on its INTENT; its name and position may differ.
module cc_traits_receiver_bad_r27
    use cc_traits_receiver_contracts, only: ILeft, IRightIn
    implicit none
    type :: T27
        integer :: n = 0
    end type
    implements (ILeft + IRightIn) :: T27
        procedure, pass :: bump => bump27
    end implements
contains
    subroutine bump27(self, by)
        class(T27), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
end module cc_traits_receiver_bad_r27

! R28: the PASS attribute marks the receiver once.
module cc_traits_receiver_bad_r28
    implicit none
    abstract interface :: IR28
        integer function f(self)
            type(itself), intent(in), pass, pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r28

! R29: a DIMENSION statement after the declaration of the receiver makes it
! an array.
module cc_traits_receiver_bad_r29
    implicit none
    abstract interface :: IR29
        integer function f(self)
            type(itself), intent(in), pass :: self
            dimension :: self(2)
        end function
    end interface
end module cc_traits_receiver_bad_r29

! R30: and so does one before it.
module cc_traits_receiver_bad_r30
    implicit none
    abstract interface :: IR30
        integer function f(item)
            dimension :: item(2)
            type(itself), intent(in), pass :: item
        end function
    end interface
end module cc_traits_receiver_bad_r30

! R31: a VOLATILE statement is not implemented yet.
module cc_traits_receiver_bad_r31
    implicit none
    abstract interface :: IR31
        subroutine s(self)
            type(itself), intent(inout), pass :: self
            volatile :: self
        end subroutine
    end interface
end module cc_traits_receiver_bad_r31

! R32: nor is an ASYNCHRONOUS one.
module cc_traits_receiver_bad_r32
    implicit none
    abstract interface :: IR32
        subroutine s(self)
            type(itself), intent(inout), pass :: self
            asynchronous :: self
        end subroutine
    end interface
end module cc_traits_receiver_bad_r32

! R33: only an array is CONTIGUOUS.
module cc_traits_receiver_bad_r33
    implicit none
    abstract interface :: IR33
        integer function f(self)
            type(itself), intent(in), pass :: self
            contiguous :: self
        end function
    end interface
end module cc_traits_receiver_bad_r33

! R34: the INTENT of the receiver is given once.
module cc_traits_receiver_bad_r34
    implicit none
    abstract interface :: IR34
        integer function f(self)
            intent(in) :: self
            type(itself), intent(in), pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r34

! Valid: the receiver may have any name and position, the implementation
! binds its passed object at its own position, and each receiver INTENT is
! implemented by the same INTENT.
module cc_traits_receiver_valid
    use cc_traits_receiver_contracts, only: IRead, IWrite, IClear, ITouch, IMake
    implicit none
    type :: Cell
        integer :: n = 0
    end type
    implements (IRead + IWrite + IClear + ITouch + IMake) :: Cell
        procedure, pass :: get => cell_get
        procedure, pass(c) :: put => cell_put
        procedure, pass :: clear => cell_clear
        procedure, pass :: touch => cell_touch
        procedure, nopass :: make => cell_make
    end implements
contains
    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%n
    end function
    subroutine cell_put(v, c)
        integer, intent(in) :: v
        class(Cell), intent(inout) :: c
        c%n = v
    end subroutine
    subroutine cell_clear(self)
        class(Cell), intent(out) :: self
        self%n = -1
    end subroutine
    subroutine cell_touch(self)
        class(Cell) :: self
        self%n = self%n + 1
    end subroutine
    integer function cell_make(n)
        integer, intent(in) :: n
        cell_make = n
    end function
end module cc_traits_receiver_valid

! Valid: the INTENT of a receiver can be given by an INTENT statement before
! or after its declaration, and its implementation has exactly that intent.
module cc_traits_receiver_valid_split
    implicit none
    abstract interface :: ISplit
        integer function get(self)
            intent(in) :: self
            type(itself), pass :: self
        end function
        subroutine put(v, self)
            integer, intent(in) :: v
            type(itself), pass :: self
            intent(inout) :: self
        end subroutine
    end interface
    type :: Slot
        integer :: n = 0
    end type
    implements ISplit :: Slot
        procedure, pass :: get => slot_get
        procedure, pass(s) :: put => slot_put
    end implements
contains
    integer function slot_get(self)
        class(Slot), intent(in) :: self
        slot_get = self%n
    end function
    subroutine slot_put(v, s)
        integer, intent(in) :: v
        class(Slot), intent(inout) :: s
        s%n = v
    end subroutine
end module cc_traits_receiver_valid_split
