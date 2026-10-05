! A private procedure bound to a public type is not imported by use
! association, so the using module can define a procedure of the same name.
module modules_76_a
implicit none
type, abstract :: tok
contains
    procedure(next_token), deferred :: next_token
end type
abstract interface
    subroutine next_token(de)
        import :: tok
        class(tok), intent(inout) :: de
    end subroutine
end interface
type :: counter
    integer :: n = 0
contains
    procedure :: bump
end type
private :: next_token, bump
contains
subroutine bump(self)
    class(counter), intent(inout) :: self
    self%n = self%n + 1
end subroutine
end module
module modules_76_b
use modules_76_a
implicit none
type, extends(tok) :: ctok
    integer :: n = 0
contains
    procedure :: next_token
end type
contains
subroutine next_token(de)
    class(ctok), intent(inout) :: de
    de%n = de%n + 10
end subroutine
subroutine bump(k)
    integer, intent(inout) :: k
    k = k + 100
end subroutine
end module
program modules_76
use modules_76_b
implicit none
type(ctok) :: c
type(counter) :: cn
integer :: k
k = 0
call c%next_token()
call cn%bump()
call bump(k)
print *, c%n, cn%n, k
if (c%n /= 10 .or. cn%n /= 1 .or. k /= 100) error stop 1
end program
