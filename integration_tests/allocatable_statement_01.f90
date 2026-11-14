! An ALLOCATABLE statement applies to a deferred-shape declaration that
! appears before it, not only to one that appears after it.
program allocatable_statement_01
implicit none
call s
contains
subroutine s
    integer :: a(:)
    allocatable a
    allocate(a(2))
    a = 1
    if (a(1) /= 1 .or. a(2) /= 1) error stop
end subroutine
end program
