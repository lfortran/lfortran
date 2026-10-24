! Data components of named constants whose value holds a procedure pointer
! component associated with a procedure of another module (#14794).
program separate_compilation_65
use separate_compilation_65a, only: os, o, ts
implicit none
if (os(2)%n /= 2) error stop
if (os(1)%k(2) /= 20) error stop
if (o%inner%n /= 4) error stop
if (o%inner%k(1) /= 50) error stop
if (o%m /= 8) error stop
if (ts(2)%inner%n /= 13) error stop
if (ts(1)%inner%k(2) /= 2) error stop
if (ts(2)%m /= 14) error stop
call plain_use()
print *, "ok"
contains
    subroutine plain_use()
    use separate_compilation_65a
    if (os(1)%n /= 1) error stop
    if (o%inner%n /= 4) error stop
    if (ts(1)%inner%n /= 11) error stop
    end subroutine
end program
