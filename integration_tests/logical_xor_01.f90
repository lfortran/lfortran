program logical_xor_01
! .xor. of constant operands is folded at compile time
implicit none
logical, parameter :: t = .true. .xor. .false.
logical, parameter :: f = .true. .xor. .true.
logical, parameter :: a(4) = [.true., .true., .false., .false.] &
    .xor. [.true., .false., .true., .false.]
logical, parameter :: b(2) = [.true., .false.] .xor. .true.
logical :: l
if (.not. t) error stop
if (f) error stop
if (any(a .neqv. [.false., .true., .true., .false.])) error stop
if (any(b .neqv. [.false., .true.])) error stop
l = .false. .xor. .false.
if (l) error stop
l = .false. .xor. .true.
if (.not. l) error stop
print *, t, f, a, b
end program
