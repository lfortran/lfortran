program intrinsics_502
    ! pack/count with a negated complex comparison of array sections (#15926)
    use iso_fortran_env, only: real64
    implicit none
    complex(real64) :: x(5)
    complex :: s(4)
    complex(real64) :: p1(1), p3(3)
    complex :: q(2)
    logical :: l(4)

    x = [(1.0_real64, 0.0_real64), (2.0_real64, 1.0_real64), &
         (1.0_real64, 0.0_real64), (3.0_real64, 0.0_real64), &
         (2.0_real64, 1.0_real64)]

    print *, pack(x(2:), .not. x(2:) == x(1))
    if (size(pack(x(2:), .not. x(2:) == x(1))) /= 3) error stop
    p3 = pack(x(2:), .not. x(2:) == x(1))
    if (p3(1) /= (2.0_real64, 1.0_real64)) error stop
    if (p3(2) /= (3.0_real64, 0.0_real64)) error stop
    if (p3(3) /= (2.0_real64, 1.0_real64)) error stop

    p1 = pack(x(2:), .not. x(2:) /= x(1))
    if (p1(1) /= (1.0_real64, 0.0_real64)) error stop

    if (count(.not. x(2:) == x(1)) /= 3) error stop
    if (count(.not. x(1:4) == x(2:5)) /= 4) error stop
    if (count(.not. x(1::2) == x(2:4)) /= 2) error stop
    if (count(.not. (.not. x(2:) == x(1))) /= 1) error stop
    if (.not. any(.not. x(::2) == x(1))) error stop
    if (all(.not. x(2:3) == x(5))) error stop

    l = .not. x(2:) == x(1)
    if (l(1) .neqv. .true.) error stop
    if (l(2) .neqv. .false.) error stop
    if (l(3) .neqv. .true.) error stop
    if (l(4) .neqv. .true.) error stop

    s = [(1.0, 0.0), (2.0, 0.0), (1.0, 0.0), (4.0, 4.0)]
    q = pack(s(2:), .not. s(2:) == s(1))
    if (q(1) /= (2.0, 0.0)) error stop
    if (q(2) /= (4.0, 4.0)) error stop
end program
