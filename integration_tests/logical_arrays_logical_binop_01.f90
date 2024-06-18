program main
   logical, parameter :: x1(2) = [.true., .false.]
   logical :: x2(2, 2)
   logical :: x3(2, 2)

   print *, x1 .neqv. .true.
   if (all((x1 .neqv. .true.) .neqv. [.false., .true.])) error stop

   x2(1, 1) = .true.
   x2(1, 2) = .false.
   x2(2, 1) = .false.
   x2(2, 2) = .true.

   print *, x2 .neqv. .true.
   x3 = reshape([.false., .true., .true., .false.], [2, 2])
   if (all((x2 .neqv. .true.) .neqv. x3)) error stop

   print *, [.false., .true.] .eqv. [.false., .true.]
   if (all(([.false., .true.] .eqv. [.false., .true.]) .neqv. [.true., .true.])) error stop

end program main
