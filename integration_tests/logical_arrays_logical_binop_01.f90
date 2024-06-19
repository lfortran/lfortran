program main
   implicit none
   logical, parameter :: x1(2) = [.true., .false.]

   ! parameter variable != logical constant
   print *, x1 .neqv. .true.
   if (all((x1 .neqv. .true.) .neqv. [.false., .true.])) error stop

   ! logical array constant == logical array constant
   print *, [.false., .true.] .eqv. [.false., .true.]
   if (all(([.false., .true.] .eqv. [.false., .true.]) .neqv. [.true., .true.])) error stop

   ! parameter variable != function returning logical constant
   print *, x1 .neqv. get_true()
   if (all((x1 .neqv. get_true()) .neqv. [.false., .true.])) error stop

   ! logical constant == parameter variable
   print *, .true. .eqv. x1
   if (all((.true. .eqv. x1) .neqv. [.true., .false.])) error stop

   ! parameter variable and logical constant
   print *, x1 .and. .false.
   if (all((x1 .and. .false.) .neqv. [.false., .false.])) error stop

   ! logical constant or parameter variable
   print *, .false. .or. x1
   if (all((.false. .or. x1) .neqv. [.true., .false.])) error stop

   ! logical expression or logical constant
   print *, 1 > 2 .or. x1
   if (all((1 > 2 .or. x1) .neqv. [.true., .false.])) error stop

   ! logical expression or logical constant
   print *, ((1 + 2) > 2) .or. x1
   if (all((((1 + 2) > 2) .or. x1) .neqv. [.true., .true.])) error stop

   ! logical expression or logical constant
   print *, ((1 + 2) == 3) .or. x1
   if (all((((1 + 2) == 3) .or. x1) .neqv. [.true., .true.])) error stop

   ! logical expression or logical constant
   print *, ((1 + 2) == 3) .or. x1
   if (all((((1 + 2) == 3) .or. x1) .neqv. [.true., .true.])) error stop

contains

   logical function get_true() result(value)
      value = .true.
   end function get_true

end program main
