program string_array_concat_02
  implicit none
  character(6) :: word(2)

  ! Concatenating two typed character array constructors element-wise.
  ! Previously this ICE'd (segfault) in compile-time evaluation of the
  ! StringConcat intrinsic, which only handled one array operand and assumed
  ! the other was a scalar.
  word = [character(6) :: [character(2) :: "ab", "cd"] // [character(3) :: "efg", "hij"]]

  ! element 1: "ab"(2) // "efg"(3) => "abefg" (padded to 6 => "abefg ")
  ! element 2: "cd"(2) // "hij"(3) => "cdhij" (padded to 6 => "cdhij ")
  if (word(1) /= "abefg ") error stop 1
  if (word(2) /= "cdhij ") error stop 2
end program string_array_concat_02
