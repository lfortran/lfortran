program string_array_concat_01
  implicit none
  character(len=2) :: c(2)

  c(1) = "x"
  c(2) = "y"

  ! c(1) = "x " => trim(c(1)) = "x"
  ! [trim(c(1)), 'a'] => ["x", "a"]
  ! 'A' // ["x", "a"] => ["Ax", "Aa"]
  ! ["Ax", "Aa"] // 'c' => ["Axc", "Aac"]
  ! Truncated to len=2 => "Ax", "Aa"
  c = ['A' // [trim(c(1)), 'a']] // 'c'

  if (c(1) /= "Ax") error stop
  if (c(2) /= "Aa") error stop
end program string_array_concat_01
