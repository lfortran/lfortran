block data bd_l
  implicit none

  logical :: lb
  common /blockl/ lb(2)
  logical :: la(2)
  equivalence (la, lb)
  data la(2) / .false. /

end

program equivalence_27
  implicit none
  
  logical :: la(2)
  common /blockl/ la

  if (la(2)) error stop

end program