program datatest
  implicit none
  integer :: ivon01,ivon02,ivon03,ivon04,ivon05
  integer :: ivon06,ivon07,ivon08,ivon09,ivon10
  integer :: ivon11,ivon12,ivon13,ivon14,ivon15
  integer :: ivon16,ivon17,ivon18,ivon19,ivon20
  integer, parameter :: vals(5) = [2,76,587,9999,21111]
  complex :: axva,axvb
  
  data ivon01,ivon02,ivon03,ivon04,ivon05 /vals(1), vals(2), vals(3), vals(4), vals(5)/
  data ivon06,ivon07,ivon08,ivon09,ivon10 /+3,+76,+587,+9999,+21111/
  
  data ivon11,ivon12,ivon13,ivon14,ivon15 /-3,-76,-587,-9999,-21111/ 
  data ivon16,ivon17,ivon18,ivon19,ivon20 / 2*119, 2*7, -427/
  data axva,axvb /(-234.23, 3), (+2, -3.0)/
end program datatest
