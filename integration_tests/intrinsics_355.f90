module timerm
    implicit none
    private
    public  print_time
    contains
      subroutine print_time(rate)
        integer rate
        call system_clock(count_rate=rate)
      end subroutine print_time
end module timerm
program intrinsic_355
use timerm
integer :: rate
call print_time(rate)
print *, rate
if (rate /= 1000) error stop
end
  