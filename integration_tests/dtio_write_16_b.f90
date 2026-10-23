! An external procedure of dtio_write_16 with defined output of u (binding wf_s).
subroutine dtio16(str)
use dtio_write_16_mod, only: u
implicit none
character(*), intent(out) :: str
type(u) :: b
write(str, '(dt)') b
end subroutine
