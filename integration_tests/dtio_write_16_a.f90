! An external procedure of dtio_write_16 with defined output of t (binding wf).
subroutine s_dtio16(str)
use dtio_write_16_mod, only: t
implicit none
character(*), intent(out) :: str
type(t) :: a
write(str, '(dt)') a
end subroutine
