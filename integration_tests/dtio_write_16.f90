! Defined output in external procedures of different files: the callbacks of
! s_dtio16 (binding wf) and dtio16 (binding wf_s) must not clash when the
! files are compiled in one invocation.
program dtio_write_16
implicit none
interface
    subroutine s_dtio16(str)
    character(*), intent(out) :: str
    end subroutine
    subroutine dtio16(str)
    character(*), intent(out) :: str
    end subroutine
end interface
character(len=8) :: a, b

call s_dtio16(a)
if (a /= "T1") error stop 1
call dtio16(b)
if (b /= "U2") error stop 2
print *, "ok"
end program
