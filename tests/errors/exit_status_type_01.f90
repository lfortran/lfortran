! The STATUS argument of the EXIT extension must be an integer; here it is
! implicitly typed real (#12319).
subroutine exit_pointer(status)
  call exit(status)  ! {Error} `status` argument of `exit` must be of integer type, but got real
end subroutine
