subroutine idz_realcomp(n,a)
   integer n
   real*8 a(n)
   a = 12.5d0
   return
end

subroutine idzp_svd(ls, w)
   use iso_c_binding, only: c_f_pointer, c_loc
   real*8 eps
   complex*16, target :: w(*)
   integer :: ls, isi, i
   real(8), pointer :: w_cast(:)
   isi = 1
   call c_f_pointer(c_loc(w(isi)), w_cast, [ls])
   call idz_realcomp(ls,w_cast)
   do i = 1, ls
      if (abs(w_cast(i) - 12.5d0) > 1e-12) error stop
   end do
   print *, w_cast
   return
end

program main
   complex*16, target :: w(5)
   interface
      subroutine idzp_svd(ls, w)
         use iso_c_binding, only: c_f_pointer, c_loc
         complex*16, target :: w(*)
         integer :: ls
      end subroutine
   end interface
   call idzp_svd(5, w)
end program

