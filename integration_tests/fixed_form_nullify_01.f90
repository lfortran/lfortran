      program fixed_form_nullify_01
      implicit none
      real, dimension(:), pointer :: f, g, h
      real, target :: x(3), y(3), z(3)
      f => x
      g => y
      h => z
      nullify(f)
      if (associated(f)) error stop 1
      NULLIFY(G)
      if (associated(g)) error stop 2
      nullify (h)
      if (associated(h)) error stop 3
      call chk
      print *, 'ok'
      contains
      subroutine chk
      real, pointer :: p
      real, target :: t
      p => t
      Nullify(p)
      if (associated(p)) error stop 4
      end subroutine chk
      end program
