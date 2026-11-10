! A namelist group object in an internal procedure that is not declared, with
! implicit typing disabled (#12375).
subroutine host
  contains
    subroutine inner
      namelist /g3/ hc  ! {Error} symbol 'hc' in namelist 'g3' must be declared before the namelist statement (or enable implicit typing with --implicit-typing)
    end subroutine
end subroutine
