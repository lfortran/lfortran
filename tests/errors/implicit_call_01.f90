subroutine callsub (xub, i)
    ! xub is a variable (implicitly typed as real),
    ! trying to call it should fail without --implicit-interface
    call xub (i)
end subroutine
