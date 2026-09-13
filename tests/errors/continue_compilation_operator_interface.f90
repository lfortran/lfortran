! An unresolved module procedure inside an interface operator(...),
! defined operator(.op.), or assignment(=) must be reported exactly
! once, at the location of the offending `module procedure` name -
! not once per program unit in the file, and not at the start of the
! file. This is a regression test for
! https://github.com/lfortran/lfortran/issues/12640
!
! Two unrelated modules (extra1, extra2) follow the erroneous module
! on purpose: before the fix, each additional program unit in the
! file produced one extra copy of every diagnostic below.
module cc_op_iface_bad_intrinsic_op
    implicit none
    interface operator(.bad.)
        module procedure cc_op_iface_bad_op
    end interface
end module

module cc_op_iface_bad_plus
    implicit none
    interface operator(+)
        module procedure cc_op_iface_bad_plus_op
    end interface
end module

module cc_op_iface_bad_assign
    implicit none
    interface assignment(=)
        module procedure cc_op_iface_bad_assign_sub
    end interface
end module

module cc_op_iface_extra1
    implicit none
end module

module cc_op_iface_extra2
    implicit none
end module
