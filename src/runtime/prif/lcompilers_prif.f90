! The start of the coarray runtime, between LFortran's startup and a PRIF
! implementation (Caffeine, for instance).
!
! Every translation unit that calls PRIF has a collective bootstrap, which
! the startup engine runs at the collective boundary -- a Fortran main
! program, or a host's lcompilers_initialize() -- before any coarray is
! allocated. It calls lcompilers_prif_start and stops the program unless the
! status is 0. prif_init itself gives 0 for success, the implementation's own
! PRIF_STAT_ALREADY_INIT when the runtime was started before -- by another
! bootstrap, or by the host -- and any other value for a failure (PRIF 5.2).
! Only the implementation's module knows the value of PRIF_STAT_ALREADY_INIT,
! so this file is compiled with it -- by a compiler that reads that module,
! with the implementation's module ABI -- and linked into every program that
! uses coarrays. With LFortran, separate compilation keeps the object from
! defining anything of the implementation's module again:
!
!     lfortran -c --separate-compilation \
!         -I<directory of the implementation's prif.mod> \
!         lcompilers_prif.f90 -o lcompilers_prif.o
!     lfortran <objects> lcompilers_prif.o -L<...> -lcaffeine ...
subroutine lcompilers_prif_start(stat) bind(c, name="lcompilers_prif_start")
    use iso_c_binding, only: c_int
    use prif, only: prif_init, prif_stat_already_init
    implicit none
    integer(c_int), intent(out) :: stat
    call prif_init(stat)
    if (stat == prif_stat_already_init) stat = 0
end subroutine lcompilers_prif_start
