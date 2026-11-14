! A submodule is compiled on its own. Its procedures have to be in that
! object file: the program that uses the parent module only has the
! interface.
program submodule_object_01
    implicit none
    character(len=1024) :: compiler
    integer :: length, status, exitstat
    call get_environment_variable("LFORTRAN", compiler, length, status)
    if (status /= 0 .or. length <= 0) compiler = "lfortran"
    call write_sources()
    ! The program is compiled from the parent interface, before the
    ! submodule is compiled, which is how a build schedules the two files.
    ! The submodule object then has to provide hello.
    call run(compiler, "-c submodule_object_01_m.f90 -o submodule_object_01_m.o", exitstat)
    if (exitstat /= 0) error stop 1
    call run(compiler, "-c submodule_object_01_p.f90 -o submodule_object_01_p.o", exitstat)
    if (exitstat /= 0) error stop 2
    call run(compiler, "-c submodule_object_01_impl.f90 -o submodule_object_01_impl.o", exitstat)
    if (exitstat /= 0) error stop 3
    call run(compiler, "submodule_object_01_p.o submodule_object_01_m.o " // &
        "submodule_object_01_impl.o -o submodule_object_01_bin", exitstat)
    if (exitstat /= 0) error stop 4
    call execute_command_line("./submodule_object_01_bin > submodule_object_01_out", exitstat=exitstat)
    if (exitstat /= 0) error stop 5
    open(unit=11, file="submodule_object_01_out", status="old")
    read(11, *) exitstat
    close(11)
    if (exitstat /= 7) error stop 6
contains
    subroutine write_sources()
        open(unit=10, file="submodule_object_01_m.f90", status="replace")
        write(10, '(a)') "module submodule_object_01_m"
        write(10, '(a)') "interface"
        write(10, '(a)') "module subroutine hello"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "end interface"
        write(10, '(a)') "end module submodule_object_01_m"
        close(10)
        open(unit=10, file="submodule_object_01_impl.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_01_m) submodule_object_01_impl"
        write(10, '(a)') "contains"
        write(10, '(a)') "module subroutine hello"
        write(10, '(a)') "print *, 7"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "end submodule submodule_object_01_impl"
        close(10)
        open(unit=10, file="submodule_object_01_p.f90", status="replace")
        write(10, '(a)') "program p"
        write(10, '(a)') "use submodule_object_01_m, only: hello"
        write(10, '(a)') "call hello"
        write(10, '(a)') "end program p"
        close(10)
    end subroutine write_sources
    subroutine run(compiler, args, exitstat)
        character(len=*), intent(in) :: compiler, args
        integer, intent(out) :: exitstat
        character(len=4096) :: cmd
        cmd = '"' // trim(compiler) // '" ' // trim(args)
        call execute_command_line(trim(cmd), exitstat=exitstat)
    end subroutine run
end program submodule_object_01
