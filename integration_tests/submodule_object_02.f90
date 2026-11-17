! The object file of a submodule defines its procedures, which may use what
! the program's object file does not define: a module only the submodule
! uses, or a procedure of the parent module that the program does not call.
! The program's object file may define the submodule's variables too, once
! the submodule has been read. Each build order has to link and share one
! copy of every variable.
program submodule_object_02
    implicit none
    character(len=1024) :: compiler
    integer :: length, status
    call get_environment_variable("LFORTRAN", compiler, length, status)
    if (status /= 0 .or. length <= 0) compiler = "lfortran"
    call write_sources()
    call compile(compiler, "u")
    call compile(compiler, "m")
    ! The program is compiled before the submodules have been read.
    call run(compiler, "-c submodule_object_02_p.f90 -o submodule_object_02_p_first.o", 1)
    call compile(compiler, "s")
    call compile(compiler, "t")
    call link_and_run(compiler, "submodule_object_02_p_first.o", "submodule_object_02_bin1", 2)
    ! Now the program reads the submodules and compiles them too.
    call run(compiler, "-c submodule_object_02_p.f90 -o submodule_object_02_p_last.o", 4)
    call link_and_run(compiler, "submodule_object_02_p_last.o", "submodule_object_02_bin3", 5)
    call run(compiler, "submodule_object_02_u.f90 submodule_object_02_m.f90 " // &
        "submodule_object_02_s.f90 submodule_object_02_t.f90 " // &
        "submodule_object_02_p.f90 -o submodule_object_02_bin4", 6)
    call run_binary("submodule_object_02_bin4", 7)
contains
    subroutine write_sources()
        open(unit=10, file="submodule_object_02_u.f90", status="replace")
        write(10, '(a)') "module submodule_object_02_u"
        write(10, '(a)') "integer :: offset = 1"
        write(10, '(a)') "contains"
        write(10, '(a)') "integer function bump(n)"
        write(10, '(a)') "integer, intent(in) :: n"
        write(10, '(a)') "bump = n + offset"
        write(10, '(a)') "end function bump"
        write(10, '(a)') "end module submodule_object_02_u"
        close(10)
        open(unit=10, file="submodule_object_02_m.f90", status="replace")
        write(10, '(a)') "module submodule_object_02_m"
        write(10, '(a)') "character(len=5) :: names(2) = [""alpha"", ""beta ""]"
        write(10, '(a)') "character(len=5) :: label = ""start"""
        write(10, '(a)') "integer :: total = 100"
        write(10, '(a)') "interface"
        write(10, '(a)') "module subroutine hello(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "module subroutine bye(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "end subroutine bye"
        write(10, '(a)') "end interface"
        write(10, '(a)') "contains"
        write(10, '(a)') "integer function helper()"
        write(10, '(a)') "helper = 3"
        write(10, '(a)') "end function helper"
        write(10, '(a)') "end module submodule_object_02_m"
        close(10)
        open(unit=10, file="submodule_object_02_s.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_02_m) submodule_object_02_s"
        write(10, '(a)') "use submodule_object_02_u, only: bump"
        write(10, '(a)') "integer :: counter = 7"
        write(10, '(a)') "contains"
        write(10, '(a)') "module subroutine hello(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "counter = counter + 1"
        write(10, '(a)') "names(2) = ""gamma"""
        write(10, '(a)') "label = ""done """
        write(10, '(a)') "total = total + 1"
        write(10, '(a)') "k = bump(counter) + helper()"
        write(10, '(a)') "end subroutine hello"
        write(10, '(a)') "end submodule submodule_object_02_s"
        close(10)
        open(unit=10, file="submodule_object_02_t.f90", status="replace")
        write(10, '(a)') "submodule (submodule_object_02_m:submodule_object_02_s) submodule_object_02_t"
        write(10, '(a)') "contains"
        write(10, '(a)') "module subroutine bye(k)"
        write(10, '(a)') "integer, intent(out) :: k"
        write(10, '(a)') "k = counter"
        write(10, '(a)') "end subroutine bye"
        write(10, '(a)') "end submodule submodule_object_02_t"
        close(10)
        open(unit=10, file="submodule_object_02_p.f90", status="replace")
        write(10, '(a)') "program p"
        write(10, '(a)') "use submodule_object_02_m, only: hello, bye, names, label, total"
        write(10, '(a)') "integer :: k"
        write(10, '(a)') "call hello(k)"
        write(10, '(a)') "if (k /= 12) error stop 1"
        write(10, '(a)') "call hello(k)"
        write(10, '(a)') "if (k /= 13) error stop 2"
        write(10, '(a)') "call bye(k)"
        write(10, '(a)') "if (k /= 9) error stop 3"
        write(10, '(a)') "if (names(1) /= ""alpha"" .or. names(2) /= ""gamma"") error stop 4"
        write(10, '(a)') "if (label /= ""done "") error stop 5"
        write(10, '(a)') "if (total /= 102) error stop 6"
        write(10, '(a)') "end program p"
        close(10)
    end subroutine write_sources
    subroutine compile(compiler, name)
        character(len=*), intent(in) :: compiler, name
        call run(compiler, "-c submodule_object_02_" // name // ".f90 -o submodule_object_02_" &
            // name // ".o", 10)
    end subroutine compile
    subroutine link_and_run(compiler, main_object, binary, code)
        character(len=*), intent(in) :: compiler, main_object, binary
        integer, intent(in) :: code
        call run(compiler, main_object // " submodule_object_02_u.o submodule_object_02_m.o " // &
            "submodule_object_02_s.o submodule_object_02_t.o -o " // binary, code)
        call run_binary(binary, code + 20)
    end subroutine link_and_run
    subroutine run(compiler, args, code)
        character(len=*), intent(in) :: compiler, args
        integer, intent(in) :: code
        character(len=4096) :: cmd
        integer :: exitstat
        cmd = '"' // trim(compiler) // '" ' // args
        call execute_command_line(trim(cmd), exitstat=exitstat)
        if (exitstat /= 0) then
            print *, "failed: ", trim(cmd)
            error stop code
        end if
    end subroutine run
    subroutine run_binary(binary, code)
        character(len=*), intent(in) :: binary
        integer, intent(in) :: code
        integer :: exitstat
        call execute_command_line("./" // binary, exitstat=exitstat)
        if (exitstat /= 0) then
            print *, "failed: ", binary
            error stop code
        end if
    end subroutine run_binary
end program submodule_object_02
