program separate_compilation_08
    use module_using_all_1_separate_compilation_08a
    use module_using_all_2_separate_compilation_08b
    real :: x(5)
    x = 9.2314

    call test_all(x)

    x = 19.2314

    call test_all_2(x)
end program
