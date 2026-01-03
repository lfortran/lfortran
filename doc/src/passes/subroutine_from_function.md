### Class `AllocateVarBasedOnFuncCall`

======== What Is It Doing? ========
    It allocates a Var based on the return type of a FunctionCall.
========  Why This Class?  ========
    While we're using the return type of the functionCall we might face FunctionParam nodes OR
    We might face already replaced FunctionParam nodes.

    The problem : That FuncParam node could be a functionCall node. If we allocate using the return type
    rightaway without creating temporary to hold the functionCall result, We'll end up double calling the function.
    one for the allocate statement and one for the passed argument.

    Another Problem : Having a functionCall (argument) inplace of a functionParam but the functionCall got replaced into
    a temporary variable, hence we got Revaluate and replace that old functionCall with the temporary -- otherwise we'll double
    call the function also.

    How to fix that ??
        Use the Function's return type (the one that has FunctionParam nodes) and start traversing through the type node
        attempting to replace functionCalls with temporaries inplace of the FunctinonParam and also the FunctionCall argument
    Note :
        We're attempting to create temporaries for functionCalls -- We rely on the fact that we're applying this whole 
        pass logic in depth first manner -- making us creating temporaries for simple functionCalls. NO AGGREGATE RETURNS

========   Example   ========
```.f90
    function foo(x) result (r)
        integer :: x
        character(len=x) :: r 
    end function
    print *, foo(f()) ! ASSUME `f()` returns an integer
```

WITHOUT THIS CLASS :
```.f90
    allocate(character(len=f()) :: return_slot) ! DOUBLE EVALUATION OF `f()`
    call foo(f(), return_slot)
    print *, return_slot
```

WITH THIS CLASS :
```.f90
    temp_var = f()
    allocate(character(len=funcCall_temp_var) :: return_slot)
    call foo(temp_var, return_slot)
    print *, return_slot
```
========   How It Works?   ========
    1. We use entry static function `Allocate()` to prevent using other functions by mistake.
    2. Pass to it Var to allocate + FunctionCall we're allocating against its return type + Other helper members.
    3. We get function return type (the one with FunctionParams).
    4. We call the replacer on the type.
    5. The replacer replaces FunctionParam nodes with arguments in the functionCall.
    6. FunctionCalls arguments are replaced with temporaries in both FunctionParam site and also the functionCall's argument site.
    7. Any other argument expression is just used normally.
    8. Then we insert an allocate statement (the main purpose of this class) 

---



### Class `CreateFunctionFromSubroutine` 


================= What Is It Doing? =================
    This pass transforms functions that return aggregate types (like arrays, structs, strings, etc.) into subroutines.

=================   How It works?   =================
    1. It visits only Function nodes in the ASR tree.
    2. We use `handle_fn_return_var()` to identify whether this function needs transformation or not.
    3. `is_aggregate_or_array_or_nonPrimitive_type()` have the roles on which we decide.
    4. If transformation is needed, We transform function's member `return_var` into intentOUT argument and we nullify the member.
    5. We also store the original return type in a map for later use.


