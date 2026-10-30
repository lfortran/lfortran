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

Procedure variables, casts and null constants must use the lowered interface
as well. A null constant's type is refreshed from its procedure-variable
expression after lowering: module-file serialization can give the constant a
separate copy of the original signature, so changing the variable alone is not
sufficient. Import that signature into the null constant's current scope:
specification expressions in the procedure variable's type can reference
symbols visible only in its declaration scope (for example a module function
used as an array bound). Reusing the type directly would leave those references
out of scope in a derived-type constructor.

Bare implicit-interface declarations have no ABI to lower. Keep the result
type recorded on their procedure variables rather than replacing it with the
declaration's potentially untyped signature, including in `ENTRY` wrappers.
Semantic analysis refreshes each wrapper's signature from its own dummy
declarations and applies the usual late-procedure-actual casts before lowering.

### Result assignment destinations

The assignment target can be used as the result argument only when the
function cannot otherwise access its storage before completing the result
(F2018 10.2.1.3). This includes access through callbacks: a procedure-pointer
actual, a procedure component or a dummy procedure may designate an internal
procedure that reads the target by host association. Such unknown procedure
actuals retain a result temporary, including when wrapped in a procedure
cast or carried by a derived-type actual or passed object. Component types
are followed recursively, including inherited components and recursive data
pointers; an unknown dynamic extension can also carry a callback.

A known implementation is not alone proof of noncapture: it may call an
externally stored procedure pointer. Follow its calls, propagating known
procedure actuals to dummy procedures, before reusing a local destination.
Known noncapturing callbacks and data types without callback components
remain eligible. The destination itself is checked along its entire
designator: a nonpointer final component reached through an intervening
pointer can still alias storage accessible to the callee.

### Runtime character result metadata

Call preparation in `array_struct_temporary` saves result specification values
before evaluating the call. A generated character temporary whose length is
such a runtime value must have deferred length: its declaration is processed on
procedure entry, before those specification assignments, and cannot initialize
its descriptor from them. Both temporary creation and return-slot creation use
the same string-type normalization.

For a nonallocatable result, the caller's allocation carries the prepared
length along with any extents. For elemental calls, array actuals supply the
iteration shape, not the result's element type, kind or character length.
Allocation and assignment reallocation preserve the prepared result length
even when it differs from every actual's length or a scalar actual is
broadcast.

Earlier elementwise lowering must scalarize both an elemental call's result
and its array actuals inside the element loop. Leaving an array-valued call
in a scalar assignment would instead materialize an unnecessary whole-array
result, with no valid per-element storage contract.

An allocatable result owns its allocation in the
callee, but may still have an explicit, nondeferred character length. Before
passing a deferred-length return slot to its converted `intent(out)` result
dummy, the caller establishes that explicit length with an allocation (empty
in every dimension for an array whose returned shape is not yet known).
The dummy's normal entry deallocation releases this allocation before the
function body executes; its descriptor retains the specified element length.
Thus the body sees an unallocated result with the correct length, including
zero-sized results and calls with different lengths in a loop. No speculative
evaluation of the function is needed to discover its shape or length.
