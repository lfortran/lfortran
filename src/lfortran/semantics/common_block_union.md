# COMMON Block Union-Based Implementation Design

## Current Approach (PR #9199: Blob + Bitcast)

```
COMMON /data/ arr(4)  ! real(8)[4] = 32 bytes

Module: file_common_block_data
├── Struct: data
│   └── arr: real(8)[4]
└── Variable: struct_instance_data (type: data)

Access: StructInstanceMember → bitcast if types differ
```

## Proposed Approach (Union of Structs)

```
COMMON /data/ arr(4)   ! main: real(8)[4]
COMMON /data/ iarr(8)  ! sub:  int[8]

Module: file_common_block_data
├── Union: data_union
│   ├── Struct: view_1  { arr: real(8)[4] }
│   └── Struct: view_2  { iarr: int[8] }
└── Variable: union_instance_data (type: data_union)

Access: UnionInstanceMember(view_N) → StructInstanceMember(member)
```

## Key Benefits

1. **Explicit type info at ASR level**: Each view is a named struct
2. **No implicit bitcasts**: UnionInstanceMember handles it
3. **Self-documenting**: ASR shows all views of the storage
4. **Matches certik's design**: Issue #661 comment

## Implementation Changes Needed

### 1. `create_common_module` (ast_common_visitor.h)

```cpp
// Instead of:
ASR::symbol_t* struct_symbol = make_Struct_t(...);

// Create:
ASR::symbol_t* union_symbol = make_Union_t(...);
// Then add struct variants to the union
```

### 2. Track layouts per COMMON block

```cpp
// Map: common_block_name -> list of (layout_hash, struct_symbol)
std::map<std::string, std::vector<std::pair<size_t, ASR::symbol_t*>>> common_block_layouts;
```

### 3. `create_StructInstanceMember` modification

```cpp
// When variable is in COMMON block:
// 1. Find which struct variant this variable belongs to
// 2. Create UnionInstanceMember to access that variant
// 3. Create StructInstanceMember on that variant
```

### 4. Codegen (already works)

`visit_UnionInstanceMember` already does bitcast - no changes needed.

## Layout Hash Calculation

```cpp
size_t compute_layout_hash(const std::vector<ASR::Variable_t*>& vars) {
    size_t hash = 0;
    for (auto* var : vars) {
        hash = hash * 31 + get_type_hash(var->m_type);
    }
    return hash;
}
```

## Example ASR Output

Before (current):
```
(Struct data
  (members [arr])
  (symtab {arr: Variable real(8)[4]}))
```

After (union approach):
```
(Union data_union
  (members [view_1, view_2])
  (symtab {
    view_1: Struct (members [arr]) (symtab {arr: Variable real(8)[4]}),
    view_2: Struct (members [iarr]) (symtab {iarr: Variable int[8]})
  }))
```
