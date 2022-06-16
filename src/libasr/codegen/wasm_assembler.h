#include <cassert>

#include <libasr/alloc.h>
#include <libasr/containers.h>

namespace LFortran {
namespace wasm {

void emit_unsigned_leb128(Vec<uint8_t> &code, Allocator &al, uint32_t n) {
    do {
        uint8_t byte = n & 0x7f;
        n >>= 7;
        if (n != 0) {
            byte |= 0x80;
        }
        code.push_back(al, byte);
    } while (n != 0);
}

void emit_signed_leb128(Vec<uint8_t> &code, Allocator &al, int32_t n) { // for i32
    bool more = true;
    do {
        uint8_t byte = n & 0x7f;
        n >>= 7;
        more = !((((n == 0) && ((byte & 0x40) == 0)) ||
                  ((n == -1) && ((byte & 0x40) != 0))));
        if (more) {
            byte |= 0x80;
        }
        code.push_back(al, byte);
    } while (more);
}

void emit_signed_leb128(Vec<uint8_t> &code, Allocator &al, int64_t n) { // for i64
    bool more = true;
    do {
        uint8_t byte = n & 0x7f;
        n >>= 7;
        more = !((((n == 0) && ((byte & 0x40) == 0)) ||
                  ((n == -1) && ((byte & 0x40) != 0))));
        if (more) {
            byte |= 0x80;
        }
        code.push_back(al, byte);
    } while (more);
}

// function to emit header of Wasm Binary Format
void emit_header(Vec<uint8_t> &code, Allocator &al) {
    code.push_back(al, 0x00);
    code.push_back(al, 0x61);
    code.push_back(al, 0x73);
    code.push_back(al, 0x6D);
    code.push_back(al, 0x01);
    code.push_back(al, 0x00);
    code.push_back(al, 0x00);
    code.push_back(al, 0x00);
}

// function to emit unsigned 32 bit integer
void emit_u32(Vec<uint8_t> &code, Allocator &al, uint32_t x) {
    emit_unsigned_leb128(code, al, x);
}

// function to emit signed 32 bit integer
void emit_i32(Vec<uint8_t> &code, Allocator &al, int32_t x) {
    emit_signed_leb128(code, al, x);
}

// function to emit signed 64 bit integer
void emit_i64(Vec<uint8_t> &code, Allocator &al, int64_t x) {
    emit_signed_leb128(code, al, x);
}

// function to append a given bytecode to the end of the code
void emit_b8(Vec<uint8_t> &code, Allocator &al, uint8_t x) {
    code.push_back(al, x);
}

void emit_u32_b32_idx(Vec<uint8_t> &code, Allocator &al, uint32_t idx,
                      uint32_t section_size) {
    /*
    Encodes the integer `i` using LEB128 and adds trailing zeros to always
    occupy 4 bytes. Stores the int `i` at the index `idx` in `code`.
    */
    Vec<uint8_t> num;
    num.reserve(al, 4);
    emit_unsigned_leb128(num, al, section_size);
    std::vector<uint8_t> num_4b = {0x80, 0x80, 0x80, 0x00};
    assert(num.size() <= 4);
    for (uint32_t i = 0; i < num.size(); i++) {
        num_4b[i] |= num[i];
    }
    for (uint32_t i = 0; i < 4u; i++) {
        code.p[idx + i] = num_4b[i];
    }
}

// function to fixup length at the given length index
void fixup_len(Vec<uint8_t> &code, Allocator &al, uint32_t len_idx) {
    uint32_t section_len = code.size() - len_idx - 4u;
    emit_u32_b32_idx(code, al, len_idx, section_len);
}

// function to emit length placeholder
uint32_t emit_len_placeholder(Vec<uint8_t> &code, Allocator &al) {
    uint32_t len_idx = code.size();
    code.push_back(al, 0x00);
    code.push_back(al, 0x00);
    code.push_back(al, 0x00);
    code.push_back(al, 0x00);
    return len_idx;
}

// function to emit a i32.const instruction
void emit_i32_const(Vec<uint8_t> &code, Allocator &al, int32_t x) {
    code.push_back(al, 0x41);
    emit_i32(code, al, x);
}

// function to emit a i64.const instruction
void emit_i64_const(Vec<uint8_t> &code, Allocator &al, int64_t x) {
    code.push_back(al, 0x42);
    emit_i64(code, al, x);
}

// function to emit end of wasm expression
void emit_expr_end(Vec<uint8_t> &code, Allocator &al) {
    code.push_back(al, 0x0B);
}

// function to emit get local variable at given index
void emit_get_local(Vec<uint8_t> &code, Allocator &al, uint32_t idx) {
    code.push_back(al, 0x20);
    emit_u32(code, al, idx);
}

// function to emit set local variable at given index
void emit_set_local(Vec<uint8_t> &code, Allocator &al, uint32_t idx) {
    code.push_back(al, 0x21);
    emit_u32(code, al, idx);
}

// function to emit call instruction
void emit_call(Vec<uint8_t> &code, Allocator &al, uint32_t idx) {
    code.push_back(al, 0x10);
    emit_u32(code, al, idx);
}

void emit_export_fn(Vec<uint8_t> &code, Allocator &al, const std::string& name,
                    uint32_t idx) {
    std::vector<uint8_t> name_bytes(name.size());
    std::memcpy(name_bytes.data(), name.data(), name.size());
    emit_u32(code, al, name_bytes.size());
    for(auto &byte:name_bytes)
        emit_b8(code, al, byte);
    emit_b8(code, al, 0x00);
    emit_u32(code, al, idx);
}

void encode_section(Vec<uint8_t> &des, Vec<uint8_t> &section_content, Allocator &al, uint32_t section_id){
    // every section in WebAssembly is encoded by adding its section id, followed by the content size and lastly the contents
    emit_u32(des, al, section_id);
    emit_u32(des, al, section_content.size());
    for(auto &byte:section_content){
        des.push_back(al, byte);
    }
}


// function to emit i32.clz instruction
void emit_i32_clz(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x67); }

// function to emit i32.ctz instruction
void emit_i32_ctz(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x68); }

// function to emit i32.popcnt instruction
void emit_i32_popcnt(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x69); }

// function to emit i32.add instruction
void emit_i32_add(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6A); }

// function to emit i32.sub instruction
void emit_i32_sub(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6B); }

// function to emit i32.mul instruction
void emit_i32_mul(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6C); }

// function to emit i32.div_s instruction
void emit_i32_div_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6D); }

// function to emit i32.div_u instruction
void emit_i32_div_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6E); }

// function to emit i32.rem_s instruction
void emit_i32_rem_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x6F); }

// function to emit i32.rem_u instruction
void emit_i32_rem_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x70); }

// function to emit i32.and instruction
void emit_i32_and(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x71); }

// function to emit i32.or instruction
void emit_i32_or(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x72); }

// function to emit i32.xor instruction
void emit_i32_xor(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x73); }

// function to emit i32.shl instruction
void emit_i32_shl(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x74); }

// function to emit i32.shr_s instruction
void emit_i32_shr_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x75); }

// function to emit i32.shr_u instruction
void emit_i32_shr_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x76); }

// function to emit i32.rotl instruction
void emit_i32_rotl(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x77); }

// function to emit i32.rotr instruction
void emit_i32_rotr(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x78); }

// function to emit i64.clz instruction
void emit_i64_clz(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x79); }

// function to emit i64.ctz instruction
void emit_i64_ctz(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7A); }

// function to emit i64.popcnt instruction
void emit_i64_popcnt(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7B); }

// function to emit i64.add instruction
void emit_i64_add(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7C); }

// function to emit i64.sub instruction
void emit_i64_sub(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7D); }

// function to emit i64.mul instruction
void emit_i64_mul(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7E); }

// function to emit i64.div_s instruction
void emit_i64_div_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x7F); }

// function to emit i64.div_u instruction
void emit_i64_div_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x80); }

// function to emit i64.rem_s instruction
void emit_i64_rem_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x81); }

// function to emit i64.rem_u instruction
void emit_i64_rem_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x82); }

// function to emit i64.and instruction
void emit_i64_and(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x83); }

// function to emit i64.or instruction
void emit_i64_or(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x84); }

// function to emit i64.xor instruction
void emit_i64_xor(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x85); }

// function to emit i64.shl instruction
void emit_i64_shl(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x86); }

// function to emit i64.shr_s instruction
void emit_i64_shr_s(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x87); }

// function to emit i64.shr_u instruction
void emit_i64_shr_u(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x88); }

// function to emit i64.rotl instruction
void emit_i64_rotl(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x89); }

// function to emit i64.rotr instruction
void emit_i64_rotr(Vec<uint8_t> &code, Allocator &al) { code.push_back(al, 0x8A); }


}  // namespace wasm

}  // namespace LFortran
