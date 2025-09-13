#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// Dummy getcwd function that mimics the real getcwd behavior
char* getcwd_dummy(char* buf, size_t size) {
    const char* dummy_path = "/home/user/test/directory";
    size_t path_len = strlen(dummy_path);
    
    if (buf == NULL) {
        printf("ERROR: Received NULL buffer pointer!\n");
        return NULL;
    }
    
    if (size <= path_len) {
        printf("ERROR: Buffer too small! Need %zu, got %zu\n", path_len + 1, size);
        return NULL;
    }
    
    // Debug: Print what we received
    printf("C function received:\n");
    printf("  - Buffer pointer: %p\n", (void*)buf);
    printf("  - Buffer size: %zu\n", size);
    
    // Copy the dummy path to the buffer
    strcpy(buf, dummy_path);
    
    printf("  - Copied path: '%s'\n", buf);
    printf("  - Path length: %zu\n", strlen(buf));
    
    return buf;  // Return the buffer pointer on success
}

char* test_char_array(char* buffer, int len) {
    printf("C test_char_array received:\n");
    printf("  - Buffer pointer: %p\n", (void*)buffer);
    printf("  - Length parameter: %d\n", len);
    
    if (buffer == NULL) {
        printf("  - ERROR: NULL buffer!\n");
        return NULL;
    }
    
    // Write a test string
    const char* test_str = "Hello from C!";
    strncpy(buffer, test_str, len - 1);
    buffer[len - 1] = '\0';
    printf("  - Wrote to buffer: '%s'\n", buffer);
    return buffer;
}
