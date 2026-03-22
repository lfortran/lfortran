/*
 * C functions for bindc_43: struct returned by value / passed by VALUE
 *
 * Tests BIND(C) derived type interoperability with by-value semantics.
 */

typedef struct { float x, y; } point_t;
typedef struct { point_t origin; float width, height; } rect_t;

point_t c43_make_point(float x, float y) {
    point_t p;
    p.x = x;
    p.y = y;
    return p;
}

rect_t c43_make_rect(float ox, float oy, float w, float h) {
    rect_t r;
    r.origin.x = ox;
    r.origin.y = oy;
    r.width = w;
    r.height = h;
    return r;
}

float c43_point_dist_sq(point_t a, point_t b) {
    float dx = a.x - b.x;
    float dy = a.y - b.y;
    return dx * dx + dy * dy;
}

float c43_rect_area(rect_t r) {
    return r.width * r.height;
}

point_t c43_add_points(point_t a, point_t b) {
    point_t r;
    r.x = a.x + b.x;
    r.y = a.y + b.y;
    return r;
}
