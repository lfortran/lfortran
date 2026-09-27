struct local_type {
    int value;
};
void global_init_29_fill(struct local_type *);

int main(void)
{
    struct local_type x = {0};
    global_init_29_fill(&x);
    return x.value != 4;
}
