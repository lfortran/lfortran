/* Minimal stubs for LAPACK routines - just enough to link and run */

int lsame(void *a, void *b) { return 1; }
float slamch(void *cmach) { return 1.0f; }
float slange(void *norm, void *m, void *n, void *a, void *lda, void *work) { return 0.0f; }
void sgemm(void *transa, void *transb, void *m, void *n, void *k,
           void *alpha, void *a, void *lda, void *b, void *ldb,
           void *beta, void *c, void *ldc) {}
void slacpy(void *uplo, void *m, void *n, void *a, void *lda, void *b, void *ldb) {}
void slascl(void *type, void *kl, void *ku, void *cfrom, void *cto,
            void *m, void *n, void *a, void *lda, void *info) {}
void xerbla(void *srname, void *info) {}
