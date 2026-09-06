/*
 * r_compat.h -- compatibilidade CRAN para o codigo C original do ASAP
 * Aplicado via -include r_compat.h no PKG_CPPFLAGS
 */
#ifndef R_COMPAT_H
#define R_COMPAT_H
#include <stdio.h>
#include <stdlib.h>
#ifdef __cplusplus
extern "C" {
#endif
void r_compat_printf(const char *fmt, ...);
int  r_compat_sprintf(char *buf, const char *fmt, ...);
void r_compat_exit(int code);
int  r_compat_rand(void);
int  r_compat_asprintf(char **strp, const char *fmt, ...);
extern FILE *r_compat_null_stream;
#ifdef __cplusplus
}
#endif
/* NAO redefinir quando a compilar o proprio r_compat.c
 * (o ficheiro define R_COMPAT_IMPL antes de incluir qualquer coisa) */
#ifndef R_COMPAT_IMPL
#undef  printf
#define printf(...)     r_compat_printf(__VA_ARGS__)
#undef  sprintf
#define sprintf(...)    r_compat_sprintf(__VA_ARGS__)
#undef  putchar
#define putchar(c)      r_compat_printf("%c", (c))
#undef  puts
#define puts(s)         r_compat_printf("%s\n", (s))
#undef  exit
#define exit(code)      r_compat_exit(code)
#undef  rand
#define rand()          r_compat_rand()
#undef  asprintf
#define asprintf(strp, ...)  r_compat_asprintf((strp), __VA_ARGS__)
#undef  stderr
#define stderr          r_compat_null_stream
#undef  stdout
#define stdout          r_compat_null_stream
#endif /* R_COMPAT_IMPL */
#endif /* R_COMPAT_H */
