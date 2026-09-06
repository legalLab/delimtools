/*
 * r_compat.c -- implementacao das substituicoes CRAN-compativeis
 */
#define R_COMPAT_IMPL  /* impede que r_compat.h se auto-redefina */
#include <R.h>
#include <Rinternals.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *r_compat_null_stream = NULL;

static void r_compat_init_null_stream(void)
{
    if (r_compat_null_stream == NULL) {
        r_compat_null_stream = fopen("/dev/null", "w");
        if (r_compat_null_stream == NULL) {
            r_compat_null_stream = tmpfile();
        }
    }
}

void r_compat_printf(const char *fmt, ...)
{
    char buf[4096];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    Rprintf("%s", buf);
}

int r_compat_sprintf(char *buf, const char *fmt, ...)
{
    va_list ap;
    int ret;
    va_start(ap, fmt);
    ret = vsnprintf(buf, 8192, fmt, ap);
    va_end(ap);
    r_compat_init_null_stream();
    return ret;
}

void r_compat_exit(int code)
{
    error("ASAP internal error (exit called with code %d). "
          "Please report this issue.", code);
}

int r_compat_rand(void)
{
    return (int)(unif_rand() * 2147483647.0);
}

int r_compat_asprintf(char **strp, const char *fmt, ...)
{
    va_list ap, ap_copy;
    va_start(ap, fmt);
    va_copy(ap_copy, ap);

    int len = vsnprintf(NULL, 0, fmt, ap_copy);
    va_end(ap_copy);

    if (len < 0) {
        va_end(ap);
        *strp = NULL;
        return -1;
    }

    *strp = (char *) malloc((size_t) len + 1);
    if (*strp == NULL) {
        va_end(ap);
        return -1;
    }

    int result = vsnprintf(*strp, (size_t) len + 1, fmt, ap);
    va_end(ap);
    return result;
}
