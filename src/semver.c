/*
 * PostgreSQL type definitions for semver type
 * Written by Sam Vilain
 * sam@vilain.net
 *
 * Copyright 2011, Sam Vilain.  This program is Free Software; see the
 * README.md file for the license conditions.
 */

#include "postgres.h"
#include <limits.h>
#include "utils/builtins.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

/* IO methods */
Datum       semver_in(PG_FUNCTION_ARGS);
Datum       semver_out(PG_FUNCTION_ARGS);

Datum       semver_eq(PG_FUNCTION_ARGS);
Datum       hash_semver(PG_FUNCTION_ARGS);
Datum       semver_ne(PG_FUNCTION_ARGS);
Datum       semver_lt(PG_FUNCTION_ARGS);
Datum       semver_le(PG_FUNCTION_ARGS);
Datum       semver_ge(PG_FUNCTION_ARGS);
Datum       semver_gt(PG_FUNCTION_ARGS);
Datum       semver_cmp(PG_FUNCTION_ARGS);

/* these typecasts are necessary for passing to functions that take text */
Datum       text_to_semver(PG_FUNCTION_ARGS);
Datum       semver_to_text(PG_FUNCTION_ARGS);

/* this constructor gives access to the lax parsing mode */
Datum       to_semver(PG_FUNCTION_ARGS);

Datum       semver_smaller(PG_FUNCTION_ARGS);
Datum       semver_larger(PG_FUNCTION_ARGS);

/* heap format of version numbers */
typedef int32 vernum;

/* memory/heap structure (not for binary marshalling) */
typedef struct semver
{
    int32 vl_len_;  /* varlena header */
    vernum numbers[3];
    char patchname[]; /* patch name, including the null byte for convenience */
} semver;

// forward declarations, mostly to shut the compiler up but some are
// actually necessary.
char*   emit_semver(semver* version);
semver* make_semver(const int *numbers, const char* patchname);
semver* parse_semver(char* str, bool lax);
int     patchnamecmp(const char* a, const char* b);
int     _semver_cmp(semver* a, semver* b);

semver* make_semver(const int *numbers, const char* patchname) {
    int varsize = offsetof(semver, patchname) + (patchname ? strlen(patchname) : 0) + 1;
    semver *rv = palloc(varsize);
    int i;
    SET_VARSIZE(rv, varsize);
    for (i = 0; i < 3; i++) {
        rv->numbers[i] = numbers[i];
    }
    if (patchname) {
        strcpy(rv->patchname, patchname);
    }
    else {
        rv->patchname[0] = '\0';
    }
    return rv;
}

/* creating a currency from a string */
semver* parse_semver(char* str, bool lax)
{
    int numbers[3];
    char* patchname, *ptr, *nptr;
    char junk[2];
    semver * newval;
    long int n;
    int i, x;

    ptr = str;
    if (lax) { x = strspn(ptr, " \t"); ptr += x; }
        
    for (i = 0; i < 3; i++) {
        n = strtol(ptr, &nptr, 10);
        if (ptr == nptr) {
            if (lax) {
                n = 0;
            }
            else {
                elog(ERROR, "bad semver value '%s': expected number at %s", str, ptr);
            }
        }
        if ( n > INT_MAX )
            elog(ERROR, "bad semver value '%s': version number exceeds 31-bit range", str);

        if ( !lax && *ptr == '0' && n != 0 )
            elog(ERROR, "bad semver value '%s': semver version numbers can't start with 0", str);

        numbers[i] = n;
        ptr = nptr;
        
        if (lax) { x = strspn(ptr, " \t"); ptr += x; }
        if (i < 2) {
            if (*ptr == '.') {
                ptr++;
                if (lax) { x = strspn(ptr, " \t"); ptr += x; }
            }
            else {
                if (!lax) {
                    elog(ERROR, "bad semver value '%s': expected '.' at: '%s'", str, ptr);
                }
            }
        }
    }

    if (lax) { x = strspn(ptr, " \t"); ptr += x; }
    
    if ( strlen(ptr) ) {
        if ( !( ( *ptr >= 'A' && *ptr <= 'Z' ) ||
            ( *ptr >= 'a' && *ptr <= 'z' ) ) )
            elog(ERROR, "bad patchlevel '%s' in semver value '%s' (must start with a letter)", ptr, str);
        patchname = palloc(strlen(ptr));
        x = sscanf(ptr, "%[A-Za-z0-9-]%c", patchname, (char*)&junk);
        if (x == 2) {
            if (!lax || !(*junk == ' ' || *junk == '\t' ) ) {
                elog(ERROR, "bad patchlevel '%s' in semver value '%s' (contains invalid character)", ptr, str);
            }
            ptr += strlen(patchname);
            if (lax) {
                x = strspn(ptr, " \t");
                ptr += x;
                if (strlen(ptr))
                    elog(ERROR, "bad semver value '%s' (contains dividing whitespace)", str);
            }
        }
    }
    else {
        patchname = 0;
    }

    newval = make_semver(numbers, patchname);
    if (patchname)
        pfree(patchname);

    return newval;
}

char* emit_semver(semver* version) {
    int len;
    char tmpbuf[32];
    char *buf;

    len = snprintf(
        tmpbuf, sizeof(tmpbuf),"%d.%d.%d%s",
        version->numbers[0],
        version->numbers[1],
        version->numbers[2],
        version->patchname
    );

    /* Should cover the vast majority of cases. */
    if (len < sizeof(tmpbuf)) return pstrdup(tmpbuf);

    /* Try agin, this time with the known length. */
    buf = palloc(len+1);
    snprintf(
        buf, len + 1,"%d.%d.%d%s",
        version->numbers[0],
        version->numbers[1],
        version->numbers[2],
        version->patchname
    );
    return buf;
}

/*
 * Pg bindings
 */

/* input function: C string */
PG_FUNCTION_INFO_V1(semver_in);
Datum
semver_in(PG_FUNCTION_ARGS)
{
    char *str = PG_GETARG_CSTRING(0);
    semver *result = parse_semver(str, false);
    if (!result)
        PG_RETURN_NULL();

    PG_RETURN_POINTER(result);
}/* output function: C string */

PG_FUNCTION_INFO_V1(semver_out);
Datum
semver_out(PG_FUNCTION_ARGS)
{
    semver* amount = (void*)PG_GETARG_POINTER(0);
    char *result;
    result = emit_semver(amount);

    PG_RETURN_CSTRING(result);
}

PG_FUNCTION_INFO_V1(text_to_semver);
Datum
text_to_semver(PG_FUNCTION_ARGS)
{
    text* sv = PG_GETARG_TEXT_PP(0);
    semver* rs = parse_semver(text_to_cstring(sv), false);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(semver_to_text);
Datum
semver_to_text(PG_FUNCTION_ARGS)
{
    semver* semver = (void*) PG_GETARG_POINTER(0);
    char* xxx = emit_semver(semver);
    text* res = cstring_to_text(xxx);
    pfree(xxx);
    PG_RETURN_TEXT_P(res);
}

int patchnamecmp(const char* a, const char* b)
{
    if (*a == '\0' && *b != '\0') {
        return 1;
    }
    if (*a != '\0' && *b == '\0') {
        return -1;
    }
    return strcasecmp(a, b);
}

/* comparisons */
int _semver_cmp(semver* a, semver* b)
{
    int rv, i, a_x, b_x;
    rv = 0;
    for (i = 0; i < 3; i++) {
        a_x = a->numbers[i];
        b_x = b->numbers[i];
        if (a_x < b_x) {
            rv = -1;
            break;
        }
        else if (a_x > b_x) {
            rv = 1;
            break;
        }
    }
    if (rv == 0) {
        rv = patchnamecmp(a->patchname, b->patchname);
    }
    return rv;
}

PG_FUNCTION_INFO_V1(semver_eq);
Datum
semver_eq(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff == 0);
}

PG_FUNCTION_INFO_V1(semver_ne);
Datum
semver_ne(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff != 0);
}

PG_FUNCTION_INFO_V1(semver_le);
Datum
semver_le(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff <= 0);
}

PG_FUNCTION_INFO_V1(semver_lt);
Datum
semver_lt(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff < 0);
}

PG_FUNCTION_INFO_V1(semver_ge);
Datum
semver_ge(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff >= 0);
}

PG_FUNCTION_INFO_V1(semver_gt);
Datum
semver_gt(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff > 0);
}

PG_FUNCTION_INFO_V1(semver_cmp);
Datum
semver_cmp(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_INT32(diff);
}

/* from catalog/pg_proc.h */
#define hashtext 400 
#define hashint2 449

/* so the '=' function can be 'hashes' */
PG_FUNCTION_INFO_V1(hash_semver);
Datum
hash_semver(PG_FUNCTION_ARGS)
{
    semver* version = (void*)PG_GETARG_POINTER(0);
    uint32 hash = 0;
    int i;
    Datum patchname;

    if (*version->patchname != '\0') {
        patchname = CStringGetTextDatum(version->patchname);
        hash = OidFunctionCall1(hashtext, patchname);
    }
    for (i = 0; i < 3; i++) {
        hash = (hash << (7+(i<<1))) & (hash >> (25-(i<<1)));
        hash ^= OidFunctionCall1(hashint2, version->numbers[i]);
    }

    PG_RETURN_INT32(hash);
}

PG_FUNCTION_INFO_V1(semver_larger);
Datum
semver_larger(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    if (diff >= 0) {
        PG_FREE_IF_COPY(b, 1);
        PG_RETURN_POINTER(a);
    }
    else {
        PG_FREE_IF_COPY(a, 0);
        PG_RETURN_POINTER(b);
    }
}

PG_FUNCTION_INFO_V1(semver_smaller);
Datum
semver_smaller(PG_FUNCTION_ARGS)
{
    semver* a = (void*)PG_GETARG_POINTER(0);
    semver* b = (void*)PG_GETARG_POINTER(1);
    int diff = _semver_cmp(a, b);
    if (diff <= 0) {
        PG_FREE_IF_COPY(b, 1);
        PG_RETURN_POINTER(a);
    }
    else {
        PG_FREE_IF_COPY(a, 0);
        PG_RETURN_POINTER(b);
    }
}

PG_FUNCTION_INFO_V1(to_semver);
Datum
to_semver(PG_FUNCTION_ARGS)
{
    text* sv = PG_GETARG_TEXT_PP(0);
    semver* rs = parse_semver(text_to_cstring(sv), true);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_POINTER(rs);
}
