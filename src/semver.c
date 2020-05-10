//  -*- tab-width:4; c-basic-offset:4; indent-tabs-mode:nil;  -*-
/*
 * PostgreSQL type definitions for semver type
 * Written by:
 * + Sam Vilain <sam@vilain.net>
 * + Tom Davis <tom@recursivedream.com>
 * + Xavier Caron <xcaron@gmail.com>
 *
 * Copyright 2010-2018 The pg-semver Maintainers. This program is Free
 * Software; see the LICENSE file for the license conditions.
 */

#include "postgres.h"
#include <string.h>
#include <ctype.h>
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

Datum       is_semver(PG_FUNCTION_ARGS);

Datum       semver_smaller(PG_FUNCTION_ARGS);
Datum       semver_larger(PG_FUNCTION_ARGS);

/* these functions allow users to access individual parts of the semver */
Datum       get_semver_major(PG_FUNCTION_ARGS);
Datum       get_semver_minor(PG_FUNCTION_ARGS);
Datum       get_semver_patch(PG_FUNCTION_ARGS);
Datum       get_semver_prerelease(PG_FUNCTION_ARGS);

/* heap format of version numbers */
typedef int32 vernum;

/* memory/heap structure (not for binary marshalling) */
typedef struct semver
{
    int32 vl_len_;  /* varlena header */
    vernum numbers[3];
    char prerel[]; /* pre-release, including the null byte for convenience */
} semver;

#define PG_GETARG_SEMVER_P(n) (semver *)PG_GETARG_POINTER(n)

// forward declarations, mostly to shut the compiler up but some are
// actually necessary.
char*   emit_semver(semver* version);
semver* make_semver(const int *numbers, const char* prerel);
semver* parse_semver(char* str, bool lax, bool throw, bool *bad);
int     prerelcmp(const char* a, const char* b);
int     _semver_cmp(semver* a, semver* b);
char*   strip_meta(const char* str);
int     tail_cmp(char *lhs, char *rhs);

semver* make_semver(const int *numbers, const char* prerel) {
    int varsize = offsetof(semver, prerel) + (prerel ? strlen(prerel) : 0) + 1;
    semver *rv = palloc(varsize);
    int i;
    SET_VARSIZE(rv, varsize);
    for (i = 0; i < 3; i++) {
        rv->numbers[i] = numbers[i];
    }
    if (prerel) {
        strcpy(rv->prerel, prerel);
    }
    else {
        rv->prerel[0] = '\0';
    }
    return rv;
}

semver* parse_semver(char* str, bool lax, bool throw, bool* bad)
{
    int parts[] = {-1, -1, -1};
    long int num;
    int len;
    int i = 0;
    int p = 0;
    int atchar = 0;
    int curpart = 0;
    char next;
    char* patch = 0;
    char* ptr, *endptr;
    bool dotlast = false;
    bool started_prerel = false;
    bool started_meta = false;
    bool skip_char = false;
    bool pred;
    semver* newval;

    *bad = false;

    ptr = str;
    len = strlen(str);

    do {
        next = (char)*ptr;
        skip_char = false;
        if (curpart < 3 && parts[2] == -1) {  // Still figuring out X.Y.Z
            if (next == '.') {  // First, check if we hit a period
                ptr++;
                atchar++;
                curpart++;
            } else {  // OK, it should be a version part number then
                num = strtol(ptr, &endptr, 10);
                // N.B. According to strtol(3), a valid number may be preceded
                // by a single +/-, so a value like 0.1-1 will end up being
                // parsed incorrectly when in `lax` mode. It will in fact end
                // up being 0.0.0 because {0, 0, -1} is coerced to {0, 0, 0}.
                // Not fun enough? 0.0+2 becomes 0.2.0!
                if (ptr == endptr || next == '-' || next == '+') {  // Not a number
                    if (lax) {
                        // Since it's not a period, we have to assume it's a legit pre-
                        // related token. We'll skip to the next number part, but leave
                        // the pointers.
                        curpart++;
                        continue;
                    } else {
                        *bad = true;
                        if (throw)
                            elog(ERROR, "bad semver value '%s': expected number/separator at char %d", str, atchar);
                        else
                            break;
                    }
                }
                if (num > INT_MAX) {  // Too big
                    *bad = true;
                    if (throw)
                        elog(ERROR, "bad semver value '%s': version number exceeds 31-bit range", str);
                    else
                        break;
                }

                if (!started_meta && next == '0' && num != 0 && !lax) {  // Leading zeros
                    *bad = true;
                    if (throw)
                        elog(ERROR, "bad semver value '%s': semver version numbers can't start with 0", str);
                    else
                        break;
                }

                parts[curpart] = num;
                atchar += (strlen(ptr) - strlen(endptr));
                ptr = endptr;
            }
        } else {  // Onto pre-release/metadata
            if (!started_prerel && !started_meta && (next == '-' || (next != '+' && lax))) {  // Starts with -
                started_prerel = true;
                if (next == '-') {
                    skip_char = true;
                }
            } else if (next == '+') {
                if (started_meta) {
                    *bad = true;
                    elog(ERROR, "bad semver value '%s': cannot have multiple + (plus) characters in metadata", str);
                } else {
                    started_meta = true;
                }
            }

            if (!patch && (started_meta || started_prerel))
                patch = palloc(len - atchar + 1);

            if (!skip_char &&
                (!started_prerel && next != '-') &&
                (!started_meta && next != '+'))  {  // Didn't start with -/+
                *bad = true;
                if (throw)
                    elog(ERROR, "bad semver value '%s': expected - (dash) or + (plus) at char %d", str, atchar);
                else
                    break;
            }
            if (next == '.' && (dotlast || (atchar + 1) == len)) {
                *bad = true;
                if (throw)
                    elog(ERROR, "bad semver value '%s': empty pre-release section at char %d", str, atchar);
                else
                    break;
            }

            if (!skip_char && (next != '.' && next != '+' && next != '-' && !isalpha(next) && !isdigit(next))) {
                if (lax && isspace(next))  // In lax mode, ignore whitespace
                    skip_char = true;
                else {
                    *bad = true;
                    if (throw)
                        elog(ERROR, "bad semver value '%s': non-alphanumeric pre-release at char %d", str, atchar);
                    else
                        break;
                }
            }
            if ((started_prerel || started_meta) && !skip_char) {
                if ((i == 1 || patch[i-2] == '.') && patch[i-1] == '0' && isdigit(next)) {
                    pred = true;
                    // Scan ahead.
                    for (p = len - atchar; p < len; p++) {
                        if (str[p] == '.') {
                            // We got to the end of this bit.
                            break;
                        }
                        if (isalpha(str[p])) {
                            // If there is a letter, it's okay to start with a leading 0.
                            pred = false;
                            break;
                        }
                    }
                }

                if (!started_meta && (pred && !lax))   {  // Leading zeros
                    *bad = true;
                    if (throw)
                        elog(ERROR, "bad semver value '%s': semver prerelease numbers can't start with 0", str);
                    else
                        break;
                } else if (pred && lax)  {  // Swap erroneous leading zero with whatever this is
                    patch[i-1] = next;
                } else {
                    dotlast = (next == '.');
                    patch[i] = next;
                    i++;
                }
                pred = false;
            }
            atchar++;
            ptr++;
        }
    } while (atchar < len);

    for (p=0; p < 3; p++) {
        if (parts[p] == -1) {
            if (lax)
                parts[p] = 0;
            else {
                *bad = true;
                if (throw)
                    elog(ERROR, "bad semver value '%s': missing major, minor, or patch version", str);
                else
                    break;
            }
        }
    }

    if ((started_prerel || started_meta) && i == 0) {  // No pre-release value after -
        *bad = true;
        if (throw)
            elog(ERROR, "bad semver value '%s': expected alphanumeric at char %d", str, atchar);
    }

    if (started_prerel || started_meta)
        patch[i] = '\0';

    newval = make_semver(parts, patch);
    if (patch)
        pfree(patch);

    return newval;
}

char* emit_semver(semver* version) {
    int len;
    char tmpbuf[32];
    char *buf;

    if (*version->prerel == '\0') {
        len = snprintf(tmpbuf, sizeof(tmpbuf), "%d.%d.%d",
                       version->numbers[0],
                       version->numbers[1],
                       version->numbers[2]
            );
    }
    else {
        len = snprintf(
            tmpbuf, sizeof(tmpbuf),"%d.%d.%d%s%s",
            version->numbers[0],
            version->numbers[1],
            version->numbers[2],
            ((version->prerel)[0] == '+' ? "" : "-"),
            version->prerel
        );
    }

    /* Should cover the vast majority of cases. */
    if (len < sizeof(tmpbuf)) return pstrdup(tmpbuf);

    /* Try again, this time with the known length. */
    buf = palloc(len+1);
    if (*version->prerel == '\0') {
        len = snprintf(buf, len+1, "%d.%d.%d",
                       version->numbers[0],
                       version->numbers[1],
                       version->numbers[2]
            );
    }
    else {
        len = snprintf(
            buf, len+1, "%d.%d.%d%s%s",
            version->numbers[0],
            version->numbers[1],
            version->numbers[2],
            ((version->prerel)[0] == '+' ? "" : "-"),
            version->prerel
        );
    }
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
    bool bad = false;
    semver *result = parse_semver(str, false, true, &bad);
    if (!result)
        PG_RETURN_NULL();

    PG_RETURN_POINTER(result);
}/* output function: C string */

PG_FUNCTION_INFO_V1(semver_out);
Datum
semver_out(PG_FUNCTION_ARGS)
{
    semver* amount = PG_GETARG_SEMVER_P(0);
    char *result;
    result = emit_semver(amount);

    PG_RETURN_CSTRING(result);
}

PG_FUNCTION_INFO_V1(text_to_semver);
Datum
text_to_semver(PG_FUNCTION_ARGS)
{
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    semver* rs = parse_semver(text_to_cstring(sv), false, true, &bad);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(semver_to_text);
Datum
semver_to_text(PG_FUNCTION_ARGS)
{
    semver* sv = PG_GETARG_SEMVER_P(0);
    char* xxx = emit_semver(sv);
    text* res = cstring_to_text(xxx);
    pfree(xxx);
    PG_RETURN_TEXT_P(res);
}

PG_FUNCTION_INFO_V1(get_semver_major);
Datum
get_semver_major(PG_FUNCTION_ARGS)
{
    semver* sv = PG_GETARG_SEMVER_P(0);
    int major = sv->numbers[0];
    PG_RETURN_INT32(major);
}

PG_FUNCTION_INFO_V1(get_semver_minor);
Datum
get_semver_minor(PG_FUNCTION_ARGS)
{
    semver* sv = PG_GETARG_SEMVER_P(0);
    int minor = sv->numbers[1];
    PG_RETURN_INT32(minor);
}

PG_FUNCTION_INFO_V1(get_semver_patch);
Datum
get_semver_patch(PG_FUNCTION_ARGS)
{
    semver* sv = PG_GETARG_SEMVER_P(0);
    int patch = sv->numbers[2];
    PG_RETURN_INT32(patch);
}

PG_FUNCTION_INFO_V1(get_semver_prerelease);
Datum
get_semver_prerelease(PG_FUNCTION_ARGS)
{
    semver* sv = PG_GETARG_SEMVER_P(0);
    char* prerelease = strip_meta(sv->prerel);
    text* res = cstring_to_text(prerelease);
    PG_RETURN_TEXT_P(res);
}


/* Remove everything at and after "+" in a pre-release suffix */
char* strip_meta(const char *str)
{
    int n = strlen(str);
    char *copy = palloc(n + 1);
    int j = 0;   // current character
    strcpy(copy, str);

    while (j < n)
    {
        /* if current character is b */
        if (str[j] == '+') {
            break;
        } else {
            copy[j] = str[j];
            j++;
        }
    }
    copy[j] = '\0';
    return copy;
}

// http://semver.org/#spec-item-11:
// Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined
// by comparing each dot separated identifier from left to right until a difference is found as follows:
// identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens
// are compared lexically in ASCII sort order. Numeric identifiers always have lower precedence than
// non-numeric identifiers. A larger set of pre-release fields has a higher precedence than a smaller set,
// if all of the preceding identifiers are equal. Example:
// 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.

#define TAIL_CMP_LT -1
#define TAIL_CMP_EQ  0
#define TAIL_CMP_GT +1
#define TAIL_CMP_KO  9

int tail_cmp ( char *lhs, char *rhs ) {
  if ( !strcasecmp ( lhs, rhs ) ) return TAIL_CMP_EQ;

  char *dot = ".";
  char *l_last, *r_last;

  char *l_token = strtok_r ( lhs, dot, &l_last );
  char *r_token = strtok_r ( rhs, dot, &r_last );

  if (  l_token && !r_token ) return TAIL_CMP_LT;
  if ( !l_token &&  r_token ) return TAIL_CMP_GT;

  while ( l_token || r_token ) {
    if ( l_token && r_token ) {
      int l_numeric = isdigit ( l_token[0] );
      int r_numeric = isdigit ( r_token[0] );

      if ( l_numeric && r_numeric ) {
        int l_int = atoi ( l_token );
        int r_int = atoi ( r_token );

        if ( l_int < r_int ) return TAIL_CMP_LT;
        if ( l_int > r_int ) return TAIL_CMP_GT;
      }
      else if ( l_numeric ) {
        return TAIL_CMP_LT;
      }
      else if ( r_numeric ) {
        return TAIL_CMP_GT;
      }
      else {
        int cmp = strcasecmp ( l_token, r_token );

        if ( cmp ) return cmp > 0 ? TAIL_CMP_GT : TAIL_CMP_LT;
      }
    }
    else if ( l_token ) {
      return TAIL_CMP_GT;
    }
    else if ( r_token ) {
      return TAIL_CMP_LT;
    }

    l_token = strtok_r ( NULL, dot, &l_last );
    r_token = strtok_r ( NULL, dot, &r_last );
  }

  return TAIL_CMP_KO;
}

int prerelcmp(const char* a, const char* b)
{
    int res;
    char *ac, *bc;

    ac = strip_meta(a);
    bc = strip_meta(b);
    if (*ac == '\0' && *bc != '\0') {
        return 1;
    }
    if (*ac != '\0' && *bc == '\0') {
        return -1;
    }
    res = tail_cmp(ac, bc);
    pfree(ac);
    pfree(bc);
    return res;
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
        rv = prerelcmp(a->prerel, b->prerel);
    }
    return rv;
}

PG_FUNCTION_INFO_V1(semver_eq);
Datum
semver_eq(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff == 0);
}

PG_FUNCTION_INFO_V1(semver_ne);
Datum
semver_ne(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff != 0);
}

PG_FUNCTION_INFO_V1(semver_le);
Datum
semver_le(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff <= 0);
}

PG_FUNCTION_INFO_V1(semver_lt);
Datum
semver_lt(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff < 0);
}

PG_FUNCTION_INFO_V1(semver_ge);
Datum
semver_ge(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff >= 0);
}

PG_FUNCTION_INFO_V1(semver_gt);
Datum
semver_gt(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_RETURN_BOOL(diff > 0);
}

PG_FUNCTION_INFO_V1(semver_cmp);
Datum
semver_cmp(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
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
    semver* version = PG_GETARG_SEMVER_P(0);
    uint32 hash = 0;
    int i;
    Datum prerel;

    if (*version->prerel != '\0') {
        prerel = CStringGetTextDatum(version->prerel);
        hash = OidFunctionCall1(hashtext, prerel);
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
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    if (diff >= 0) {
        PG_RETURN_POINTER(a);
    }
    else {
        PG_RETURN_POINTER(b);
    }
}

PG_FUNCTION_INFO_V1(semver_smaller);
Datum
semver_smaller(PG_FUNCTION_ARGS)
{
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    if (diff <= 0) {
        PG_RETURN_POINTER(a);
    }
    else {
        PG_RETURN_POINTER(b);
    }
}

PG_FUNCTION_INFO_V1(to_semver);
Datum
to_semver(PG_FUNCTION_ARGS)
{
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    semver* rs = parse_semver(text_to_cstring(sv), true, true, &bad);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(is_semver);
Datum
is_semver(PG_FUNCTION_ARGS)
{
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    semver* rs = parse_semver(text_to_cstring(sv), false, false, &bad);
    if (rs != NULL) pfree(rs);
    PG_RETURN_BOOL(!bad);
}
