//  -*- tab-width:4; c-basic-offset:4; indent-tabs-mode:nil;  -*-
/*
 * PostgreSQL type definitions for semver type
 * Written by:
 * + Sam Vilain <sam@vilain.net>
 * + Tom Davis <tom@recursivedream.com>
 * + Xavier Caron <xcaron@gmail.com>
 * + David Wheeler <david@justatheory.com>
 *
 * Copyright 2010-2026 The pg-semver Maintainers. This program is Free
 * Software; see the LICENSE file for the license conditions.
 */

#include "math.h"

#include "postgres.h"
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stddef.h>
#include "utils/builtins.h"
#include "utils/numeric.h"
#include "catalog/pg_collation.h"
#include "access/hash.h"
#include "lib/stringinfo.h"
#include "libpq/pqformat.h"
#if PG_VERSION_NUM < 100000
#include "utils/int8.h"
#include "access/hash.h"
#elif PG_VERSION_NUM >= 160000
#include "varatt.h"
#endif

#include "version.h"

#ifdef PG_MODULE_MAGIC_EXT // Added in 18
PG_MODULE_MAGIC_EXT(.name = "semver", .version = SEMVER_VERSION);
#else
PG_MODULE_MAGIC; // Added in 8.2
#endif

/* IO methods */
Datum       semver_in(PG_FUNCTION_ARGS);
Datum       semver_out(PG_FUNCTION_ARGS);
Datum       semver_recv(PG_FUNCTION_ARGS);
Datum       semver_send(PG_FUNCTION_ARGS);

Datum       semver_eq(PG_FUNCTION_ARGS);
Datum       hash_semver(PG_FUNCTION_ARGS);
Datum       hash_semver_extended(PG_FUNCTION_ARGS);
Datum       semver_ne(PG_FUNCTION_ARGS);
Datum       semver_lt(PG_FUNCTION_ARGS);
Datum       semver_le(PG_FUNCTION_ARGS);
Datum       semver_ge(PG_FUNCTION_ARGS);
Datum       semver_gt(PG_FUNCTION_ARGS);
Datum       semver_cmp(PG_FUNCTION_ARGS);

/* these typecasts are necessary for passing to functions that take text */
Datum       text_to_semver(PG_FUNCTION_ARGS);
Datum       semver_to_text(PG_FUNCTION_ARGS);
Datum       numeric_to_semver(PG_FUNCTION_ARGS);
Datum       real_to_semver(PG_FUNCTION_ARGS);
Datum       double_to_semver(PG_FUNCTION_ARGS);
Datum       int_to_semver(PG_FUNCTION_ARGS);
Datum       smallint_to_semver(PG_FUNCTION_ARGS);
Datum       bigint_to_semver(PG_FUNCTION_ARGS);

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

Datum       get_semver_major_bigint(PG_FUNCTION_ARGS);
Datum       get_semver_minor_bigint(PG_FUNCTION_ARGS);
Datum       get_semver_patch_bigint(PG_FUNCTION_ARGS);
Datum       get_semver_major_text(PG_FUNCTION_ARGS);
Datum       get_semver_minor_text(PG_FUNCTION_ARGS);
Datum       get_semver_patch_text(PG_FUNCTION_ARGS);

Datum       to_semver1(PG_FUNCTION_ARGS);

#include "semver.h"

// forward declarations
int     prerelcmp(const char* a, const char* b);
int     _semver_cmp(semver* a, semver* b);
char*   strip_meta(const char* str);
int     tail_cmp(char *lhs, char *rhs);

static inline void get_semver2_components(const semver2 *sv2,
                                          const char **major, uint8 *major_len,
                                          const char **minor, uint8 *minor_len,
                                          const char **patch, uint8 *patch_len,
                                          const char **prerel) {
    size_t offset;
    char c;

    *major_len = sv2->len_major & 0x7F;
    *minor_len = sv2->len_minor & 0x7F;
    *patch_len = sv2->len_patch & 0x7F;

    *major = sv2->verstring;
    *minor = sv2->verstring + (size_t)*major_len + 1;
    *patch = sv2->verstring + (size_t)*major_len + 1 + (size_t)*minor_len + 1;

    offset = (size_t)*major_len + 1 + (size_t)*minor_len + 1 + (size_t)*patch_len;
    c = sv2->verstring[offset];
    if (c == '-')
        *prerel = sv2->verstring + offset + 1;
    else if (c == '+')
        *prerel = sv2->verstring + offset;
    else
        *prerel = "";
}

static bool parse_comp_to_int64(const char *str, uint8 len, int64 *val) {
    int64 res = 0;
    int i;

    if (len > 19) return false;
    for (i = 0; i < len; i++) {
        int d = str[i] - '0';
        if (res > (INT64_MAX - d) / 10) return false;
        res = res * 10 + d;
    }
    *val = res;
    return true;
}

static int cmp_v1_num_vs_v2_str(int32 v1_num, const char *v2_str, uint8 v2_len) {
    int64 v2_val = 0;
    int32 v2_num;
    int i;

    if (v2_len > 10) {
        return -1;
    }
    for (i = 0; i < v2_len; i++) {
        v2_val = v2_val * 10 + (v2_str[i] - '0');
    }
    if (v2_val > (int64) INT32_MAX) {
        return -1;
    }
    v2_num = (int32) v2_val;
    if (v1_num < v2_num) return -1;
    if (v1_num > v2_num) return 1;
    return 0;
}

semver* make_semver(const int *numbers, const char* prerel) {
    int varsize = offsetof(semver1, prerel) + (prerel ? strlen(prerel) : 0) + 1;
    semver *rv = palloc(varsize);
    int i;

    SET_VARSIZE(rv, varsize);
    for (i = 0; i < 3; i++) {
        rv->v1.numbers[i] = numbers[i];
    }
    if (prerel) {
        strcpy(rv->v1.prerel, prerel);
    } else {
        rv->v1.prerel[0] = '\0';
    }
    return rv;
}

semver* make_semver2(const char *verstr, uint8 len_major, uint8 len_minor, uint8 len_patch, bool has_prerel, bool has_build) {
    int varsize = offsetof(semver2, verstring) + strlen(verstr) + 1;
    semver2 *rv;
    uint8 tag = SEMVER2_FORMAT_VERSION;

    if (has_prerel) tag |= SEMVER2_FLAG_PREREL;
    if (has_build)  tag |= SEMVER2_FLAG_BUILD;

    rv = palloc(varsize);
    SET_VARSIZE(rv, varsize);

    rv->tag = SEMVER_DISCRIMINATOR_BIT | tag;
    rv->len_major = SEMVER_DISCRIMINATOR_BIT | (len_major & 0x7F);
    rv->len_minor = SEMVER_DISCRIMINATOR_BIT | (len_minor & 0x7F);
    rv->len_patch = SEMVER_DISCRIMINATOR_BIT | (len_patch & 0x7F);

    strcpy(rv->verstring, verstr);
    return (semver *) rv;
}

semver* parse_semver(char* str, bool lax, bool throw, bool* bad) {
    char num_bufs[3][128];
    int num_lens[3] = {0, 0, 0};
    int len;
    int i = 0;
    int p = 0;
    int atchar = 0;
    int curpart = 0;
    unsigned char next;
    char* patch = 0;
    char* ptr;
    bool dotlast = false;
    bool started_prerel = false;
    bool started_meta = false;
    bool skip_char = false;
    bool pred = false;
    semver* newval;
    char *num_start;
    char *norm_start;
    int raw_len;
    int norm_len;
    StringInfoData buf;

    *bad = false;

    while (*str == ' ') {
        str++;
    }
    ptr = str;
    len = strlen(str);
    if (lax) {
        while (len > 0 && str[len - 1] == ' ') {
            len--;
        }
    }

    do {
        // Make sure the next character is visible ASCII or nul terminator.
        if ((unsigned char)*ptr > '~' || (*ptr && (unsigned char)*ptr < ' ')) {
            *bad = true;
            if (!throw) break;
            ereport(ERROR,
                    (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                     errmsg("bad semver value '%s': invalid character at char %d", str, atchar+1)));
        }
        next = (unsigned char)*ptr;
        skip_char = false;
        if (curpart < 3 && num_lens[2] == 0) {  // Still figuring out X.Y.Z
            if (next == '.') {  // First, check if we hit a period
                ptr++;
                atchar++;
                curpart++;
            } else {  // OK, it should be a version part number then
                if (!isdigit(next)) {
                    if (lax && isspace(next)) {
                        ptr++;
                        atchar++;
                        continue;
                    }
                    if (lax && (next == '-' || next == '+')) {
                        curpart++;
                        continue;
                    }
                    if (lax) {
                        curpart++;
                        continue;
                    }
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': expected number/separator at char %d", str, atchar)));
                }

                num_start = ptr;
                while (atchar + (ptr - num_start) < len && isdigit((unsigned char)*ptr)) {
                    ptr++;
                }
                raw_len = ptr - num_start;

                if (!started_meta && *num_start == '0' && raw_len > 1 && !lax) {  // Leading zeros
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': semver version numbers can't start with 0", str)));
                }

                norm_start = num_start;
                if (lax) {
                    while (norm_start < ptr - 1 && *norm_start == '0') {
                        norm_start++;
                    }
                }
                norm_len = ptr - norm_start;
                if (norm_len > 127) {
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': version number component exceeds 127 digits", str)));
                }

                memcpy(num_bufs[curpart], norm_start, norm_len);
                num_bufs[curpart][norm_len] = '\0';
                num_lens[curpart] = norm_len;
                atchar += raw_len;
            }
        } else {  // Onto pre-release/metadata
            if (lax && isspace(next)) {
                atchar++;
                ptr++;
                continue;
            }
            if (!started_prerel && !started_meta && (next == '-' || (next != '+' && lax))) {  // Starts with -
                started_prerel = true;
                if (next == '-') {
                    skip_char = true;
                }
            } else if (next == '+') {
                if (started_meta) {
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': cannot have multiple + (plus) characters in metadata", str)));
                } else {
                    started_meta = true;
                }
            }

            if (!patch && (started_meta || started_prerel)) {
                patch = palloc(len - atchar + 1);
            }

            if (
                !skip_char &&
                (!started_prerel && next != '-') &&
                (!started_meta && next != '+')
            ) { // Didn't start with -/+
                *bad = true;
                if (!throw) break;
                ereport(ERROR,
                        (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                         errmsg("bad semver value '%s': expected - (dash) or + (plus) at char %d", str, atchar)));
            }
            if (next == '.' && (dotlast || (atchar + 1) == len || i == 0 || (i > 0 && patch[i-1] == '+'))) {
                *bad = true;
                if (!throw) break;
                ereport(ERROR,
                        (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                         errmsg("bad semver value '%s': empty pre-release section at char %d", str, atchar)));
            }

            if (!skip_char && (next != '.' && next != '+' && next != '-' && !isalpha(next) && !isdigit(next))) {
                if (lax && isspace(next)) { // In lax mode, ignore whitespace
                    skip_char = true;
                } else {
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': non-alphanumeric pre-release at char %d", str, atchar)));
                }
            }
            if ((started_prerel || started_meta) && !skip_char) {
                if (!started_meta && i >= 1 && (i == 1 || patch[i-2] == '.') && patch[i-1] == '0' && isdigit(next)) {
                    pred = true;
                    // Numeric identifiers must not include a leading 0. Scan ahead.
                    for (p = atchar; p < len; p++) {
                        if (str[p] == '.' || str[p] == '+') {
                            // We got to the end of this bit.
                            break;
                        }
                        if (isalpha((unsigned char)str[p]) || str[p] == '-') {
                            // If there is a letter or a dash, it's okay to start with a leading 0.
                            pred = false;
                            break;
                        }
                    }
                }

                if (!started_meta && (pred && !lax)) {  // Leading zeros
                    *bad = true;
                    if (!throw) break;
                    ereport(ERROR,
                            (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                             errmsg("bad semver value '%s': semver prerelease numbers can't start with 0", str)));
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

    for (p = 0; p < 3; p++) {
        if (num_lens[p] == 0) {
            if (lax) {
                num_bufs[p][0] = '0';
                num_bufs[p][1] = '\0';
                num_lens[p] = 1;
            } else {
                *bad = true;
                if (!throw) break;
                ereport(ERROR,
                        (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                         errmsg("bad semver value '%s': missing major, minor, or patch version", str)));
            }
        }
    }

    if ((started_prerel || started_meta) && (i == 0 || patch[i-1] == '+')) {  // No pre-release value after - or metadata after +
        *bad = true;
        if (throw) {
            ereport(ERROR,
                    (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                     errmsg("bad semver value '%s': expected alphanumeric at char %d", str, atchar)));
        } else {
            if (patch) pfree(patch);
            return NULL;
        }
    }

    if (*bad) {
        if (patch) pfree(patch);
        return NULL;
    }

    if (started_prerel || started_meta) {
        patch[i] = '\0';
    }

    initStringInfo(&buf);
    appendBinaryStringInfo(&buf, num_bufs[0], num_lens[0]);
    appendStringInfoChar(&buf, '.');
    appendBinaryStringInfo(&buf, num_bufs[1], num_lens[1]);
    appendStringInfoChar(&buf, '.');
    appendBinaryStringInfo(&buf, num_bufs[2], num_lens[2]);
    if (patch && patch[0] != '\0') {
        if (patch[0] != '+') {
            appendStringInfoChar(&buf, '-');
        }
        appendStringInfoString(&buf, patch);
    }

    newval = make_semver2(buf.data, (uint8) num_lens[0], (uint8) num_lens[1], (uint8) num_lens[2], started_prerel, started_meta);
    pfree(buf.data);
    if (patch) pfree(patch);

    return newval;
}

char* emit_semver(semver* version) {
    if (IS_SEMVER2(version)) {
        semver2 *sv2 = (semver2 *) version;
        return pstrdup(sv2->verstring);
    } else {
        int len;
        char tmpbuf[32];
        char *buf;

        if (*version->v1.prerel == '\0') {
            len = snprintf(
                tmpbuf, sizeof(tmpbuf), "%d.%d.%d",
                version->v1.numbers[0],
                version->v1.numbers[1],
                version->v1.numbers[2]
            );
        } else {
            len = snprintf(
                tmpbuf, sizeof(tmpbuf),"%d.%d.%d%s%s",
                version->v1.numbers[0],
                version->v1.numbers[1],
                version->v1.numbers[2],
                ((version->v1.prerel)[0] == '+' ? "" : "-"),
                version->v1.prerel
            );
        }

        /* Should cover the vast majority of cases. */
        if (len < sizeof(tmpbuf)) return pstrdup(tmpbuf);

        /* Try again, this time with the known length. */
        buf = palloc(len+1);
        if (*version->v1.prerel == '\0') {
            len = snprintf(
                buf, len+1, "%d.%d.%d",
                version->v1.numbers[0],
                version->v1.numbers[1],
                version->v1.numbers[2]
            );
        } else {
            len = snprintf(
                buf, len+1, "%d.%d.%d%s%s",
                version->v1.numbers[0],
                version->v1.numbers[1],
                version->v1.numbers[2],
                ((version->v1.prerel)[0] == '+' ? "" : "-"),
                version->v1.prerel
            );
        }
        return buf;
    }
}

/*
 * Pg bindings
 */

/* input function: C string */
PG_FUNCTION_INFO_V1(semver_in);
Datum
semver_in(PG_FUNCTION_ARGS) {
    char *str = PG_GETARG_CSTRING(0);
    bool bad = false;
    semver *result = parse_semver(str, false, true, &bad);
    if (!result) PG_RETURN_NULL();

    PG_RETURN_POINTER(result);
}

/* output function: C string */
PG_FUNCTION_INFO_V1(semver_out);
Datum
semver_out(PG_FUNCTION_ARGS) {
    semver* amount = PG_GETARG_SEMVER_P(0);
    char *result;
    result = emit_semver(amount);
    PG_FREE_IF_COPY(amount, 0);
    PG_RETURN_CSTRING(result);
}

/*
 * semver type send function
 *
 * The type is sent as text in binary mode, so this is almost the same as the
 * output function, but it's prefixed with a version number so we can change the
 * binary format sent in future if necessary. For now, only version 1 is
 * supported.
 */
PG_FUNCTION_INFO_V1(semver_send);
Datum
semver_send(PG_FUNCTION_ARGS) {
    semver *result = PG_GETARG_SEMVER_P(0);
    char   *str = emit_semver(result);
    char    version = 1;
    StringInfoData buf;

    pq_begintypsend(&buf);
    pq_sendbyte(&buf, version);
    pq_sendtext(&buf, str, strlen(str));
    pfree(str);
    PG_FREE_IF_COPY(result, 0);

    PG_RETURN_BYTEA_P(pq_endtypsend(&buf));
}

/*
 * semver type recv function
 *
 * The type is sent as text in binary mode, so this is almost the same as the
 * input function, but it's prefixed with a version number so we can change the
 * binary format sent in future if necessary. For now, only version 1 is
 * supported.
 */
PG_FUNCTION_INFO_V1(semver_recv);
Datum
semver_recv(PG_FUNCTION_ARGS) {
    StringInfo  buf = (StringInfo) PG_GETARG_POINTER(0);
    char        version = pq_getmsgbyte(buf);
    char       *str;
    int         nbytes;
    bool        bad = false;
    semver     *result;

    if (version != 1) {
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_BINARY_REPRESENTATION),
                 errmsg("unsupported semver type version number %d", version)));
    }

    str = pq_getmsgtext(buf, buf->len - buf->cursor, &nbytes);
    if (strlen(str) != (size_t) nbytes) {
        pfree(str);
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_BINARY_REPRESENTATION),
                 errmsg("invalid input syntax for type semver: contains null character")));
    }
    pq_getmsgend(buf);

    result = parse_semver(str, false, true, &bad);
    pfree(str);

    if (!result) PG_RETURN_NULL();
    PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(text_to_semver);
Datum
text_to_semver(PG_FUNCTION_ARGS) {
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    char *str = text_to_cstring(sv);
    semver* rs;

    if (strlen(str) != (size_t) VARSIZE_ANY_EXHDR(sv)) {
        pfree(str);
        PG_FREE_IF_COPY(sv, 0);
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                 errmsg("invalid input syntax for type semver: contains null character")));
    }

    rs = parse_semver(str, false, true, &bad);
    pfree(str);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(semver_to_text);
Datum
semver_to_text(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    char* xxx = emit_semver(sv);
    text* res = cstring_to_text(xxx);
    pfree(xxx);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_TEXT_P(res);
}

PG_FUNCTION_INFO_V1(get_semver_major);
Datum
get_semver_major(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int major = sv->v1.numbers[0];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32(major);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(major, major_len, &val) || val > INT32_MAX) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver major version \"%.*s\" exceeds 32-bit integer range", (int) major_len, major)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32((int32) val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_minor);
Datum
get_semver_minor(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int minor = sv->v1.numbers[1];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32(minor);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(minor, minor_len, &val) || val > INT32_MAX) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver minor version \"%.*s\" exceeds 32-bit integer range", (int) minor_len, minor)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32((int32) val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_patch);
Datum
get_semver_patch(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int patch = sv->v1.numbers[2];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32(patch);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(patch, patch_len, &val) || val > INT32_MAX) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver patch version \"%.*s\" exceeds 32-bit integer range", (int) patch_len, patch)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT32((int32) val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_major_bigint);
Datum
get_semver_major_bigint(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int64 major = (int64) sv->v1.numbers[0];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(major);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(major, major_len, &val)) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver major version \"%.*s\" exceeds 64-bit integer range", (int) major_len, major)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_minor_bigint);
Datum
get_semver_minor_bigint(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int64 minor = (int64) sv->v1.numbers[1];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(minor);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(minor, minor_len, &val)) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver minor version \"%.*s\" exceeds 64-bit integer range", (int) minor_len, minor)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_patch_bigint);
Datum
get_semver_patch_bigint(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    if (!IS_SEMVER2(sv)) {
        int64 patch = (int64) sv->v1.numbers[2];
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(patch);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        int64 val;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        if (!parse_comp_to_int64(patch, patch_len, &val)) {
            PG_FREE_IF_COPY(sv, 0);
            ereport(ERROR,
                    (errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
                     errmsg("semver patch version \"%.*s\" exceeds 64-bit integer range", (int) patch_len, patch)));
        }
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_INT64(val);
    }
}

PG_FUNCTION_INFO_V1(get_semver_major_text);
Datum
get_semver_major_text(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    text *res;
    if (!IS_SEMVER2(sv)) {
        char buf[32];
        pg_ltoa(sv->v1.numbers[0], buf);
        res = cstring_to_text(buf);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        res = cstring_to_text_with_len(major, major_len);
    }
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_TEXT_P(res);
}

PG_FUNCTION_INFO_V1(get_semver_minor_text);
Datum
get_semver_minor_text(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    text *res;
    if (!IS_SEMVER2(sv)) {
        char buf[32];
        pg_ltoa(sv->v1.numbers[1], buf);
        res = cstring_to_text(buf);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        res = cstring_to_text_with_len(minor, minor_len);
    }
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_TEXT_P(res);
}

PG_FUNCTION_INFO_V1(get_semver_patch_text);
Datum
get_semver_patch_text(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    text *res;
    if (!IS_SEMVER2(sv)) {
        char buf[32];
        pg_ltoa(sv->v1.numbers[2], buf);
        res = cstring_to_text(buf);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        res = cstring_to_text_with_len(patch, patch_len);
    }
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_TEXT_P(res);
}

PG_FUNCTION_INFO_V1(get_semver_prerelease);
Datum
get_semver_prerelease(PG_FUNCTION_ARGS) {
    semver* sv = PG_GETARG_SEMVER_P(0);
    char* prerelease;
    text* res;

    if (!IS_SEMVER2(sv)) {
        prerelease = strip_meta(sv->v1.prerel);
    } else {
        const char *major, *minor, *patch, *prerel;
        uint8 major_len, minor_len, patch_len;
        get_semver2_components((const semver2 *) sv, &major, &major_len, &minor, &minor_len, &patch, &patch_len, &prerel);
        prerelease = strip_meta(prerel);
    }
    res = cstring_to_text(prerelease);
    pfree(prerelease);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_TEXT_P(res);
}

/* Remove everything at and after "+" in a pre-release suffix */
char* strip_meta(const char *str) {
    const char *plus = strchr(str, '+');
    size_t len = plus ? (size_t)(plus - str) : strlen(str);
    char *copy = palloc(len + 1);
    memcpy(copy, str, len);
    copy[len] = '\0';
    return copy;
}

// https://semver.org/#spec-item-11:
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

int tail_cmp(char *lhs, char *rhs) {
    const char *dot = ".";
    char *l_last, *r_last, *l_token, *r_token;

    if (!strcmp(lhs, rhs)) return TAIL_CMP_EQ;

    l_token = strtok_r(lhs, dot, &l_last);
    r_token = strtok_r(rhs, dot, &r_last);

    while (l_token || r_token) {
        if (l_token && r_token) {
            size_t l_len = strlen(l_token);
            size_t r_len = strlen(r_token);
            bool l_numeric = (l_len > 0 && strspn(l_token, "0123456789") == l_len);
            bool r_numeric = (r_len > 0 && strspn(r_token, "0123456789") == r_len);

            if (l_numeric && r_numeric) {
                const char *p1 = l_token;
                const char *p2 = r_token;
                size_t len1;
                size_t len2;
                int cmp;

                // Prerelease numbers cannot start with zero, but prune them anyway.
                while (*p1 == '0' && *(p1 + 1) != '\0') p1++;
                while (*p2 == '0' && *(p2 + 1) != '\0') p2++;

                len1 = strlen(p1);
                len2 = strlen(p2);
                if (len1 < len2) return TAIL_CMP_LT;
                if (len1 > len2) return TAIL_CMP_GT;
                cmp = strcmp(p1, p2);
                if (cmp < 0) return TAIL_CMP_LT;
                if (cmp > 0) return TAIL_CMP_GT;
            } else if (l_numeric) {
                return TAIL_CMP_LT;
            } else if (r_numeric) {
                return TAIL_CMP_GT;
            } else {
                int cmp = strcmp(l_token, r_token);
                if (cmp) return cmp > 0 ? TAIL_CMP_GT : TAIL_CMP_LT;
            }
        } else if (l_token) {
            return TAIL_CMP_GT;
        } else if (r_token) {
            return TAIL_CMP_LT;
        }

        l_token = strtok_r(NULL, dot, &l_last);
        r_token = strtok_r(NULL, dot, &r_last);
    }

    return TAIL_CMP_EQ;
}

int prerelcmp(const char* a, const char* b) {
    int res;
    char *ac, *bc;

    if (*a == '\0' && *b == '\0') {
        return 0;
    }

    ac = strip_meta(a);
    bc = strip_meta(b);
    if (*ac == '\0' && *bc != '\0') {
        res = 1;
    } else if (*ac != '\0' && *bc == '\0') {
        res = -1;
    } else {
        res = tail_cmp(ac, bc);
    }
    pfree(ac);
    pfree(bc);
    return res;
}

/* comparisons */
int _semver_cmp(semver* a, semver* b) {
    int i;
    if (IS_SEMVER2(a) && IS_SEMVER2(b)) {
        const char *a_parts[3], *b_parts[3];
        uint8 a_lens[3], b_lens[3];
        const char *a_prerel, *b_prerel;

        get_semver2_components((const semver2 *) a, &a_parts[0], &a_lens[0], &a_parts[1], &a_lens[1], &a_parts[2], &a_lens[2], &a_prerel);
        get_semver2_components((const semver2 *) b, &b_parts[0], &b_lens[0], &b_parts[1], &b_lens[1], &b_parts[2], &b_lens[2], &b_prerel);

        for (i = 0; i < 3; i++) {
            int cmp;
            if (a_lens[i] < b_lens[i]) return -1;
            if (a_lens[i] > b_lens[i]) return 1;
            cmp = strncmp(a_parts[i], b_parts[i], a_lens[i]);
            if (cmp != 0) return cmp < 0 ? -1 : 1;
        }
        return prerelcmp(a_prerel, b_prerel);
    } else if (!IS_SEMVER2(a) && !IS_SEMVER2(b)) {
        for (i = 0; i < 3; i++) {
            int a_x = a->v1.numbers[i];
            int b_x = b->v1.numbers[i];
            if (a_x < b_x) return -1;
            if (a_x > b_x) return 1;
        }
        return prerelcmp(a->v1.prerel, b->v1.prerel);
    } else if (!IS_SEMVER2(a) && IS_SEMVER2(b)) {
        /* a is v1, b is v2 */
        const char *b_parts[3];
        uint8 b_lens[3];
        const char *b_prerel;
        get_semver2_components((const semver2 *) b, &b_parts[0], &b_lens[0], &b_parts[1], &b_lens[1], &b_parts[2], &b_lens[2], &b_prerel);

        for (i = 0; i < 3; i++) {
            int cmp = cmp_v1_num_vs_v2_str(a->v1.numbers[i], b_parts[i], b_lens[i]);
            if (cmp != 0) return cmp;
        }
        return prerelcmp(a->v1.prerel, b_prerel);
    } else {
        /* a is v2, b is v1 */
        const char *a_parts[3];
        uint8 a_lens[3];
        const char *a_prerel;
        get_semver2_components((const semver2 *) a, &a_parts[0], &a_lens[0], &a_parts[1], &a_lens[1], &a_parts[2], &a_lens[2], &a_prerel);

        for (i = 0; i < 3; i++) {
            int cmp = cmp_v1_num_vs_v2_str(b->v1.numbers[i], a_parts[i], a_lens[i]);
            if (cmp != 0) return -cmp;
        }
        return prerelcmp(a_prerel, b->v1.prerel);
    }
}

PG_FUNCTION_INFO_V1(semver_eq);
Datum
semver_eq(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff == 0);
}

PG_FUNCTION_INFO_V1(semver_ne);
Datum
semver_ne(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff != 0);
}

PG_FUNCTION_INFO_V1(semver_le);
Datum
semver_le(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff <= 0);
}

PG_FUNCTION_INFO_V1(semver_lt);
Datum
semver_lt(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff < 0);
}

PG_FUNCTION_INFO_V1(semver_ge);
Datum
semver_ge(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff >= 0);
}

PG_FUNCTION_INFO_V1(semver_gt);
Datum
semver_gt(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_BOOL(diff > 0);
}

PG_FUNCTION_INFO_V1(semver_cmp);
Datum
semver_cmp(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    PG_FREE_IF_COPY(a, 0);
    PG_FREE_IF_COPY(b, 1);
    PG_RETURN_INT32(diff);
}

/* so the '=' function can be 'hashes' */
PG_FUNCTION_INFO_V1(hash_semver);
Datum
hash_semver(PG_FUNCTION_ARGS) {
    semver* version = PG_GETARG_SEMVER_P(0);
    uint32 hash = 0;
    int i;
    int64 val;
    Datum prerel;

    if (IS_SEMVER2(version)) {
        const char *parts[3];
        uint8 lens[3];
        const char *prerel_raw;
        get_semver2_components((const semver2 *) version, &parts[0], &lens[0], &parts[1], &lens[1], &parts[2], &lens[2], &prerel_raw);

        if (*prerel_raw != '\0') {
            char *prerel_clean = strip_meta(prerel_raw);
            if (*prerel_clean != '\0') {
                prerel = CStringGetTextDatum(prerel_clean);
                hash = DirectFunctionCall1Coll(hashtext, C_COLLATION_OID, prerel);
                pfree(DatumGetPointer(prerel));
            }
            pfree(prerel_clean);
        }
        for (i = 0; i < 3; i++) {
            hash = (hash << (7 + (i << 1))) | (hash >> (25 - (i << 1)));
            if (parse_comp_to_int64(parts[i], lens[i], &val) && val <= INT32_MAX) {
                hash ^= DirectFunctionCall1(hashint4, (int32) val);
            } else {
                hash ^= DatumGetInt32(hash_bytes((const unsigned char *) parts[i], (int) lens[i]));
            }
        }
    } else {
        if (*version->v1.prerel != '\0') {
            char *prerel_clean = strip_meta(version->v1.prerel);
            if (*prerel_clean != '\0') {
                prerel = CStringGetTextDatum(prerel_clean);
                hash = DirectFunctionCall1Coll(hashtext, C_COLLATION_OID, prerel);
                pfree(DatumGetPointer(prerel));
            }
            pfree(prerel_clean);
        }
        for (i = 0; i < 3; i++) {
            hash = (hash << (7 + (i << 1))) | (hash >> (25 - (i << 1)));
            hash ^= DirectFunctionCall1(hashint4, version->v1.numbers[i]);
        }
    }

    PG_FREE_IF_COPY(version, 0);
    PG_RETURN_INT32(hash);
}

PG_FUNCTION_INFO_V1(hash_semver_extended);
Datum
hash_semver_extended(PG_FUNCTION_ARGS) {
    semver* version = PG_GETARG_SEMVER_P(0);
    uint64 seed = PG_GETARG_INT64(1);
    uint64 hash = 0;
    int i;
    int64 val;

    if (IS_SEMVER2(version)) {
        const char *parts[3];
        uint8 lens[3];
        const char *prerel_raw;
        get_semver2_components((const semver2 *) version, &parts[0], &lens[0], &parts[1], &lens[1], &parts[2], &lens[2], &prerel_raw);

        if (*prerel_raw != '\0') {
            char *prerel_clean = strip_meta(prerel_raw);
            if (*prerel_clean != '\0') {
                hash = DatumGetUInt64(hash_any_extended((const unsigned char *) prerel_clean,
                                                        strlen(prerel_clean), seed));
            }
            pfree(prerel_clean);
        }
        for (i = 0; i < 3; i++) {
            hash = ROTATE_HIGH_AND_LOW_32BITS(hash);
            if (parse_comp_to_int64(parts[i], lens[i], &val) && val <= INT32_MAX) {
                hash ^= DatumGetUInt64(hash_uint32_extended((uint32) val, seed));
            } else {
                hash ^= DatumGetUInt64(hash_any_extended((const unsigned char *) parts[i], (int) lens[i], seed));
            }
        }
    } else {
        if (*version->v1.prerel != '\0') {
            char *prerel_clean = strip_meta(version->v1.prerel);
            if (*prerel_clean != '\0') {
                hash = DatumGetUInt64(hash_any_extended((const unsigned char *) prerel_clean,
                                                        strlen(prerel_clean), seed));
            }
            pfree(prerel_clean);
        }
        for (i = 0; i < 3; i++) {
            hash = ROTATE_HIGH_AND_LOW_32BITS(hash);
            hash ^= DatumGetUInt64(hash_uint32_extended((uint32) version->v1.numbers[i], seed));
        }
    }

    PG_FREE_IF_COPY(version, 0);
    PG_RETURN_UINT64(hash);
}

PG_FUNCTION_INFO_V1(semver_larger);
Datum
semver_larger(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    if (diff >= 0) {
        PG_FREE_IF_COPY(b, 1);
        PG_RETURN_POINTER(a);
    }
    PG_FREE_IF_COPY(a, 0);
    PG_RETURN_POINTER(b);
}

PG_FUNCTION_INFO_V1(semver_smaller);
Datum
semver_smaller(PG_FUNCTION_ARGS) {
    semver* a = PG_GETARG_SEMVER_P(0);
    semver* b = PG_GETARG_SEMVER_P(1);
    int diff = _semver_cmp(a, b);
    if (diff <= 0) {
        PG_FREE_IF_COPY(b, 1);
        PG_RETURN_POINTER(a);
    }
    PG_FREE_IF_COPY(a, 0);
    PG_RETURN_POINTER(b);
}

PG_FUNCTION_INFO_V1(to_semver);
Datum
to_semver(PG_FUNCTION_ARGS) {
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    char *str = text_to_cstring(sv);
    semver* rs;

    if (strlen(str) != (size_t) VARSIZE_ANY_EXHDR(sv)) {
        pfree(str);
        PG_FREE_IF_COPY(sv, 0);
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                 errmsg("invalid input syntax for type semver: contains null character")));
    }

    rs = parse_semver(str, true, true, &bad);
    pfree(str);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(is_semver);
Datum
is_semver(PG_FUNCTION_ARGS) {
    text* sv = PG_GETARG_TEXT_PP(0);
    bool bad = false;
    char *str = text_to_cstring(sv);
    semver* rs;

    if (strlen(str) != (size_t) VARSIZE_ANY_EXHDR(sv)) {
        pfree(str);
        PG_FREE_IF_COPY(sv, 0);
        PG_RETURN_BOOL(false);
    }

    rs = parse_semver(str, false, false, &bad);
    if (rs != NULL) pfree(rs);
    pfree(str);
    PG_FREE_IF_COPY(sv, 0);
    PG_RETURN_BOOL(!bad);
}

PG_FUNCTION_INFO_V1(numeric_to_semver);
Datum
numeric_to_semver(PG_FUNCTION_ARGS) {
    Numeric n = PG_GETARG_NUMERIC(0);
    Datum d = NumericGetDatum(n);
    char* t = DatumGetCString(DirectFunctionCall1(numeric_out, d));
    bool bad = false;
    semver* rs;
    if (
#if PG_VERSION_NUM >= 140000
        numeric_is_inf(n) ||
#endif
        numeric_is_nan(n)) {
        ereport(ERROR,
                (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                 errmsg("cannot convert \"%s\" to semver", t)));
    }
    rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_FREE_IF_COPY(n, 0);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(real_to_semver);
Datum
real_to_semver(PG_FUNCTION_ARGS) {
    float4 f = PG_GETARG_FLOAT4(0);
    Datum d = Float4GetDatum(f);
    char* t = DatumGetCString(DirectFunctionCall1(float4out, d));
    bool bad = false;
    semver* rs;
    if (isinf(f) || isnan(f)) {
        ereport(ERROR,
                (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                 errmsg("cannot convert \"%s\" to semver", t)));
    }
    rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(double_to_semver);
Datum
double_to_semver(PG_FUNCTION_ARGS) {
    float8 f = PG_GETARG_FLOAT8(0);
    Datum d = Float8GetDatum(f);
    char* t = DatumGetCString(DirectFunctionCall1(float8out, d));
    bool bad = false;
    semver* rs;
    if (isinf(f) || isnan(f)) {
        ereport(ERROR,
                (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                 errmsg("cannot convert \"%s\" to semver", t)));
    }
    rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(int_to_semver);
Datum
int_to_semver(PG_FUNCTION_ARGS) {
    Datum n = Int32GetDatum(PG_GETARG_INT32(0));
    char* t = DatumGetCString(DirectFunctionCall1(int4out, n));
    bool bad = false;
    semver* rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(smallint_to_semver);
Datum
smallint_to_semver(PG_FUNCTION_ARGS) {
    Datum n = Int16GetDatum(PG_GETARG_INT16(0));
    char* t = DatumGetCString(DirectFunctionCall1(int2out, n));
    bool bad = false;
    semver* rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(bigint_to_semver);
Datum
bigint_to_semver(PG_FUNCTION_ARGS) {
    Datum n = Int64GetDatum(PG_GETARG_INT64(0));
    char* t = DatumGetCString(DirectFunctionCall1(int8out, n));
    bool bad = false;
    semver* rs = parse_semver(t, true, true, &bad);
    pfree(t);
    PG_RETURN_POINTER(rs);
}

/* constructor for testing legacy v1 format */
PG_FUNCTION_INFO_V1(to_semver1);
Datum
to_semver1(PG_FUNCTION_ARGS) {
    int32 major;
    int32 minor;
    int32 patch;
    char *prerel = "";
    text *prerel_txt = NULL;
    int nums[3];
    semver *res;

    if (PG_ARGISNULL(0) || PG_ARGISNULL(1) || PG_ARGISNULL(2))
        PG_RETURN_NULL();

    major = PG_GETARG_INT32(0);
    minor = PG_GETARG_INT32(1);
    patch = PG_GETARG_INT32(2);

    if (major < 0 || minor < 0 || patch < 0) {
        ereport(ERROR,
                (errcode(ERRCODE_INVALID_PARAMETER_VALUE),
                 errmsg("semver version numbers cannot be negative")));
    }

    if (PG_NARGS() > 3 && !PG_ARGISNULL(3)) {
        prerel_txt = PG_GETARG_TEXT_PP(3);
        prerel = text_to_cstring(prerel_txt);
        if (strlen(prerel) != (size_t) VARSIZE_ANY_EXHDR(prerel_txt)) {
            pfree(prerel);
            PG_FREE_IF_COPY(prerel_txt, 3);
            ereport(ERROR,
                    (errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
                     errmsg("invalid input syntax for type semver: contains null character")));
        }
    }

    nums[0] = major;
    nums[1] = minor;
    nums[2] = patch;
    res = make_semver(nums, prerel);
    if (PG_NARGS() > 3 && !PG_ARGISNULL(3)) {
        pfree(prerel);
        PG_FREE_IF_COPY(prerel_txt, 3);
    }
    PG_RETURN_POINTER(res);
}
