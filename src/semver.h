#ifndef SEMVER_H
#define SEMVER_H

#include "postgres.h"

/* heap format of version numbers for legacy v1 */
typedef int32 vernum;

/* legacy v1 memory/heap structure (read-only for existing tuples) */
typedef struct semver1 {
    int32  vl_len_;  /* varlena header */
    vernum numbers[3];
    char   prerel[]; /* pre-release, including the null byte for convenience */
} semver1;

/* new v2 textual memory/heap structure */
typedef struct semver2 {
    int32  vl_len_;  /* varlena header */
    uint8  tag;
    uint8  len_major;
    uint8  len_minor;
    uint8  len_patch;
    char   verstring[]; /* full canonical null-terminated version string */
} semver2;

/* generic memory/heap structure */
typedef struct semver {
    int32  vl_len_;  /* varlena header */
    union {
        struct {
            vernum numbers[3];
            char   prerel[];
        } v1;
        struct {
            uint8  tag;
            uint8  len_major;
            uint8  len_minor;
            uint8  len_patch;
            char   verstring[];
        } v2;
    };
} semver;

#define SEMVER_DISCRIMINATOR_BIT 0x80
#define SEMVER2_FORMAT_VERSION   1
#define SEMVER2_FLAG_PREREL      0x10
#define SEMVER2_FLAG_BUILD       0x20

#define IS_SEMVER2(sv) (*((const int32 *) ((const char *)(sv) + VARHDRSZ)) < 0)

#define PG_GETARG_SEMVER_P(n) ((semver *) PG_DETOAST_DATUM(PG_GETARG_DATUM(n)))

char*   emit_semver(semver* version);
semver* make_semver(const int *numbers, const char* prerel);
semver* make_semver2(const char *verstr, uint8 len_major, uint8 len_minor, uint8 len_patch, bool has_prerel, bool has_build);
semver* parse_semver(char* str, bool lax, bool throw, bool *bad);

#endif /* SEMVER_H */
