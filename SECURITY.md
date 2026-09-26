# Security Policy

The semver project takes security issues seriously. This document outlines the
process for reporting security vulnerabilities, our contact channels, and our
expected response timelines.

---

## Supported Versions

Security fixes are actively provided for the following versions:

| Version   | Supported | Notes           |
| --------- | --------- | --------------- |
| `< 1.0.0` | ✅         | Active support |

The semver extension is supported and tested against PostgreSQL 9.2 and later.

---

## Reporting a Vulnerability

> [!IMPORTANT]
> **Do not report suspected security vulnerabilities through public GitHub
> issues, discussions, or pull requests.**

If you discover a security vulnerability in semver, please report it privately
by email to the project maintainers:

*   **David Wheeler**: `david@justatheory.com`

Please include all email addresses on the report to ensure prompt handling.

### What to Include in Your Report

To help us triage and resolve the issue quickly, please provide as much of the
following details as possible:

1.  **Vulnerability Summary**: A clear description of the potential
    vulnerability and its estimated impact (e.g., server crash/DoS,
    out-of-bounds read/write, memory corruption, arbitrary code execution,
    privilege escalation, or hash/index contract violation).
2.  **Affected Environment**:
    *  semver version or commit SHA
    *  PostgreSQL version(s) tested (e.g., PostgreSQL 16.4, 17.0)
    *  Platform / Operating System and architecture (e.g., Debian 12 x86_64,
       Linux ARM64)
3.  **Steps to Reproduce (PoC)**: A minimal, reproducible SQL script or
    reproduction instructions.
4.  **Proposed Fix**: Any suggested patch or remediation steps (if available).
5.  **Disclosure Preferences**: Let us know how you would like to be credited
    (name, handle, or anonymous) in release notes and security advisories once
    the issue is resolved.

---

## Expected Timelines & SLA

We follow a structured vulnerability handling process with the following
target timelines:

|            Stage            |                      Expected Timeline                       |                                                Action                                                |
| -------------------------- | ------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| **Initial Acknowledgment** | **Within 48 hours** (2 business days)                         | Maintainers acknowledge receipt of the report.                                                       |
| **Triage & Validation**    | **Within 5 business days**                                    | Maintainers confirm reproduction, assess severity (per CVSS v3.1), and determine scope of impact.    |
| **Status Updates**         | **At least every 7 days**                                     | Maintainers keep the reporter informed of investigation and patch development progress.              |
| **Fix Development**        | **14–30 days** (Critical/High)<br>**30–60 days** (Medium/Low) | A secure fix is developed, reviewed, and regression tested across all supported PostgreSQL versions. |
| **Release & Disclosure**   | **Coordinated (target <= 90 days)**                           | Patched release published on GitHub alongside a public security advisory.                            |

---

## Coordinated Disclosure Process

1.  **Private Handling**: The vulnerability report is investigated privately.
    Fixes and regression tests are developed in a private branch or fork.
2.  **Reporter Verification**: When feasible, a proposed patch will be shared
    with the reporter for confirmation prior to release.
3.  **Release & Public Disclosure**: A new release containing the fix will be
    published, and an advisory will be posted detailing the issue, severity,
    mitigation, and reporter attribution (if desired).
4.  **Safe Harbor**: We consider security research conducted in good faith
    according to this policy to be authorized. We will not pursue legal action
    against researchers who report vulnerabilities following these guidelines,
    avoid privacy violations, do not destroy data, and do not interrupt or
    degrade production services.
