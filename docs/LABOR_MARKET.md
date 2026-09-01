# Labor-market microdata

## Scope

The labor-market module is source-first and keeps resident-worker outcomes separate from establishment-location outcomes in the Economic Census. The first active source is NSS 64 Schedule 10.2 (July 2007–June 2008), which the official NSS catalog identifies as the Employment, Unemployment and Migration Survey.

## NSS 64 source contract

The active extended source contract uses three official files under `data/raw/nss/NSS 2007-08 Employment, Unemployment and Migration Survey 64th Round/`:

- `survey0/data/ddi.xml` for machine-readable file/schema metadata;
- `Block-4-demographic-usual-activity-members.sav` for demographic and usual-principal/subsidiary activity records;
- `Block-6-members-migration-records.sav` for member migration and temporary-away particulars.

The DDI reports 572,254 cases in both Block 4 and Block 6. Production ingestion therefore requires a complete one-to-one person-key universe across those two blocks. It also requires the common NSS design fields (sector, sub-round, sub-sample, NSS region, stratum, sub-stratum, FSU, second-stage stratum) and the combined multiplier `wgt_combined` to be present, with positive finite weights.

Block 4 exposes age, sex, education, usual principal activity status, NIC-2004, NCO-2004, and subsidiary-activity fields. Block 6 exposes temporary absence, migration-status/history fields, last usual place of residence, reason for leaving, and usual principal activity fields. These are source variables only at this stage; no district labor outcome is registered yet.

## Geography and inference boundary

NSS 64 uses two-digit state and district source codes. The source reader preserves those codes and the full survey design, but this patch does not assume that every survey district is a one-to-one Census-2001 district. District outcomes will only be activated after the source districts are passed through the existing reviewed lineage machinery and after effective sample/support diagnostics are defined.

The next labor patch should predeclare a small person-level outcome family and its support rules before running district regressions. Candidate concepts are labor-force participation, employment/unemployment, regular salaried work, self/casual employment composition, occupation/industry structure, and migration. Wage outcomes require the weekly-status/earnings block rather than Block 4 and are intentionally deferred until that source contract is inspected.
