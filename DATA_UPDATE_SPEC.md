# Data Update Refactor Specification

## Overview

Update shinyppg app to support the new PPG data format that uses WFO taxon
IDs and has a modified column structure.

## Background

The PPG data has been updated with the following key changes:

### Column Changes

**Removed columns:**
- `acceptedNameUsage` (can be generated)
- `parentNameUsage` (can be generated)
- `ipniURL`
- `modifiedBy`
- `modifiedByID`

**Added columns:**
- `scientificNameAuthorship`
- `nomenclaturalStatus`
- `created`

**Retained columns:**
- `taxonID`
- `scientificName`
- `taxonRank`
- `parentNameUsageID`
- `namePublishedIn`
- `taxonomicStatus`
- `acceptedNameUsageID`
- `modified`
- `taxonRemarks` (assumed still present)

### Example Data

**Old format:**
```csv
taxonID,scientificName,taxonRank,taxonomicStatus,acceptedNameUsage,
acceptedNameUsageID,parentNameUsage,parentNameUsageID,namePublishedIn,
taxonRemarks,ipniURL,modified,modifiedBy,modifiedByID
efa93c7765b8d86f47bae8bed1d70c76,Abacopteris Fée,genus,accepted,NA,NA,
Thelypteroideae C. F. Reed,5b100167cb6ff292a1ca5b9b8e28e4d0,
Congr. Sci. France 10(1): 178. 1843 ,
"Distribution: continental SE-Asia, Malesia, Australia, ?Africa",
https://www.ipni.org/n/17000000-1,NA,NA,NA
```

**New format:**
```csv
taxonID,scientificName,scientificNameAuthorship,taxonRank,
parentNameUsageID,nomenclaturalStatus,namePublishedIn,taxonomicStatus,
acceptedNameUsageID,created,modified
wfo-0001315027,Leucotrichum madagascariense,
Rakotondr. & Rouhan,species,wfo-4000021620,valid,
"Syst. Bot. 37(2): 335, f.1-5 (2012)",accepted,NA,2022-04-16,2025-03-19
```

## Technical Approach

### Data Generation

Missing `acceptedNameUsage` and `parentNameUsage` columns will be generated
using the `dwctaxon` R package:

```r
# Fill acceptedNameUsage
data <- dct_fill_col(
  data,
  fill_to = "acceptedNameUsage",
  fill_from = "scientificName",
  match_to = "taxonID",
  match_from = "acceptedNameUsageID"
)

# Fill parentNameUsage
data <- dct_fill_col(
  data,
  fill_to = "parentNameUsage",
  fill_from = "scientificName",
  match_to = "taxonID",
  match_from = "parentNameUsageID"
)
```

## Required Changes

### 1. Data Processing Scripts

**File:** [data-raw/ppg_small.R](data-raw/ppg_small.R)

- Update data loading to work with new column structure
- Add calls to `dct_fill_col()` to generate missing `acceptedNameUsage` and
  `parentNameUsage` columns
- Remove references to `modifiedBy` and `modifiedByID` (no longer needed)
- Update validation checks if needed for new column structure
- Ensure new columns (`scientificNameAuthorship`, `nomenclaturalStatus`,
  `created`) are preserved

### 2. Column Definitions

**File:** [R/startup.R](R/startup.R)

- Update `cols_select` vector:
  - Remove: `"modifiedBy"`, `"modifiedByID"`, `"ipniURL"`
  - Add: `"scientificNameAuthorship"`, `"nomenclaturalStatus"`, `"created"`
  - Keep: `"acceptedNameUsage"`, `"parentNameUsage"` (these will be
    generated)

### 3. Documentation

**File:** [inst/doc.md](inst/doc.md)

- Update column descriptions to reflect new data structure
- Document that `acceptedNameUsage` and `parentNameUsage` are now
  generated fields
- Remove documentation for `ipniURL`, `modifiedBy`, `modifiedByID`
- Add documentation for `scientificNameAuthorship`, `nomenclaturalStatus`,
  `created`

### 4. Functions

**Files to review:**
- [R/functions.R](R/functions.R)
- [R/load_display_ppg.R](R/load_display_ppg.R)
- [R/autocomp.R](R/autocomp.R)
- [R/subset.R](R/subset.R)

**Actions:**
- Review all references to removed columns (`ipniURL`, `modifiedBy`,
  `modifiedByID`)
- Remove or adapt code that references removed columns
- Ensure code works with generated `acceptedNameUsage` and
  `parentNameUsage` columns
- Add support for displaying new columns if appropriate

### 5. Tests

**File:** [tests/testthat/test-functions.R](tests/testthat/test-functions.R)

- Update test expectations to match new column structure
- Update snapshots in
  [tests/testthat/_snaps/functions.md](tests/testthat/_snaps/functions.md)
- Ensure tests pass with generated columns

### 6. Test Data

**Files:**
- [data/ppg_small.rda](data/ppg_small.rda)
- Other data files as needed

**Actions:**
- Regenerate `ppg_small.rda` with new data structure
- Verify data includes all necessary columns after generation

## Implementation Order

1. **Update data processing script** (ppg_small.R)
   - Add `dct_fill_col()` calls to generate missing columns
   - Remove obsolete column handling
   - Regenerate test data

2. **Update column definitions** (startup.R)
   - Modify `cols_select` to match new structure

3. **Review and update functions** (functions.R, load_display_ppg.R, etc.)
   - Remove references to deleted columns
   - Test with new data structure

4. **Update documentation** (inst/doc.md)
   - Document new columns
   - Remove obsolete column docs

5. **Update tests**
   - Fix test expectations
   - Regenerate snapshots
   - Verify all tests pass

6. **Verify app functionality**
   - Test all features with new data
   - Ensure UI displays correctly
   - Check filtering, searching, and editing features

## Validation Criteria

- [ ] App loads successfully with new data format
- [ ] All tests pass
- [ ] `acceptedNameUsage` and `parentNameUsage` are correctly generated
- [ ] UI displays all appropriate columns
- [ ] Filtering and searching work correctly
- [ ] Editing functionality works with new columns
- [ ] No references to removed columns remain in code
- [ ] Documentation is updated and accurate
- [ ] New columns are properly handled throughout the app

## Notes

- The `dwctaxon` package must be available as a dependency
- Consider whether `created` column should be displayed or used for any
  features
- Verify that `nomenclaturalStatus` values are valid and properly handled
- Check if `scientificNameAuthorship` should be displayed in any views
