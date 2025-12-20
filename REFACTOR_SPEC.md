# PPG App Refactor Specification: Editor → Viewer

## Overview

**Goal**: Convert the shinyppg application from a taxonomic database editor to
a read-only viewer.

**Rationale**: The project's original purpose was to provide a Shiny-based
interface for users to log in and edit a taxonomic database. The editing
workflow has since moved to a different tool, but users still need to inspect
and explore the data. This refactor removes editing capabilities while
preserving all viewing, browsing, and inspection features.

## Scope

### Remove (Editing Functionality)
- Authentication/login system
- Data entry (add rows)
- Data modification (modify rows)
- Data deletion (delete rows)
- Undo functionality
- Git synchronization and session management
- Settings related to editing
- Validation (since no edits are being made)

### Keep (Viewing Functionality)
- Data display table
- Search and filter capabilities
- Data subsetting/browsing
- Name composition (as a query helper, not for editing)
- User guide documentation
- Core data loading and display functions

### Simplify
- Remove tabbed interface complexity
- Streamline UI to focus on data exploration
- Remove authentication dependencies

---

## Detailed Implementation Plan

### Phase 1: Remove Authentication

**Files to modify:**
- `R/ppg_app.R` - Remove login/logout UI and server logic
- `R/auth.R` - Mark as deprecated or remove entirely

**Changes:**
- Remove `shinyauthr::loginUI()` and `shinyauthr::logoutUI()`
- Remove `credentials` reactive and all references to it
- Remove user authentication checks (`shiny::req(credentials()$user_auth)`)
- Remove user-based configuration in `dwctaxon::dct_options()`

### Phase 2: Remove Editing Modules

**Files to remove:**
- `R/add_row.R` - Add row functionality
- `R/modify_row.R` - Modify row functionality
- `R/delete_row.R` - Delete row functionality
- `R/undo.R` - Undo functionality
- `R/validate.R` - Validation functionality
- `R/sync.R` - Git sync functionality
- `R/settings.R` - Settings module
- `R/git.R` - Git operations

**Corresponding documentation to remove:**
- `man/` files for removed functions

**Changes to `R/ppg_app.R`:**
- Remove all server module calls for:
  - `add_row_server()`
  - `modify_row_server()`
  - `delete_row_server()`
  - `undo_server()`
  - `validate_server()`
  - `sync_server()`
  - `settings_server()`
- Remove all UI elements for these modules
- Remove reactive values used only for editing:
  - `composed_name_add`
  - `composed_name_modify`
  - `show_advanced`
  - `cols_fill`

### Phase 3: Simplify UI

**Files to modify:**
- `R/ppg_app.R` - Simplify UI structure

**Changes:**
- Change title from "PPG Editor" to "PPG Viewer" or "PPG Browser"
- Remove tabbed interface (`tabsetPanel`)
- Create single-page layout focusing on:
  - Data display table (keep `display_ppg_ui`)
  - Search/filter panel (keep relevant parts of `subset_ui`)
  - Optional: Name composition as query helper (evaluate if needed)
- Keep User Guide tab or convert to modal/sidebar

### Phase 4: Refactor Data Loading

**Files to keep and simplify:**
- `R/data.R` - Data loading functions
- `R/load_display_ppg.R` - Display logic
- `R/subset.R` - Subset/filter functionality
- `R/functions.R` - Utility functions (evaluate which are needed)
- `R/compose_name.R` - Evaluate if useful as query helper

**Changes:**
- Simplify `subset_server()` to remove editing-related logic
- Remove git-related data operations
- Make `ppg` reactive value read-only (no updates)
- Simplify `compose_name_server()` if kept, removing editing logic

### Phase 5: Update Dependencies

**Files to modify:**
- `DESCRIPTION` - Remove unused dependencies
- `NAMESPACE` - Update exports

**Dependencies to remove:**
- `shinyauthr` (authentication)
- Any git-related packages
- `dwctaxon` validation features (if only used for editing)

**Dependencies to keep:**
- `shiny` (core framework)
- Data manipulation packages
- Display/table packages

### Phase 6: Update Documentation

**Files to modify:**
- `README.md` - Update project description
- `inst/doc.md` - Update user guide
- `R/ppg_app.R` - Update function documentation

**Changes:**
- Update all references from "edit" to "view/browse"
- Remove authentication instructions
- Simplify usage documentation
- Update examples

### Phase 7: Testing and Cleanup

**Files to modify:**
- `tests/testthat/test-*.R` - Remove tests for deleted functions

**Tasks:**
- Remove obsolete test files
- Add/update tests for viewer functionality
- Test the simplified app end-to-end
- Remove any orphaned files

---

## Implementation Order

1. ✅ Create this specification document
2. ✅ Remove authentication system (Phase 1)
3. ✅ Remove editing modules and their calls (Phase 2)
4. ✅ Simplify and redesign UI (Phase 3)
5. ✅ Refactor data loading and display (Phase 4)
6. ✅ Update dependencies (Phase 5)
7. ✅ Update documentation (Phase 6)
8. ✅ Testing and cleanup (Phase 7)

---

## Success Criteria

- [ ] App launches without authentication
- [ ] Users can view the complete PPG taxonomic database
- [ ] Users can search and filter the data
- [ ] Users can browse different subsets of data
- [ ] No editing functionality remains
- [ ] No git/sync functionality remains
- [ ] Documentation accurately reflects viewer-only functionality
- [ ] All obsolete code and dependencies removed
- [ ] Simplified, maintainable codebase

---

## Notes

- Consider keeping `compose_name.R` as a query helper if users find it useful
  for constructing searches
- The data files in `data/` and `data-raw/` should remain unchanged
- Focus on creating a clean, intuitive browsing experience
- Consider adding export/download functionality in future (out of scope for
  this refactor)

---

## Rollback Plan

- Work is being done on `db` branch
- Main branch remains unchanged until refactor is complete and tested
- Easy to abandon branch if needed
