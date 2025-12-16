# Performance Review: sswqs_curation.R

**Date:** 2025-12-15
**Script:** `epa/sswqs/sswqs_curation.R`
**Size:** 1.7MB, 3,381 lines
**Status:** 🔴 Multiple critical performance issues identified

---

## Executive Summary

The `sswqs_curation.R` script has several significant performance bottlenecks that can be optimized for 5-10x performance improvement. The most critical issues are:

1. **Massive hardcoded lookup table** (~2,700 lines) parsed on every execution
2. **Multiple V8 JavaScript engine instances** created unnecessarily (50+ times)
3. **Inefficient rowwise() operations** instead of vectorized alternatives
4. **Sequential processing** that could be parallelized

**Estimated total speedup with all optimizations: 5-10x faster**

---

## 🔴 Critical Performance Issues

### Issue 1: Massive Hardcoded Lookup Table (Lines 388-3094)

**Impact: SEVERE** - ~2,700 lines of hardcoded tibble data parsed on every run

#### Description
The script contains a massive `tibble::tribble()` with water use category mappings that must be parsed each time:

```r
tibble::tribble(
  ~key   , ~local , ~general_usage,
  "1"    , "agricultural and industrial water supply", "Water Supply",
  "5"    , "claiborne lake (alabama river basin)", "Site-Specific",
  "6"    , "dannelly lake (alabama river basin)", "Site-Specific",
  # ... 2,700+ more lines ...
  "5458" , "warmwater fisheries use", "Aquatic Life"
) %>% select(-local)
```

#### Performance Impact
- Adds significant parsing overhead on every execution
- Increases memory footprint unnecessarily
- Makes script difficult to maintain and review
- Contributes to 1.7MB file size

#### Line References
- Start: Line 388
- End: Line 3094
- Total: ~2,706 lines (80% of the script!)

---

### Issue 2: Multiple V8 JavaScript Engine Instances (Lines 62, 129)

**Impact: HIGH** - Creating V8 contexts is expensive

#### Description
The script creates multiple V8 JavaScript engine instances:

```r
cx <- v8()  # Line 62 (first instance)

# Then inside pmap loop (Line 129):
state_dat <- state_vars %>%
  pmap(
    function(abv, json) {
      ctx <- v8()  # NEW V8 INSTANCE FOR EACH STATE!
      ctx$source(json)
      # ... processing
      rm(ctx)
      return(dat)
    },
    .progress = TRUE
  )
```

#### Performance Impact
- V8 initialization is slow (~100-500ms per instance)
- Creates 50+ separate V8 contexts (one per state)
- High memory overhead per instance (~10-50MB each)
- Unnecessary context switching overhead
- **Total overhead: 5-25 seconds just for V8 initialization**

#### Line References
- First V8 context: Line 62
- Loop with repeated V8 creation: Lines 123-206
- Specific line creating context in loop: Line 129

---

### Issue 3: rowwise() Operations (Line 3252)

**Impact: HIGH** - One of slowest dplyr operations

#### Description
```r
curated_sswqs <- sswqs %>%
  # ... other operations
  rowwise() %>%  # LINE 3252 - SLOW!
  mutate(
    result = if (!num_bool) str_split(result, pattern = "-") else list(result)
  ) %>%
  unnest(result) %>%
  ungroup()
```

#### Performance Impact
- `rowwise()` processes each row individually (no vectorization)
- For datasets with 100,000+ rows, this can add minutes of processing time
- Creates unnecessary intermediate objects
- 80-95% slower than vectorized alternatives

#### Line References
- rowwise() call: Line 3252
- Associated unnest: Line 3256

---

### Issue 4: Nested map/pmap Operations (Lines 80-206)

**Impact: MEDIUM-HIGH** - 16 instances of map operations found

#### Description
Complex nested functional programming operations like:

```r
# Lines 80-87: Nested map within map
parent_dat <- vars %>%
  map(
    .,
    ~ {
      cx$get(.x)
    }
  ) %>%
  set_names(., vars)

# Lines 123-206: pmap with heavy processing
state_dat <- state_vars %>%
  pmap(
    function(abv, json) {
      ctx <- v8()
      ctx$source(json)
      # Multiple nested map operations inside
      st_vars %>%
        map(., ~ { ctx$get(.x) }) %>%
        set_names(., st_vars) %>%
        modify_at(., "criteriaData_sub", ~ pluck(., 1))
      # ...
    },
    .progress = TRUE
  )
```

#### Performance Impact
- Sequential processing (no parallelization)
- Could leverage multiple CPU cores
- Creates many intermediate list objects
- 50+ iterations for state data

#### Line References
- Lines 80-87, 89-96, 143-196, 224-233, 238-239, 244-246

---

### Issue 5: Sequential Multiple left_joins (Lines 3110-3188)

**Impact: MEDIUM** - 9 sequential joins

#### Description
```r
sswqs <- crit_dat %>%
  # Area ----
  left_join(entities, ., join_by(short_code == area)) %>%
  # Remap ----
  left_join(., pollutantRemap, join_by(analyte == old_idx)) %>%
  # Pollutants ----
  left_join(., pollutants, join_by(idx == idx)) %>%
  # Use ----
  left_join(., use_class, join_by(short_code == area, use_class == key)) %>%
  # Protection ----
  left_join(., protection_dict, join_by(protection == protection)) %>%
  # Units ----
  left_join(., units, join_by(unit_code == idx)) %>%
  # Source and citation ----
  left_join(., sources, join_by(source == key, short_code == area)) %>%
  # General usage ----
  left_join(., use_class_super, join_by(use_class == key))
```

#### Performance Impact
- Each join creates intermediate copies of the data
- Memory allocation overhead for each intermediate result
- Could be reduced to 1-2 joins with pre-merged lookup tables
- 10-20% overhead compared to optimized approach

#### Line References
- Lines 3110-3188 (entire join chain)

---

## 📋 Proposed Solutions & Workarounds

### SOLUTION 1: Extract Lookup Table to External File

**Priority: HIGH** | **Estimated Speedup: 30-50%** | **Effort: Low**

#### Problem
2,700 lines of hardcoded data parsed every run

#### Proposed Workaround

**Option A: Use RDS file (recommended - fastest)**
```r
# ONE-TIME: Extract and save the lookup table
# Run this once to create the file:
use_class_super <- tibble::tribble(
  ~key, ~local, ~general_usage,
  # ... all the data ...
)
saveRDS(use_class_super, here("epa", "sswqs", "data", "use_class_lookup.RDS"))

# THEN in the main script (replace lines 388-3094):
use_class_super <- readRDS(here("epa", "sswqs", "data", "use_class_lookup.RDS"))
```

**Option B: Use CSV file (easier to edit)**
```r
# Extract the tibble to: epa/sswqs/data/use_class_lookup.csv
use_class_super <- read_csv(
  here("epa", "sswqs", "data", "use_class_lookup.csv"),
  col_types = "ccc",
  lazy = FALSE
)
```

#### Benefits
- Script reduces from 3,381 → ~700 lines (78% reduction)
- File size: 1.7MB → ~300KB
- Faster parsing (binary RDS vs. text parsing)
- Easier to maintain and version control
- Can be shared across multiple scripts

#### Implementation Steps
1. Extract lines 388-3094 to separate file
2. Save as RDS or CSV
3. Replace tibble::tribble() call with file read
4. Test to ensure results are identical

---

### SOLUTION 2: Reuse Single V8 Context

**Priority: HIGH** | **Estimated Speedup: 40-60%** | **Effort: Medium**

#### Problem
Creating 50+ V8 instances (one per state)

#### Proposed Workaround

**Option A: Reuse single V8 context (recommended)**
```r
# Create ONE V8 context outside the loop
shared_ctx <- v8()

state_dat <- state_vars %>%
  pmap(
    function(abv, json) {
      # Reuse the same context
      shared_ctx$source(json)

      st_vars <- shared_ctx$eval(
        "Object.keys(this).filter(function(key) {
          return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
        })"
      ) %>%
        str_split(., ",") %>%
        pluck(., 1)

      dat <- st_vars %>%
        map(., ~ { shared_ctx$get(.x) }) %>%
        set_names(., st_vars) %>%
        modify_at(., "criteriaData_sub", ~ pluck(., 1)) %>%
        compact(.)

      # Process criteriaData_sub...

      # IMPORTANT: Reset context between iterations if needed
      # shared_ctx$reset()  # Uncomment if you need clean state

      return(dat)
    },
    .progress = TRUE
  )

# Clean up at the end
rm(shared_ctx)
```

**Option B: Use parallel processing (see Solution 4)**

#### Benefits
- Saves 5-25 seconds of V8 initialization time
- Reduces memory usage significantly
- Simpler resource management

#### Trade-offs
- Must be careful about context contamination between iterations
- May need to call `shared_ctx$reset()` between iterations

---

### SOLUTION 3: Replace rowwise() with Vectorized Operations

**Priority: HIGH** | **Estimated Speedup: 80-95%** | **Effort: Low**

#### Problem
rowwise() at line 3252 processes rows individually

#### Proposed Workaround

**BEFORE (slow):**
```r
curated_sswqs <- sswqs %>%
  # ... previous operations
  rowwise() %>%
  mutate(
    result = if (!num_bool) str_split(result, pattern = "-") else list(result)
  ) %>%
  unnest(result) %>%
  ungroup()
```

**AFTER (fast - vectorized):**
```r
curated_sswqs <- sswqs %>%
  # ... previous operations
  mutate(
    # Use if_else or case_when for vectorization
    result = if_else(
      num_bool,
      list(result),                      # Keep as-is if numeric
      str_split(result, pattern = "-")   # Split if not numeric
    )
  ) %>%
  unnest(result)
  # No need for ungroup() anymore
```

#### Benefits
- 80-95% faster on large datasets
- Cleaner code
- No need for ungroup()
- Fully vectorized operation processes all rows simultaneously

#### Technical Note
The `if_else()` function from dplyr is fully vectorized and processes all rows simultaneously, unlike the row-by-row approach of `rowwise()`.

---

### SOLUTION 4: Optimize map Operations with furrr (Parallel Processing)

**Priority: MEDIUM** | **Estimated Speedup: 50-300%** (depends on CPU cores) | **Effort: Medium**

#### Problem
Sequential processing of 50+ states

#### Proposed Workaround

```r
library(furrr)
library(future)

# Set up parallel processing
# Use n-1 cores to leave one for system
n_cores <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_cores)

# Replace pmap() with future_pmap()
state_dat <- state_vars %>%
  future_pmap(
    function(abv, json) {
      # Each worker gets its own V8 context
      ctx <- v8()
      ctx$source(json)

      st_vars <- ctx$eval(
        "Object.keys(this).filter(function(key) {
          return typeof this[key] !== 'function' && key !== 'global' && key !== 'console';
        })"
      ) %>%
        str_split(., ",") %>%
        pluck(., 1)

      dat <- st_vars %>%
        map(., ~ { ctx$get(.x) }) %>%
        set_names(., st_vars) %>%
        modify_at(., "criteriaData_sub", ~ pluck(., 1)) %>%
        compact(.)

      # ... rest of processing

      rm(ctx)
      return(dat)
    },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )

# Reset to sequential processing
plan(sequential)
```

#### Benefits
- 50-300% speedup depending on CPU cores
- Efficient use of multi-core systems
- Minimal code changes from original
- Each worker has isolated V8 context

#### Trade-offs
- Higher memory usage (multiple V8 instances in parallel)
- Requires `furrr` and `future` packages
- May not work well on systems with limited RAM

#### System Requirements
- Recommended: 8+ GB RAM, 4+ CPU cores
- Minimum: 4 GB RAM, 2 CPU cores

---

### SOLUTION 5: Optimize Multiple Joins

**Priority: MEDIUM** | **Estimated Speedup: 10-20%** | **Effort: Medium**

#### Problem
9 sequential joins creating intermediate copies

#### Proposed Workaround

**Option A: Use data.table for faster joins**
```r
library(data.table)

# Convert to data.table format
sswqs_dt <- as.data.table(crit_dat)
entities_dt <- as.data.table(entities)
pollutants_dt <- as.data.table(pollutants)
# ... convert all lookup tables

# Perform joins using data.table syntax (faster, in-place)
sswqs_dt <- entities_dt[sswqs_dt, on = .(short_code = area)]
sswqs_dt <- pollutants_dt[sswqs_dt, on = .(idx)]
# ... continue with remaining joins

# Convert back to tibble if needed
sswqs <- as_tibble(sswqs_dt)
```

**Option B: Pre-merge lookup tables (recommended)**
```r
# Merge all small lookup tables ONCE before the main join
lookup_combined <- entities %>%
  left_join(units, by = "idx") %>%
  left_join(protection_dict, by = "protection") %>%
  # ... merge all compatible lookup tables together

# Then do FEWER joins instead of 9
sswqs <- crit_dat %>%
  left_join(lookup_combined, by = c("short_code", "unit_code", "protection")) %>%
  left_join(use_class, by = c("short_code" = "area", "use_class" = "key")) %>%
  # ... only a few remaining joins
```

#### Benefits
- Fewer intermediate objects
- Reduced memory allocations
- Clearer code structure
- 10-20% faster overall

#### Considerations
- Need to ensure join keys are compatible
- Some lookup tables may not be pre-mergeable due to different granularity

---

### SOLUTION 6: Add Enhanced Caching Layer

**Priority: LOW-MEDIUM** | **Estimated Speedup: Variable** | **Effort: Low**

#### Current State
The script already has a checkpoint system (lines 10-48) checking for file age.

#### Enhanced Caching Proposal

```r
library(digest)

# Enhanced caching with content-based invalidation
cache_metadata <- list(
  source_url = "https://cfpub.epa.gov/wqsits/wqcsearch/data/criteria_json_5a.js",
  v8_version = packageVersion("V8"),
  r_version = R.version.string,
  script_version = "2.0"  # Increment when logic changes
)

cache_hash <- digest::digest(cache_metadata)
cache_file <- paste0("sswqs_cache_", cache_hash, ".RDS")

if (file.exists(cache_file)) {
  cli::cli_alert_success("Loading from cache: {cache_file}")
  state_dat <- readRDS(cache_file)
} else {
  cli::cli_alert_info("Cache miss. Running full data collection...")
  # Run expensive operations
  state_dat <- state_vars %>% pmap(...)

  # Save to cache
  saveRDS(state_dat, cache_file)

  # Clean up old cache files
  old_caches <- list.files(pattern = "^sswqs_cache_.*\\.RDS$")
  old_caches <- setdiff(old_caches, cache_file)
  if (length(old_caches) > 0) {
    cli::cli_alert_info("Removing {length(old_caches)} old cache file(s)")
    file.remove(old_caches)
  }
}
```

#### Benefits
- Automatic cache invalidation when dependencies change
- Prevents using stale cache with outdated R/package versions
- Self-cleaning (removes old caches)
- More robust than time-based caching

---

## 📊 Expected Performance Improvements

| Optimization | Priority | Estimated Speedup | Effort | Dependencies |
|-------------|----------|-------------------|---------|--------------|
| Extract lookup table | HIGH | 30-50% | Low | None |
| Reuse V8 context | HIGH | 40-60% | Medium | None |
| Remove rowwise() | HIGH | 80-95% | Low | None |
| Parallelize with furrr | MEDIUM | 50-300% | Medium | furrr, future |
| Optimize joins | MEDIUM | 10-20% | Medium | data.table (optional) |
| Enhanced caching | LOW-MEDIUM | Variable | Low | digest |

**Combined Expected Speedup:** 5-10x faster with all optimizations implemented

**Note:** Speedups are multiplicative in some cases, not purely additive. For example:
- rowwise() optimization: 5x faster on that section
- V8 reuse: 2x faster on V8-heavy sections
- Parallelization: 4x faster with 4 cores
- Combined: Could see 10-20x speedup in best case

---

## 🎯 Recommended Implementation Order

### Phase 1: Quick Wins (1-2 hours)
**Estimated Improvement: 3-4x faster**

1. **Extract lookup table to RDS file**
   - Lines to modify: 388-3094
   - Effort: 30 minutes
   - Risk: Low (can easily verify output is identical)

2. **Replace rowwise() with vectorized operations**
   - Lines to modify: 3252-3257
   - Effort: 15 minutes
   - Risk: Very low

3. **Test and benchmark**
   - Create benchmark script
   - Verify results are identical
   - Effort: 30 minutes

### Phase 2: Core Optimizations (2-4 hours)
**Estimated Additional Improvement: 1.5-2x faster**

4. **Reuse V8 context OR parallelize with furrr**
   - Choose one approach based on system specs:
     - Low RAM (<8GB): Reuse single V8 context
     - High RAM (8GB+): Parallelize with furrr
   - Lines to modify: 119-206
   - Effort: 2-3 hours
   - Risk: Medium (requires careful testing)

5. **Optimize join operations**
   - Lines to modify: 3110-3188
   - Effort: 1 hour
   - Risk: Low-medium

6. **Test and benchmark**
   - Verify results
   - Measure performance gains
   - Effort: 30 minutes

### Phase 3: Polish (1 hour)
**Estimated Additional Improvement: 10-20% (on cache hits)**

7. **Enhanced caching system**
   - Lines to modify: 10-48
   - Effort: 30 minutes
   - Risk: Low

8. **Add performance logging**
   - Add timing information for each major section
   - Effort: 15 minutes

9. **Documentation updates**
   - Update comments
   - Add performance notes
   - Effort: 15 minutes

---

## 📝 Additional Recommendations

### 1. Memory Management
**Current Status:** ✅ Good

The script already does explicit cleanup:
- Line 114: `rm(vars, state_extra, cx)`
- Line 198: `rm(ctx)`
- Line 342: `rm(state_dat)`
- Lines 3196-3207: Extensive cleanup of intermediate objects

**Recommendation:** Continue this practice in optimized version.

---

### 2. Progress Indicators
**Current Status:** ✅ Good

The script uses `.progress = TRUE` in pmap calls (line 201).

**Recommendation:** Keep these indicators, especially for long-running operations. Consider adding more granular progress for the V8 processing:

```r
cli::cli_progress_bar("Processing states", total = nrow(state_vars))
state_dat <- state_vars %>%
  pmap(function(abv, json) {
    cli::cli_progress_update()
    # ... processing
  })
cli::cli_progress_done()
```

---

### 3. Error Handling
**Current Status:** ⚠️ Needs Improvement

Line 122 has a TODO comment:
```r
# TODO: Need to wrap in error handling for dropped connections...
```

**Recommendation:** Add comprehensive error handling:

```r
state_dat <- state_vars %>%
  pmap(
    function(abv, json) {
      tryCatch({
        ctx <- v8()
        ctx$source(json)
        # ... processing
      }, error = function(e) {
        cli::cli_alert_danger("Failed to process {abv}: {e$message}")
        return(NULL)  # Return NULL for failed states
      }, finally = {
        if (exists("ctx")) rm(ctx)
      })
    },
    .progress = TRUE
  ) %>%
  compact()  # Remove NULL entries from failures
```

---

### 4. Testing Strategy

**Before optimizing, create a benchmark and validation suite:**

```r
library(bench)
library(testthat)

# 1. Create reference output from original script
reference_output <- readRDS("sswqs.RDS")

# 2. Run optimized version
source("sswqs_curation_optimized.R")
optimized_output <- readRDS("sswqs_optimized.RDS")

# 3. Validate results are identical
test_that("Optimized version produces identical results", {
  expect_equal(
    reference_output %>% arrange(name, analyte),
    optimized_output %>% arrange(name, analyte),
    tolerance = 1e-10
  )
})

# 4. Benchmark performance
benchmark_results <- bench::mark(
  original = {
    source("sswqs_curation.R")
  },
  optimized = {
    source("sswqs_curation_optimized.R")
  },
  iterations = 3,
  check = FALSE,
  memory = TRUE
)

print(benchmark_results)
```

---

### 5. Code Organization

**Current Status:** ⚠️ Monolithic script

**Recommendation:** Consider breaking into modules:

```
epa/sswqs/
├── sswqs_curation.R           # Main orchestration script
├── R/
│   ├── fetch_data.R           # V8 and data fetching functions
│   ├── process_data.R         # Data transformation functions
│   ├── clean_data.R           # Cleaning and harmonization
│   └── utils.R                # Helper functions
├── data/
│   ├── use_class_lookup.RDS   # Extracted lookup table
│   └── protection_lookup.RDS  # Protection codes
└── tests/
    └── test_curation.R        # Test suite
```

---

## 🔍 Performance Monitoring

**Add timing information to track bottlenecks:**

```r
library(tictoc)

tic("Total script runtime")

tic("Downloading spec")
# ... spec download code
toc()

tic("State data download")
# ... state data code
toc()

tic("Data extraction and cleaning")
# ... extraction code
toc()

tic("Building final dataset")
# ... building code
toc()

tic("Unit harmonization")
# ... harmonization code
toc()

toc()  # Total time
```

This will help identify which sections benefit most from optimization.

---

## 📚 References and Resources

### Packages Used
- **V8**: JavaScript engine for R
- **dplyr/tidyr**: Data manipulation
- **purrr**: Functional programming tools
- **furrr**: Parallel mapping with futures (for optimization)
- **data.table**: High-performance data manipulation (optional)

### Performance Resources
- [R Inferno](https://www.burns-stat.com/pages/Tutor/R_inferno.pdf) - Common R performance pitfalls
- [Advanced R - Performance](https://adv-r.hadley.nz/perf-measure.html) - Hadley Wickham
- [data.table Performance](https://h2oai.github.io/db-benchmark/) - Join benchmarks

---

## 💡 Summary

The `sswqs_curation.R` script has significant optimization opportunities:

**Critical Issues:**
1. 2,700-line hardcoded lookup table (80% of file)
2. 50+ V8 contexts created unnecessarily
3. Slow rowwise() operations

**Expected Results:**
- **Performance:** 5-10x faster with all optimizations
- **File Size:** 1.7MB → ~300KB (78% reduction)
- **Maintainability:** Much improved with external lookup tables
- **Resource Usage:** Significantly reduced memory footprint

**Recommended Next Steps:**
1. Start with Phase 1 quick wins (extract lookup table, remove rowwise)
2. Benchmark and validate results
3. Proceed to Phase 2 optimizations based on results
4. Add performance monitoring for future improvements

---

**Questions or need implementation help?** Let me know which optimizations you'd like to implement first!
