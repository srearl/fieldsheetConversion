## fieldsheetConversion

Companion R package to
[`mocness_field_sheets`](https://github.com/srearl/mocness_field_sheets). It
ingests JSON (or fenced JSON-in-text) exports produced from handwritten MOCNESS
field sheets and converts them into tidy tabular structures: a header table (one
row per tow) and a nets table (one row per net deployment).

### ⭐ Key Features
- Robust parsing of JSON even when wrapped in Markdown code fences.
- Flexible field name handling (accepts multiple legacy variants).
- Latitude/longitude normalization to decimal degrees.
- Automatic extraction of time ranges, volumes, flow counts, and metadata.
- Consistent, tidy outputs ready for analysis / archiving.
- Simple end-to-end helper: parse a directory and write CSV outputs.

### 📦 Installation
This repository is currently a source package. Install directly from the local
clone or (if hosted) GitHub:

```r
# From a local checkout
remotes::install_local("path/to/fieldsheetConversion")

# Or (if/when public)
remotes::install_github("srearl/fieldsheetConversion")
```

### 📁 Expected Inputs
`moc_parse_all()` scans a directory for files named like:
```
tow_1.json, tow_2.json, ... OR tow_1.txt (containing JSON)
```
Files may be plain JSON or JSON surrounded by Markdown fences (e.g. ```json ... ```).

### 🔧 Core Functions

| Function | Purpose |
|----------|---------|
| `moc_parse_all(input_dir)` | Parse all qualifying tow files in a directory; returns list(headers, nets). |
| `moc_parse_single(rec)` | Parse a single record returned by `moc_read_file()`. |
| `moc_read_file(path)` | Read and attempt to parse JSON from a file (supports fenced content). |
| `moc_normalize_header(header)` | Standardize header list into a one-row tibble. |
| `moc_normalize_nets(x)` | Standardize nets subsection into tidy rows. |
| `moc_write_outputs(res, out_dir)` | Write `mocness_headers.csv` and `mocness_nets.csv`. |
| `moc_bind_results(parsed_list)` | Combine per-tow parsed outputs into unified tables. |

Helper utilities (`%||%`, `moc_split_range`, `moc_parse_latlon`, etc.) support parsing and normalization.

### 🗂 Output Data Model
#### Headers Table (`res$headers` / `mocness_headers.csv`)
Columns include (not exhaustive):
- `tow_id` (derived from filename), `tow_file`, `cruise`, `tow_label`, `date`, `date_raw`
- `local_start`, `local_end`, `gmt_start`, `gmt_end`
- `start_lat_raw`, `start_long_raw`, `end_lat_raw`, `end_long_raw`
- `start_lat_dd`, `start_long_dd`, `end_lat_dd`, `end_long_dd`
- `net_size_raw`, `net_mesh_raw`, `net_condition`, `general_comments`

#### Nets Table (`res$nets` / `mocness_nets.csv`)
Per net deployment:
- `tow_id`, `tow_file`, `net`
- `net_time_raw`, `depth_m`, `angle_deg`
- `flow_counts`, `volume_raw`, `volume_calc`
- `mwo_raw`, `comments`
- Joined cruise/tow/date context from the headers table.

### ⚙️ Typical R Workflow
```r
library(fieldsheetConversion)

# 1. Parse all tow files in a directory
res <- moc_parse_all("data/field_sheets")

# 2. Inspect
res$headers
res$nets

# 3. Write outputs (optional)
moc_write_outputs(res, out_dir = "derived")

# 4. Simple QC
dplyr::count(res$nets, tow_id)
```

### 🖥 Command Line Usage (Original Script Pattern)
While this package exposes functions for programmatic use, the original
development workflow supported a CLI-style script with flags:

```
Rscript script.R --in path/to/tow_dir --out /tmp/output
```

To replicate:
1. Create a small driver script (example below).
2. Call the exported functions.

Example driver (`inst/scripts/moc_batch.R`):
```r
#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
inp <- "."; out <- "./out"
if ("--in" %in% args)  inp <- args[which(args == "--in") + 1]
if ("--out" %in% args) out <- args[which(args == "--out") + 1]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
res <- fieldsheetConversion::moc_parse_all(inp)
fieldsheetConversion::moc_write_outputs(res, out)
```

Make executable:
```bash
chmod +x inst/scripts/moc_batch.R
```

Then run:
```bash
Rscript inst/scripts/moc_batch.R --in data/field_sheets --out derived
```

### 🔍 Quality Control Suggestions
- Check date span: `range(res$headers$date, na.rm = TRUE)`.
- Verify coordinate parsing: compare raw vs decimal degree columns.
- Spot missing depths: `dplyr::filter(res$nets, is.na(depth_m))`.
- Ensure net numbering sequential per tow: `dplyr::count(res$nets, tow_file, net)`.

### 🧩 Handling Uncertain / Variant Fields
The parser accepts multiple synonymous field names (e.g., `net_tows`,
`net_tow_information`). If new variants appear, extend the coalescing logic in
`moc_normalize_nets()` / `moc_normalize_header()`.

### 🗺 Latitude / Longitude Parsing
Supports inputs like:
```
42 30.5 N
42°30.5' N
42.5083N
```
Converted to decimal degrees; southern and western hemispheres become negative.

### 🧪 Reproducible Example (Mock)
```r
tmp <- tempdir()
# Suppose tmp contains tow_1.json, tow_2.json produced by extraction pipeline
res <- moc_parse_all(tmp)
str(res$headers)
str(res$nets)
```

### 🤝 Contributing
Pull requests welcome: add field name variants, improve validation, or add
tests. Please run `devtools::document()` before submitting so Rd files stay in
sync.

### 📄 License
See `LICENSE` file.

### ℹ Related Repository
Data extraction (OCR / transcription) lives in: https://github.com/srearl/mocness_field_sheets

---
Feel free to open an issue for enhancements or parsing edge cases.