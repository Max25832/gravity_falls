# Configuration file for the project

# Earth Engine project
EE_PROJECT = "ee-sentinel-ts-an-detection"

# AOI asset path
AOI_PATH = "projects/ee-sentinel-ts-an-detection/assets/test_area_small"

# Date range
START_DATE = "2025-08-01"
END_DATE = "2026-01-15"

# Output folder in Google Drive
OUTPUT_FOLDER = "python_wrapper"

# Kernel for neighborhood operations
KERNEL_RADIUS = 1  # pixels

# Band lists
BAND_LIST = [
    "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12", "SCL", "QA60"
]

# Bands for variance and mean calculation
VAR_BANDS = [
    "B3", "B4", "B5", "B6", "B7", "B8"
]