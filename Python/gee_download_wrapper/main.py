#!/usr/bin/env python3
"""
Main script for processing Sentinel-2 data with Earth Engine.
This script loads AOI, processes images, and exports monthly samples.
"""

from utilities import (
    initialize_ee, load_aoi, add_neighborhood_variance, add_neighborhood_mean,
    month_ranges, sample_image, export_monthly
)
from config import (
    EE_PROJECT, AOI_PATH, START_DATE, END_DATE, OUTPUT_FOLDER,
    KERNEL_RADIUS, BAND_LIST, VAR_BANDS
)

def main():
    # Initialize Earth Engine
    initialize_ee(EE_PROJECT)

    # Load AOI
    aoi = load_aoi(AOI_PATH)
    smallAOI = aoi  # As in the original script

    print("AOI loaded")

    # Create kernel
    kernel = ee.Kernel.square(radius=KERNEL_RADIUS, units="pixels")

    # Load Sentinel-2 collection
    S2 = (
        ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
        .filterBounds(aoi)
        .filterDate(START_DATE, END_DATE)
        .select(BAND_LIST)
    )

    # Add variance and mean bands
    S2_with_var = S2.map(
        lambda img: add_neighborhood_variance(img, kernel, VAR_BANDS)
    )
    S2_with_var = S2_with_var.map(
        lambda img: add_neighborhood_mean(img, kernel, VAR_BANDS)
    )

    # Sample images
    data_collection = S2_with_var.map(lambda img: sample_image(img, aoi)).flatten()

    # Export monthly data
    months = month_ranges(START_DATE, END_DATE)
    for period in months:
        export_monthly(data_collection, period, OUTPUT_FOLDER)

if __name__ == "__main__":
    main()