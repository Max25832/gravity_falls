import ee
import pandas as pd

def initialize_ee(project="ee-sentinel-ts-an-detection"):
    """Initialize Earth Engine with the specified project."""
    ee.Initialize(project=project)

def load_aoi(aoi_path):
    """Load AOI from Earth Engine asset."""
    return ee.FeatureCollection(aoi_path)

def add_neighborhood_variance(image, kernel_select, var_bands):
    """Add neighborhood variance bands to the image."""
    nbr_var = (
        image.select(var_bands)
        .reduceNeighborhood(
            reducer=ee.Reducer.variance(),
            kernel=kernel_select,
            skipMasked=True,
        )
    )
    var_band_names = nbr_var.bandNames().map(
        lambda b: ee.String(b).cat("_var")
    )
    nbr_var = nbr_var.rename(var_band_names)
    return image.addBands(nbr_var)

def add_neighborhood_mean(image, kernel_select, mean_bands):
    """Add neighborhood mean bands to the image."""
    nbr_mean = (
        image.select(mean_bands)
        .reduceNeighborhood(
            reducer=ee.Reducer.mean(),
            kernel=kernel_select,
            skipMasked=True,
        )
    )
    mean_band_names = nbr_mean.bandNames().map(
        lambda b: ee.String(b).cat("_mean")
    )
    nbr_mean = nbr_mean.rename(mean_band_names)
    return image.addBands(nbr_mean)

def month_ranges(start, end):
    """Generate monthly periods."""
    return pd.period_range(start=start, end=end, freq="M")

def sample_image(image, aoi):
    """Sample image within AOI."""
    time_str = image.date().format("YYYY-MM-dd")
    sampled = image.sample(
        region=aoi,
        scale=10,
        projection="EPSG:4326",
        geometries=True,
    )
    filtered = sampled.filter(ee.Filter.inList("SCL", [4, 5]))
    return filtered.map(lambda f: f.set("time", time_str))

def export_monthly(fc, period, folder):
    """Export monthly data to Google Drive."""
    start = period.to_timestamp("M") - pd.offsets.MonthEnd(0)
    start = start.replace(day=1)
    end = period.to_timestamp("M")

    start_str = start.strftime("%Y-%m-%d")
    end_str = end.strftime("%Y-%m-%d")

    subset = fc.filterDate(start_str, end_str)

    task = ee.batch.Export.table.toDrive(
        collection=subset,
        description=f"samples_{start_str}",
        folder=folder,
        fileNamePrefix=f"samples_{start_str}",
        fileFormat="CSV",
    )
    task.start()
    print(f"Export gestartet: {start_str} – {end_str}")