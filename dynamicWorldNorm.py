import os
import warnings
import numpy as np
import rasterio
import argparse
import matplotlib.pyplot as plt
plt.ioff() # ensure interactive matplotlib is deactivated

# CALL:
# python3 dynamic-world-norm.py [-p|--plot] /.../src.tif /.../dst.tif
# python3 dynamic-world-norm.py -p /bulk-1/rasters/2023-07-04_index_blue_test-buffer_filled_aligned.tif /bulk-1/rasters/2023-07-04_index_blue_test-buffer_filled_aligned_normalized.tif

# normalize single band geotiff with dynamic world method
    # 1. Log transform to handle long-tailed values
    # 2. Calculate 1st and 99th percentiles of log-transformed data
    # 3. Remap these percentiles to sigmoid curve
    # results in reshaped DN, with extremes squished but not clipped, bounded on (0, 1)

def plot_transformations(data_dict, output_path):
    """
A 2x2 grid showing the raster at each stage of transformation.
    """
    fig, axs = plt.subplots(2, 2, figsize=(15, 15))
    stages = ['original', 'log_transformed', 'scaled', 'normalized']

    for i, (stage, data) in enumerate(data_dict.items()):
        ax = axs[i//2, i%2]
        im = ax.imshow(data, cmap='viridis')
        ax.set_title(stage.capitalize())
        plt.colorbar(im, ax=ax)

    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()

def plot_histograms(data_dict, output_path):
    """
A 2x2 grid showing histograms of the data at each stage of transformation.
    """
    fig, axs = plt.subplots(2, 2, figsize=(15, 15))
    stages = ['original', 'log_transformed', 'scaled', 'normalized']

    for i, (stage, data) in enumerate(data_dict.items()):
        ax = axs[i//2, i%2]
        ax.hist(data.ravel(), bins=50)
        ax.set_title(f'{stage.capitalize()} Histogram')
        ax.set_xlabel('Value')
        ax.set_ylabel('Frequency')

    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()

def counts(data, name, nodata_value):
    """
    Check data set properties for diagnostics
    """

    #arg properties
    print("ARG PROPERTIES:")
    print(f"name: {name}")
    print(f"ndval: {nodata_value}")
    print(f"data type: {data.dtype}")
    print(f"Data range: {data.min()} to {data.max()}")

    #Nan and masked
    print("NAN AND MASKED")
    if np.ma.is_masked(data):
        print(f"Number of masked values: {data.mask.sum()}")
    print(f"n_nan: {np.count_nonzero(np.isnan(data))}")

    #nodata value
    print("NODATA")
    print(f"n_neg10k_f: {np.count_nonzero(data == -10000.0)}")
    print(f"n_neg10k_i: {np.count_nonzero(data == -10000)}")
    print(f"n_neg10k_close: {np.count_nonzero(np.isclose(data, -10000.0, rtol=1e-5))}")
    print(f"n_ndVal: {np.count_nonzero(data == nodata_value)}")
    print(f"n_ndVal_close: {np.count_nonzero(np.isclose(data, nodata_value, rtol=1e-5))}")
    near_nodata = data[(data < -9999) & (data > -10001)]
    print(f"Values close to -10000: {np.unique(near_nodata)}")

    # zero
    print("ZERO")
    print(f"n_zero: {np.count_nonzero(data == 0)}")
    print(f"n_zero_close: {np.count_nonzero(np.isclose(data, 0, atol=1e-8))}")

    #values negative
    print("NEGATIVE")
    print(f"n_pos: {np.count_nonzero(data > 0 )}")
    print(f"n_neg: {np.count_nonzero(data < 0)}")
    negative_values = data[data < 0]
    if len(negative_values > 0):
        print(f" Min negative value: {np.min(negative_values)}")
        print(f" Max negative value: {np.max(negative_values)}")
        print(f" Mean of negative values: {np.mean(negative_values)}")

def normalize(data, nodata_value=None):
    """
    Normalize data using the Dynamic World paper's method:

    Args:
        data: numpy array of input values
        nodata_value: value to be masked/ignored in calculations

    Returns:
        numpy array of normalized values in range [0,1]
    """
    #create a mask if nodata_value is provided
    if nodata_value is not None:
        data = np.ma.masked_equal(data, nodata_value)

    # Print diagnostics and Throw error if any negative values  remain after nodata_value is converted to mask
    negative_values = data[(data < 0) & (~data.mask)]
    if len(negative_values > 0):
        counts(data, "data after no value mask", nodata_value)
        raise ValueError(f"Negative values found in the data after masking: {negative_values}")

    # Log transform
    # add epsilon to handle zeros
    log_data = np.log10(data + 1e-10) # masked values '--' do not affect calculation
    #
    # Calculate percentiles of log-transformed data
    if np.ma.is_masked(log_data):
        pmin = np.percentile(log_data.compressed(), 1)
        pmax = np.percentile(log_data.compressed(), 99)
    else:
        pmin = np.percentile(log_data, 1)
        pmax = np.percentile(log_data, 99)
    #
    # Map percentile[1, 99] to approximately [-2, 2]
    # shift midpoint to 0, scale range to 1, expand to range
    transformed_data = 4 * (log_data  - (pmax + pmin) / 2) / (pmax - pmin)
    # apply sigmoid normalization s.t. OP is (0, 1) and extremes are compressed
    norm_data = 1/(1+np.exp(-transformed_data)) #sigmoid: 1/(1+e^(-x))
    #
    #carry mask forward
    if nodata_value is not None:
        norm_data = np.ma.array(norm_data, mask=data.mask)
    #
    return {
        'original': data,
        'log_transformed': log_data,
        'scaled': transformed_data,
        'normalized': norm_data
    }

def normalize_raster(input_path, output_path, plot):
    """
    Normalize a single band raster geotiff file using the Dynamic World normalization method.

    Args:
        input_path: path to input raster file
        output_path: path where normalized raster will be saved
        plot: boolean if histogram are to be made, will be placed at path of output_path
    """

    with rasterio.open(input_path) as src:
        success = False
        data = src.read(1)
        nodata = src.nodata
        data_dict = normalize(data, nodata)
        normalized = data_dict['normalized']

        if plot:
            directory = os.path.dirname(output_path)
            raster_name = os.path.splitext(os.path.basename(output_path))[0]
            plot_transformations(data_dict,
                                 os.path.join(directory, f'{raster_name}_transformations.png'))
            plot_histograms(data_dict,
                            os.path.join(directory, f'{raster_name}_histograms.png'))

        # Prepare the output profile (metadata)
        profile = src.profile.copy()
        dtype_norm = normalized.dtype
        if dtype_norm != np.float32:
            warnings.warn(f"The normalized data type is {dtype_norm} Expected to find float32, ensure this is correct for down stream processing")
        profile.update(dtype=dtype_norm)

        # Write the normalized data
        with rasterio.open(output_path, 'w', **profile) as dst:
            dst.write(normalized.astype(dtype_norm), 1)
            print(f"Normalized raster: {output_path}")
            success = True

        if not success:
            raise RuntimeError(f"Unsuccessful attempt to produce normalized raster: {output_path}")

def run(srcFile, dstFile, plot):
    """
    call from py4cl2
    (dynamic-world-norm:run :src-file \"f\" :dst-file \"f\" :plot \"bool\" )
    """
    normalize_raster(srcFile, dstFile, plot)

def main():
    """Main function to handle command-line arguments and execute file operations."""
    parser = argparse.ArgumentParser(
        description="File operations utility script",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "src_file",
        help="Path to the source file"
    )
    parser.add_argument(
        "dst_file",
        help="New path and name for the file"
    )
    parser.add_argument(
        "-p", "--plot",
        action="store_true",
        help="Enable plots"
    )
    args = parser.parse_args()

    normalize_raster(args.src_file, args.dst_file, args.plot)
    plt.close('all') #ensure matplotlib ends

if __name__ == "__main__":
    main()
