import sys
import os

# windows system
def write_to_vhd(vhd_path, bin_path, sector_number):
    if not os.path.exists(bin_path):
        print(f"Error: The file {bin_path} does not exist.")
        return
    
    # Get bin file size
    bin_size = os.path.getsize(bin_path)
    
    # Calculate the number of sectors to write
    sector_size = 512
    num_sectors = (bin_size + sector_size - 1) // sector_size

    # Open the VHD file
    with open(vhd_path, "r+b") as f:
        # Seek to the sector number
        f.seek(sector_number * sector_size)
        
        # Read the bin file
        with open(bin_path, "rb") as bin_file:
            data = bin_file.read()
        
            # Write the bin file to the VHD file
            for i in range(num_sectors):
                f.write(data[i * sector_size:(i + 1) * sector_size])
    

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python VHDWriter.py <vhd_path> <bin_path> <sector_number>")
        sys.exit(1)
    
    vhd_path = sys.argv[1]
    bin_path = sys.argv[2]
    sector_number = int(sys.argv[3])  # Make sure the sector number is an integer

    write_to_vhd(vhd_path, bin_path, sector_number)