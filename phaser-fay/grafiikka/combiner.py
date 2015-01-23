import os
import sys
from glob import glob
from PIL import Image

def __main__():
    folder = None
    if len(sys.argv) == 2:
        folder = sys.argv[1]
    else:
        folder = raw_input('folder name: ')

    imagenames = sorted(glob(os.path.join(folder, '*.png')))

    if len(imagenames) == 0:
        print('Error: no images found in ' + folder)
        return

    # Load images and calculate image size for the merged image.
    total_width = 0
    total_height = 0
    images = []
    for imgname in imagenames:
        image = Image.open(imgname)
        images.append(image)
        total_width += image.size[0]
        total_height = max(total_height, image.size[1])

    # Merge images.
    merged = Image.new('RGBA', (total_width, total_height))
    paste_pos = 0
    for img in images:
        merged.paste(img, (paste_pos, 0))
        paste_pos += img.size[0]

    # Save merged image.
    merged.save(folder + '.png', 'PNG')

if __name__ == '__main__':
    __main__()
