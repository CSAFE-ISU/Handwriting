import qrtools
import pyqrcode
from pyzbar.pyzbar import decode
from PIL import Image

def decode_file(file):
	#test = decode(Image.open('Desktop/06052019_jekruse_005.png'))
	test = decode(Image.open(file))
	test_data = test[0].data
	return test_data
