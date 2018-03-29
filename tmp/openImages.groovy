import ij.IJ;
import ij.gui.OvalRoi
IJ.run("Image Sequence...", "open=[/Users/tischer/Documents/sylwia/data] file=(BT_10--W00037--P00001--Z(?<slice>\\d+)--T(?<timePoint>\\d+)--(?<channel>.+)\\.tif) sort")
IJ.getImage().setSlice(16.886)
IJ.getImage().setRoi( new OvalRoi(1474.859,906.544,50,50) )

