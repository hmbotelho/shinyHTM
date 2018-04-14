import ij.IJ;
import ij.ImagePlus;
import ij.gui.OvalRoi;
IJ.run("Image Sequence...", "open=[/Users/tischer/Documents/sylwia/data] file=(SK_10--W00061--P00001--Z(?<slice>\\d+)--T(?<timePoint>\\d+)--(?<channel>.+)\\.tif) sort");
ImagePlus imp1 = IJ.getImage(); 
IJ.open("/Users/tischer/Documents/sylwia/data/SK_10--W00061--P00001--Z--T--foreground--labelMask.tif");
IJ.run("glasbey inverted", "");
ImagePlus imp2 = IJ.getImage(); 
IJ.run(imp1, "Merge Channels...", "c1="+imp1.getTitle()+" c2="+imp2.getTitle()+" create");
IJ.wait( 100 );
IJ.getImage().setPosition( 1 ,15, 1);
IJ.getImage().setRoi( new OvalRoi(846.019,1000.878,50,50) )
IJ.run (IJ.getImage(), "Add Selection...", "")

