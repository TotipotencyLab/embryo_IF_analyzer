// version 0.1.2
// last update: 2024-03-01


// # Current Analysis Configuration ==============================================

// ## Output specification -------------------------------------------------------
outdir="/Users/chad/Lab/0_imaging/test/"
out_basename="test_16C_"

//outdir="/Users/chad/Lab/0_imaging/SR002_Nr5a2_F1/measurements/"
//out_basename="SR006_"
//out_basename="test_"

// these two settings do nothing at the moment
//slice_start=1
//slice_end=72

// # Set_basename
cur_window = getTitle();
cur_name = getInfo("slice.label");
cur_name = split(cur_name, "\\/");
//Array.print(cur_name);
use_name = Array.filter(cur_name, "Position");
out_basename=out_basename + use_name[0];
//out_basename=out_basename + "test"

// cur_name = getTitle();
//out_basename = split(cur_name, ".");
//out_basename = out_basename[0] + "_z" + slice_start + "_" + slice_end;
//out_basename = out_basename[0];

// ## Image information --------------------------------------------------------
dna_mask_ch=1;
//cyto_mask_ch=1;
channel_to_measure=newArray(1,2,3);
//channel_to_measure=newArray(1,2);

// ## Behavior control ---------------------------------------------------------
run_mask_nucleoli = false;
keep_nuc_roi = true;
run_mask_cytoplasm = false;

// Testing feature; didn't seems to work properly
run_fit_ellipse = false;

// Default by Imre
threshold_method_nucleus = "Huang2"
//threshold_method_nucleolus = "Triangle"
//threshold_method_nucleolus = "Huang2"
//threshold_method_nucleolus = "Li"
threshold_method_nucleolus = "Default"

//
//auto_set_measurement = true
only_current_z_stack = false;
close_outline_window = true;
close_outline_table_window = true;
// Result behavior
auto_measure_results = false;
auto_reset_results_window = false;



// # ===========================================================================
// # Analysis Section ==========================================================

// # Set measurement
//if(run_fit_ellipse){
//	run("Set Measurements...", "area min area_fraction limit display redirect=None decimal=3");
//}else{
//	run("Set Measurements...", "area centroid perimeter fit shape feret's redirect=None decimal=2");
//}


// # Fetching some constant information
// Getting the dimensional information of the pixel in real world unit
getPixelSize(unit, pixelWidth, pixelHeight);

// # get the current position of the original (ori) image viewing position
start_title = getTitle();
Stack.getPosition(start_ch, start_z, start_frame);

if(only_current_z_stack){
	// Only duplicate the current z-stack
	//run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+slice_start+"-"+slice_end);
	run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+start_z);
	dup_title = getTitle();
}

// # Nucleus --------------------------------------------------------------------
clear_roi();
// mask_nucleus(dna_mask_ch, sigma, particle_size, fill_hole, close_popup_window)
mask_nucleus(dna_mask_ch, 8, threshold_method_nucleus, "40-Infinity", true, close_outline_window);

n = roiManager("count");
if(n>0){
	modify_roi_name_range("nucleus_", "", 0, n);
}


if(run_fit_ellipse){
	selectWindow(cur_window);
	draw_best_fitting_ellipses();
	//selectWindow(cur_window);
}

n = roiManager("count");
if(n>0){
	// save_outline_coord(dm_name, outdir, outfile_basename, close_popup_window)
	save_outline_coord("nucleus_outline", outdir, out_basename, close_outline_table_window);
	selectWindow(cur_window); // come back to the main image
	
	// measure_roi(cur_window, channel_to_measure, outdir, out_basename+"_nucleus", true);
	if(auto_measure_results){
		measure_roi(cur_window, channel_to_measure, outdir, out_basename+"_nucleus", auto_reset_results_window);
	}
	run("Select None");
}else{
	print("No nucleus found!");
}



// # Nucleoli --------------------------------------------------------------------
if(run_mask_nucleoli){
	if(keep_nuc_roi){
		clear_roi();
	}
	//mask_nucleus(dna_mask_ch, 5, "3-100", false, close_outline_window);
	mask_nucloli(dna_mask_ch, 8, threshold_method_nucleolus, "3-300", "0.50-1.00", close_outline_window);
	
	n = roiManager("count");
	if(n>0){
		modify_roi_name_range("nucleolus_", "", 0, n);
		save_outline_coord("nucleolus_outline", outdir, out_basename, close_outline_table_window);
		selectWindow(cur_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(cur_window, channel_to_measure, outdir, out_basename+"_nucleolus", auto_reset_results_window);
		}
		
		run("Select None");
		
	}else{
		print("No nucleoli found!");
	}
}

print("Analysis Done!");

// # Cytoplasm background -------------------------------------------------------
if(run_mask_cytoplasm){
	clear_roi();
	mask_cytoplasm(cyto_mask_ch, 10, close_outline_table_window);
	
}


// # Close temporary window

if(only_current_z_stack){
	close(dup_title);
}

// ## local FUNCTIONS ============================================================

// ROI management ----------------------------------------------------------------
function clear_roi() {
	run("Select None");
	if (roiManager("count")>0) {
		roiManager("Deselect");
		roiManager("Delete");
	}
}

function modify_roi_name_range(prefix, suffix, min_idx, max_idx){
	n=roiManager("count");
	for(i=0; i<n; i++){
		do_rename=(i>=min_idx)&(i<max_idx);
		if(do_rename){
			roiManager("select", i);
			roi_old_name = RoiManager.getName(i);
			// # renaming the recently added one
			roiManager("select", i); // not in original code
			roiManager("rename", prefix + roi_old_name + suffix);
		}
	}
}

// # masking outline ------------------------------------------------------------------------------------------------

function mask_nucleus(dna_mask_ch, sigma, method, particle_size, fill_hole, close_popup_window) {
	run("Duplicate...", "duplicate channels="+dna_mask_ch);
	tmpt=getTitle();
	selectWindow(tmpt);
	
	// # Alternative duplication function
	//run("Duplicate...", "slices="+slice_start+"-"+slice_end);
	//run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+slice_start+"-"+slice_end);
	
	// Bluring the nucleus
	//run("Gaussian Blur...", "sigma=3 stack");
	run("Gaussian Blur...", "sigma="+sigma+" stack");
	
//	run("Auto Threshold", "method=Huang2 ignore_black ignore_white white stack use_stack_histogram");
	run("Auto Threshold", "method="+method+" ignore_black ignore_white white stack use_stack_histogram");
	//run("Close-", "stack");
			
	// Process > Binary > Fill Holes
	if(fill_hole){
		run("Fill Holes", "stack");
	}
	
	setOption("BlackBackground", true);
	//run("Erode", "stack");
	run("Analyze Particles...", "size="+particle_size+" exclude include add stack");
	
	if(close_popup_window){
		// closing active image window
		close();
	}
}


function mask_nucloli(dna_mask_ch, sigma, method, particle_size, particle_circularity, close_popup_window){
	// recommended input value:
	//		sigma: 5
	//		particle_size: 3-300
	//		circularity: 0.50-1.00
	
	run("Duplicate...", "duplicate channels="+dna_mask_ch);
	tmpt=getTitle();
	selectWindow(tmpt);
	
	run("Invert", "stack");
	run("Gaussian Blur...", "sigma="+sigma+" stack");
	// run("Auto Threshold", "method=Triangle white stack");
	run("Auto Threshold", "method="+method+" white stack");
	run("Close-", "stack");
	//run("Dilate", "stack");
	run("Analyze Particles...", "size="+particle_size+" circularity="+particle_circularity+" exclude add stack");
	
	if(close_popup_window){
		// closing active image window
		close();
	}
}


function mask_cytoplasm(cyto_mask_ch, sigma, close_popup_window){
	// # Set value
	mx=50;
	trim_cyto=500; //in nm
	trim_cyto_px=round(trim_cyto/pixelWidth/1000);
	
	run("Duplicate...", "duplicate channels="+cyto_mask_ch);
	
	// clear any available ROI
	if (roiManager("count")>0){
		roiManager("Deselect");
		roiManager("Delete");
	}
	
	// Masking
	run("Gaussian Blur...", "sigma="+sigma+" stack");
	
	//run("Convert to Mask", "method=Triangle background=Dark black");
	
	
	// # Not sure why Imre use this threshold
	setThreshold(0.5*mx, 65535);
	run("Convert to Mask", "method=Default background=Dark black");
	run("Fill Holes", "stack");
	//for (rep=0;rep<trim_cyto_px;rep++) run("Erode", "stack");
	run("Erode", "stack");
	run("Analyze Particles...", "include add stack");
	
	if(close_popup_window){
		// closing active image window
		close();
	}
}

// # After masking the area -----------------------------------------------------------------

function save_outline_coord(dm_name, outdir, outfile_basename, close_popup_window) {
	dm=dm_name;
	// # A placeholder for our outline table
	Table.create(dm);
	
	// # This should be unnecessary 	
	//getPixelSize(unit, pixelWidth, pixelHeight);
	
	n = roiManager("count");
	if(n>0) {
		cnt=0; // for line count?
		
		// # save all currently available ROI object
		roiManager("Save", outdir + outfile_basename + "_" + dm_name + "_ROIs.zip");
		
		// # Saving the outline into a text file
		for ( i=0; i<n; i++ ) { 
			roiManager("select", i); // select one ROI
			
			// # fetch information of the current ROI
			Stack.getPosition(channel, curr, frame);
			roi_name=Roi.getName;
			Roi.getCoordinates(xpoints, ypoints);
			
			// # add info to a table
			for (noc=0; noc<xpoints.length; noc++){
				Table.set("name", cnt, outfile_basename, dm);
				Table.set("roi", cnt, roi_name, dm);
				Table.set("z", cnt, curr, dm);
				Table.set("x", cnt, xpoints[noc]*pixelWidth, dm);
				Table.set("y", cnt, ypoints[noc]*pixelWidth, dm);
				cnt++;
			}
			
		} // end of for loop
		
		// # Save to file
		Table.update(dm);
		Table.save(outdir+outfile_basename+"_"+dm_name+".txt");	
		
		if(close_popup_window){
			cur_img=getTitle();
			//Table.reset(dm);
			selectWindow(dm);
			run("Close");
			selectWindow(cur_img);
		}
		
		
	}else{
		print("No ROI Found!");
	} // end of if n>0
}


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res) {
	selectWindow(window_name);
	//channel_to_measure=newArray(1,2,3);
	num_channel=channel_to_measure.length;
	
	n = roiManager("count");
	if(n>0) {
		// # i.e., there is at least one valid ROI
		
		for ( ch=0; ch<num_channel; ch++ ) {
			Stack.setChannel(ch+1)
			
			for ( i=0; i<n; i++ ) { 
				roiManager("select", i); // select one ROI
				roiManager("measure");
			}
		}
		
		// save measurement
		selectWindow("Results");
		saveAs("txt", outdir + outfile_basename + "_res.txt");
		
		if(reset_res) {
			Table.reset("Results");
			selectWindow("Results");
			run("Close");
			selectWindow(window_name);
		}
		
	}else{
		print("No ROI found!");
	}
	
}

// # Misc --------------------------------------------------------------------------------

function outline2results(lbl) {
	nR = nResults;
	Roi.getCoordinates(x, y);
	for (i=0; i<x.length; i++) {
		setResult("Label", i+nR, lbl);
		setResult("X", i+nR, x[i]);
		setResult("Y", i+nR, y[i]);
	}
}



function draw_best_fitting_ellipses() {
	// source: https://microscopynotes.com/imagej/best_fit_ellipse/index.html
	// requires run("Set Measurements...", "area centroid perimeter fit shape feret's redirect=None decimal=2");
	original = getImageID;
	// originalTitle = getTitleStripExtension();
	// rename(originalTitle);
	getVoxelSize(rescale, height, depth, unit);
	
	n = roiManager("count");
	if(n>0){
		// # originally n was nResults
		for(i=0; i<n; i++) {
			roiManager("select", i); // not in original code
			// # draw ellipse
			
			List.setMeasurements;
  			//print(List.getList); // list all measurements
  			xc = List.getValue("X");
  			yc = List.getValue("Y");
  			major = List.getValue("Major");
  			minor = List.getValue("Minor");
  			angle = List.getValue("Angle");
			
			// From original
			// xc = getResult("X", i) / rescale;		
			// yc = getResult("Y", i) / rescale;
			// major = getResult("Major", i) / rescale;
			// minor = getResult("Minor", i) / rescale;
			// angle = getResult("Angle", i);
			makeOval(xc-(major/2), yc-(minor/2), major, minor);
			run("Rotate...", "  angle="+(180-angle));
				roiManager("Add"); // comment out if don't want ellipses added to ROI Manager
				roi_old_name = RoiManager.getName(i);
				// # renaming the recently added one
				new_roi_idx=n+i;
				roiManager("select", new_roi_idx); // not in original code
				roiManager("rename", roi_old_name + "_ellipse");
			// run("Overlay Options...", "stroke=cyan width=0 fill=none");
			// run("Add Selection...");
			
			// # draw axes
			//a = angle*PI/180;	// convert angle degrees to radians
			//run("Overlay Options...", "stroke=blue width=0 fill=none");
			//d = major;
      		//makeLine(xc+(d/2)*cos(a), yc-(d/2)*sin(a), xc-(d/2)*cos(a), yc+(d/2)*sin(a));
      		//run("Add Selection...");
      		//d=getResult('Minor',i);
      		//a=a+PI/2;			// rotate angle 90 degrees
      		//run("Overlay Options...", "stroke=red width=0 fill=none");
      		//d = minor;
      		//makeLine(xc+(d/2)*cos(a), yc-(d/2)*sin(a), xc-(d/2)*cos(a), yc+(d/2)*sin(a));
      		//run("Add Selection...");
		}
	}
	
	run("Select None");
}


