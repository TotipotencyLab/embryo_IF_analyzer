// version 0.1.3
// last update: 2024-03-01

// # ANALYSIS CONFIGURATION ======================================================
// ## Output specification -------------------------------------------------------
outdir="/Users/chad/Lab/0_imaging/test/"
out_basename="test_16C_1_"


// ## Image information --------------------------------------------------------
dna_mask_ch=1;
cyto_mask_ch=1; // ignore this for now
channel_to_measure=newArray(1,2,3);
//channel_to_measure=newArray(1,2);

//# Z stack to analyze
// # Haven't implement the use of these two parameters at the moment
//use_z_stack_range=1-Infinity;
only_current_z_stack = false;


// # Particle detection parameters ---------------------------------------------
// The default setting by Imre uses Huang2 for nucleus, Triangle for nucleolus
// For the list of all method, see:
//	 https://imagej.net/plugins/auto-threshold
threshold_method_nucleus = "Huang2"
threshold_method_nucleolus = "Default"


// ## Behavior control ---------------------------------------------------------
run_mask_nucleoli = true; // if true, nucleoli outline will also be detected
keep_nuc_roi = true;       // only in effect when `run_mask_nucleoli`=true, if true, roi of nucleus detection will remain after nucleoli detection
run_mask_cytoplasm = false; // not functioning at the moment

// Testing feature; try to fit an ellipse in each detected particles
run_fit_ellipse = false; // DEPRECIATED, only keep for historical reason

// # Pop-up window behavior
//auto_set_measurement = true // DEPRECIATED
close_outline_window = true;
close_outline_table_window = true;

// # Result measurement behavior
extract_outline_coord = true;
auto_measure_results = true; // if true, will automatically measure the signal in each ROI in every channel specified in `channel_to_measure`.
// auto_reset_results_window was intended to work together with `save_measurement=true`
auto_reset_results_window = true; // if true, will reset the Results window after done with the measurement 

// # Output controls
// # Control which type of an output will be saved to file (as specified in `outdir` and `out_basename` variable).
save_roi=true;
save_outline_coord=true;
save_measurement = true;




// ===================================================================================================================================================
// # Analysis Section ================================================================================================================================
// ===================================================================================================================================================

// Reformatting user input ----------------------------------------------------
if(run_mask_nucleoli){keep_nuc_roi=true;}

// # Set_basename
// This will attempt to get the imaging position information of the current image
//		Thus, would require to have the word 'Position' within the name
cur_name = getInfo("slice.label");
cur_name = split(cur_name, "\\/");
//Array.print(cur_name);
use_name = Array.filter(cur_name, "Position");
out_basename=out_basename + use_name[0];
//out_basename=out_basename + "test"


// # Set measurement
//if(run_fit_ellipse){
//	run("Set Measurements...", "area min area_fraction limit display redirect=None decimal=3");
//}else{
//	run("Set Measurements...", "area centroid perimeter fit shape feret's redirect=None decimal=2");
//}


// # Fetching some constant information
// Getting the dimensional information of the pixel in real world unit
getPixelSize(unit, pixelWidth, pixelHeight);
// NB: n_slices is not the same as total number of z-stack
//		the first slice will be the fist available z-stack in the image, thus can be any number.
getDimensions(img_width, img_height, n_channels, n_slices, n_frames);

// # get the current position of the original (ori) image viewing position
start_window = getTitle();
Stack.getPosition(start_ch, start_z, start_frame);

// # Manage z-stack to be analyzed ----


if(only_current_z_stack){
	// Only duplicate the current z-stack
	//run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+slice_start+"-"+slice_end);
	run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+start_z);
	dup_title = getTitle();
}else{
	// # Maybe not a good idea to do this now.
	//run("Duplicate...", "duplicate channels="+dna_mask_ch+" slices="+use_z_stack_range);
	//dup_title = getTitle();
}

// # Nucleus --------------------------------------------------------------------
clear_roi();
// mask_nucleus(dna_mask_ch, sigma, particle_size, fill_hole, close_popup_window)
mask_nucleus(dna_mask_ch, 8, threshold_method_nucleus, "40-Infinity", true, close_outline_window);

n = roiManager("count");
// Add prefix to the detected ROIs
if(n>0){modify_roi_name_range("nucleus_", "", 0, n);}

// doesn't work for now
if(run_fit_ellipse){
	selectWindow(start_window);
	draw_best_fitting_ellipses();
	//selectWindow(start_window);
}

// # Measurement
// NB: Consider turning this chunk and the modify_roi_name_range() section into a function
n = roiManager("count");
if(n>0){
	if(extract_outline_coord){
		// detect_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord, save_roi)
		detect_outline_coord("nucleus_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi);
	}
	selectWindow(start_window); // come back to the main image
	if(auto_measure_results){
		measure_roi(start_window, channel_to_measure, outdir, out_basename+"_nucleus", auto_reset_results_window, save_measurement);
	}
	run("Select None");
}else{
	print("No nucleus found!");
}



// # Nucleoli --------------------------------------------------------------------
if(run_mask_nucleoli){
	if(!keep_nuc_roi){
		clear_roi();
	}
	//mask_nucleus(dna_mask_ch, 5, "3-100", false, close_outline_window);
	mask_nucloli(dna_mask_ch, 8, threshold_method_nucleolus, "3-300", "0.50-1.00", close_outline_window);
	
	// # this chunk has the same structure as the nucleus one.
	n = roiManager("count");
	if(n>0){
		modify_roi_name_range("nucleolus_", "", 0, n);
		if(extract_outline_coord){
			detect_outline_coord("nucleolus_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi);
		}
		selectWindow(start_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(start_window, channel_to_measure, outdir, out_basename+"_nucleolus", auto_reset_results_window, save_measurement);
		}
		
		run("Select None");
		
	}else{
		print("No nucleoli found!");
	}
}

// # Cytoplasm background -------------------------------------------------------
// still not working properly
if(run_mask_cytoplasm){
	clear_roi();
	mask_cytoplasm(cyto_mask_ch, 10, close_outline_table_window);
	
}


// # Close temporary window

if(only_current_z_stack){
	close(dup_title);
}

// Return to the original window, channel, and slice (z-stack)
selectWindow(start_window);
Stack.setChannel(start_ch)
Stack.setSlice(start_z);
//Stack.setDimensions(start_ch, start_z, start_frame); // will collapse the hyperstack


print("Analysis Done!");


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

function mask_nucleus(dna_mask_ch, sigma, method, particle_size, fill_hole, close_popup_window){
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

// # ROI processing -----------------------------------------------------------------

function detect_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord, save_roi){
	dm=dm_name;
	// # A placeholder for our outline table
	Table.create(dm);
	
	// # This should be unnecessary 	
	//getPixelSize(unit, pixelWidth, pixelHeight);
	
	n = roiManager("count");
	if(n>0) {
		cnt=0; // for line count?
		
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
		if(save_coord){
			Table.save(outdir+outfile_basename+"_"+dm_name+".txt");	
		}
		
		if(save_roi){
			// # save all currently available ROI object
			roiManager("Save", outdir + outfile_basename + "_" + dm_name + "_ROIs.zip");
		}
		
		
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


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res, save_to_file) {
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
		if(save_to_file){
			saveAs("txt", outdir + outfile_basename + "_res.txt");
		}
		
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


