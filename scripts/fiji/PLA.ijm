// version 0.0.1
// last update: 2024-03-01

// # ANALYSIS CONFIGURATION ======================================================
// ## Output specification -------------------------------------------------------

// // /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251003_Nr5a2_Gata6_PLA_rep1_DAPI_TexasRed.lif
//outdir="/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Gata6_rep1/"
//out_basename="PLA_Nr5a2_Gata6-"
// Value used for manual thresholding
//nuc_min_thresh=1200;
//PLA_min_thresh=2000;

// /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251005_PLA_Nr5a2_Gata6_rep2_DAPU_Texasred.lif
//outdir="/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Gata6_rep2/"
//out_basename="PLA_Nr5a2_Gata6_rep2-"
//nuc_min_thresh=1200;
//PLA_min_thresh=2000;

//// /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251005_PLA_Nr5a2_Gata6_rep2_DAPU_Texasred.lif
//outdir="/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Klf5_rep1/"
//out_prefix="PLA_Nr5a2_Klf5_rep1-"
//nuc_min_thresh=1200;
//PLA_min_thresh=2000;


// /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251005_PLA_Nr5a2_KLF5_rep2_DAPI_TexaRed.lif
outdir="/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Klf5_rep2/"
out_prefix="PLA_Nr5a2_Klf5_rep2-"
nuc_min_thresh=5000;
PLA_min_thresh=5000;

// ## Image information --------------------------------------------------------
nuc_ch=1;
PLA_ch=2;

//nuc_thresh_method="Huang2";
nuc_thresh_method="Huang";
//nuc_thresh_method="Default";
PLA_thresh_method="Default";

only_current_z_stack = false;

// # Particle detection parameters ---------------------------------------------
// The default setting by Imre uses Huang2 for nucleus, Triangle for nucleolus
// For the list of all method, see:
//	 https://imagej.net/plugins/auto-threshold
//threshold_method_nucleus = "Huang2"
//threshold_method_nucleolus = "Default"

// ## Behavior control ---------------------------------------------------------

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



///////////////////////////////////////////////////////////////////////////////////
// TODO: Declare Functions
///////////////////////////////////////////////////////////////////////////////////

// #region ROI Utils ==============================================================

function clear_roi() {
	// Clear existing Regions of Interests (ROIs)
	run("Select None");
	if (roiManager("count")>0) {
		roiManager("Deselect");
		roiManager("Delete");
	}
}


function modify_roi_name_range(prefix, suffix, min_idx, max_idx){
	// Adding prefix or suffix to existing ROIs
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

function detect_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord, save_roi){
	// Utility function to save shape outline coordinate to file.
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


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res, save_to_file){
	// Run measurement in existing ROIs
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

// #region Misc. ==============================================================

function outline2results(lbl) {
	nR = nResults;
	Roi.getCoordinates(x, y);
	for (i=0; i<x.length; i++) {
		setResult("Label", i+nR, lbl);
		setResult("X", i+nR, x[i]);
		setResult("Y", i+nR, y[i]);
	}
}

function z_process(method, draw_roi, draw_ch, stroke_color, outfile){
	run("Z Project...", "projection=["+method+"]");
	
	// Draw ROI
	if(draw_roi){
		Stack.setChannel(draw_ch);
		setColor(stroke_color);
		n=roiManager("count");
		if(n>0){
			for(i=0; i<n; i++){
//				Roi.setStrokeColor(stroke_color);
				roiManager("draw");
			}
		}
	}
	
	// reduce scale
	run("Scale...", "x=0.5 y=0.5 width=500 height=500 interpolation=Bilinear average create");
	saveAs("tiff", outfile);
	// closing active image window
	close();
	close();
}

// #region detect_particle ====================================================

function detect_particle(PLA_ch, blur_sigma, thresh_method, do_auto_thresh, lower_thresh, upper_thresh, slice_start, slice_end, do_watershed, do_fill_hole, particle_size, close_popup_window){
	// Main function to run thresholding and particle detection.
	run("Duplicate...", "duplicate channels="+PLA_ch+" slices="+slice_start+"-"+slice_end);
	tmpt=getTitle();
	selectWindow(tmpt);
	// Bluring the nucleus
	run("Gaussian Blur...", "sigma="+blur_sigma+" stack");
	
	if(do_auto_thresh){
		// Running autothreshold
  		run("Auto Threshold", "method="+thresh_method+" ignore_black ignore_white white stack use_stack_histogram");
		//run("Close-", "stack");
	}else{
		// Running fixed thresholding
		setThreshold(lower_thresh, upper_thresh);
		// run("Threshold", "method="+thresh_method+" stack_histogram");
  		//run("Convert to Mask");
		setOption("BlackBackground", true);
		run("Convert to Mask", "method="+thresh_method+" black ");
	}
	
	if(do_fill_hole){
		run("Fill Holes", "stack");
	}
	
	// Process > Binary > Fill Holes
	if(do_watershed){
		run("Watershed", "stack");
	}
	
	setOption("BlackBackground", true);
	//run("Erode", "stack");
	run("Analyze Particles...", "size="+particle_size+" exclude include add stack");
	
	if(close_popup_window){
		// closing active image window
		close();
	}
}


///////////////////////////////////////////////////////////////////////////////////
// TODO: RUN
///////////////////////////////////////////////////////////////////////////////////
analyze_all_windows=true;
ask_before_proceed=true;
close_window_after_done=true;

print("\\Clear")


if(analyze_all_windows){
	img_list=getList("image.titles");
}else{
	// img_list=getTitle();
	img_list=newArray(1);
	img_list[0]=getTitle();
}

// Tell user what we are going to do
n_img=lengthOf(img_list);
//n_img=3; // debug
print("Analyzing "+n_img+" images ... ");
for (i = 0; i < n_img; i++) {
	print("   img["+i+"] -- "+img_list[i]);
}

// Check with user
selectWindow("Log");
if(ask_before_proceed){
//	waitForUser("Do you want to continue?");
	ans = getBoolean("Do you want to continue?", "yes", "no");
	print("ans: "+ans);
	if(!ans){
		exit();
	}
}else{
	wait_time=1000; // unit in milliseconds
	wait(wait_time);
	print(".");
	wait(wait_time);
	print("\\Update:. .");
	wait(wait_time);
	print("\\Update:. . .");
}

//exit();

for (img = 0; img < n_img; img++) {
	// print("  img "+img+": "+img_list[img]);
	cur_img_name=img_list[img];
	selectWindow(cur_img_name);
	Stack.setDisplayMode("grayscale");
	cur_name = getInfo("slice.label");
	cur_name_a = split(cur_name, "\\/");
	// Array.print(cur_name_a);
	use_name = Array.filter(cur_name_a, "Position");
	print("Image position: "+use_name[0]);
	out_basename = out_prefix + use_name[0];
	print("------------------------------------------------");
	
	
	//	print("Start Analysis for "+out_basename);
	clear_roi();
	// # Fetching some constant information
	// Getting the dimensional information of the pixel in real world unit
	getPixelSize(unit, pixelWidth, pixelHeight);
	// NB: n_slices is not the same as total number of z-stack
	//		the first slice will be the fist available z-stack in the image, thus can be any number.
	getDimensions(img_width, img_height, n_channels, n_slices, n_frames);
	
	// # get the current position of the original (ori) image viewing position
	start_window = getTitle();
	Stack.getPosition(start_ch, start_z, start_frame);
	
	// Detect Cytoplasm =============================================================================
	// TODO?
	
	// Detect Nucleus ================================================================================
	
	detect_particle(nuc_ch, 4, nuc_thresh_method, false, nuc_min_thresh, 60000, 0, "Infinity", false, true, "100-Infinity", true);
	
	
	n = roiManager("count");
	print("Detected ROI (Nucleus): "+n);
	// Add prefix to the detected ROIs
	if(n>0){
		modify_roi_name_range("nucleus_", "", 0, n);
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
			measure_roi(start_window, newArray(nuc_ch), outdir, out_basename+"_nucleus", auto_reset_results_window, save_measurement);
		}
		run("Select None");
	}else{
		print("No nucleus found!");
	}
	
	//exit();
	
	//z_process(method, draw_roi, draw_ch, stroke_color, outfile)
	z_process("Max Intensity", true, 2, "yellow", outdir+"/zproj_max_nuc_drawn/"+out_basename+"zproj_max_draw-nucleus.tiff");
	
	
	// PLA spots ========================================================================================
	clear_roi();
//	detect_particle(PLA_ch, blur_sigma, thresh_method, do_auto_thresh, lower_thresh, upper_thresh, slice_start, slice_end, do_watershed, do_fill_hole, particle_size, close_popup_window){
	detect_particle(PLA_ch, 2, PLA_thresh_method, false, PLA_min_thresh, 60000, 0, "Infinity", true, false, "0.5-10", true);
	
	n = roiManager("count");
	print("Detected ROI (PLA): "+n);
	if(n>0){
		modify_roi_name_range("PLA_", "", 0, n);
		if(extract_outline_coord){
			detect_outline_coord("PLA_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi);
		}
		selectWindow(start_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(start_window, newArray(PLA_ch), outdir, out_basename+"_PLA", auto_reset_results_window, save_measurement);
		}
		
		run("Select None");
		
	}else{
		print("No nucleoli found!");
	}
	
	
	// get z-project
	//z_process(method, draw_roi, draw_ch, stroke_color, outfile)
	z_process("Max Intensity", false, 0, 0, outdir+"/zproj_max/"+out_basename+"zproj_max.tiff");
	
	if(close_window_after_done){
		close(cur_img_name);
	}
	
	// Premature termination
	print(use_name[0]+" - Done");
	print("");
	
}

print("*** All Analysis Done! ***");
selectWindow("Log");
exit();













