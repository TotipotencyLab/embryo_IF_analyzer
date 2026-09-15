// last update: 2026-09-14
//
// CHANGELOG
//  - FIX: measure_roi() measured channels 1..N rather than the channels listed
//         in `channel_to_measure`. The array contents are now honoured.
//  - FIX: callers passed newArray(nuc_ch) / newArray(PLA_ch). In the IJ1 macro
//         language newArray(n) builds an array of LENGTH n filled with zeros,
//         NOT a one-element array holding n. newArray(PLA_ch) with PLA_ch=2
//         therefore produced a 2-element array and the PLA measurement was
//         written for channels 1 AND 2. Use channel_array() instead.
//         NB: re-running this macro now yields _PLA_res.txt with half the rows
//         of the files behind the published analysis. The counts are unchanged
//         (downstream R de-duplicates ROI ids), but the files differ.
//  - FIX: outline y-coordinates were scaled by pixelWidth instead of pixelHeight.
//  - FIX: the "Position" token lookup crashed on images whose slice label has
//         no match. Now configurable, with a fallback to the image title.
//  - Set Measurements is now issued explicitly. This macro previously set it
//         nowhere at all, so the output columns depended entirely on whatever
//         the operator had ticked in Fiji.
//  - FIX: z_process() hardcoded width=500 height=500, silently mis-scaling any
//         image that was not 1000x1000, and closed two windows positionally.
//         Now derived from the image, and closed by title.

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
// Token used to locate the imaging position inside the slice label.
// Leica .lif series carry e.g. ".../Position003". Set to "" to always fall
// back to the image window title instead.
position_pattern = "Position";

nuc_ch=1;
PLA_ch=2;

// ## Measurements --------------------------------------------------------------
// Fiji's Set Measurements is a persistent USER PREFERENCE, not a per-macro
// setting, so relying on it makes the output columns machine-dependent and
// silently breaks the downstream R scripts. Setting it explicitly here makes
// the output schema deterministic. Reproduces the expected column set:
//   Label Area Mean StdDev Min Max X Y Circ. IntDen Median RawIntDen
//   Ch Slice AR Round Solidity
// NB: this overwrites the operator's Fiji preference, and it persists.
measurement_fields  = "area mean standard min centroid shape integrated median stack display";
measurement_decimal = 3;

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

// #region Naming =================================================================
// NB: duplicated verbatim from nucleus_selector.ijm -- the IJ1 macro language
//     has no import mechanism. This is the duplication the Groovy port removes.

function resolve_image_id(pattern){
	// Return an identifier for the current image, for use in output filenames.
	// Prefers a token matching `pattern` inside the slice label; falls back to
	// the image window title when the label carries no match (or is empty).
	label = getInfo("slice.label");
	if( (pattern != "") && (label != "") ){
		parts = split(label, "\\/");
		matched = Array.filter(parts, pattern);
		if(lengthOf(matched) > 0){
			// NB: assign before returning. The IJ1 macro interpreter cannot infer
			//     a string return type through a nested user-function call, so
			//     "return sanitize_name(...)" fails with "Numeric return value
			//     expected". Do not collapse these two lines.
			id = sanitize_name(matched[0]);
			return id;
		}
	}
	print("No '" + pattern + "' token in slice label -- falling back to image title.");
	id = sanitize_name(getTitle());
	return id;
}

function sanitize_name(s){
	// Drop a trailing image extension and replace characters that are unsafe
	// or awkward inside a filename.
	s = replace(s, "\\.(tif|tiff|lif|lifext|czi|nd2)$", "");
	s = replace(s, "[\\/\\\\:\\*\\?\"<>\\|]", "_");
	s = replace(s, "^\\s+", "");
	s = replace(s, "\\s+$", "");
	return s;
}

function channel_array(ch){
	// Build a genuine one-element array holding the channel number.
	// NB: newArray(n) creates an array of LENGTH n filled with zeros -- it does
	//     NOT create a one-element array containing n. Writing newArray(2) to
	//     mean "channel 2" silently yields a 2-element array instead.
	a = newArray(1);
	a[0] = ch;
	return a;
}

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

function detect_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord, save_roi, min_idx, max_idx){
	// Utility function to save shape outline coordinate to file.
	dm=dm_name;
	// # A placeholder for our outline table
	Table.create(dm);
	
	// # This should be unnecessary 	
	//getPixelSize(unit, pixelWidth, pixelHeight);
	
	n = roiManager("count");
	if(max_idx > min_idx) {
		cnt=0; // for line count?
		
		// # Saving the outline into a text file
		// NB: only ROIs in [min_idx, max_idx). Previously this walked the whole
		//     manager, so with keep_nuc_roi=true the nucleolus outputs also
		//     contained every nucleus ROI.
		for ( i=min_idx; i<max_idx; i++ ) { 
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
				Table.set("y", cnt, ypoints[noc]*pixelHeight, dm);
				cnt++;
			}
			
		} // end of for loop
		
		// # Save to file
		Table.update(dm);
		if(save_coord){
			Table.save(outdir+outfile_basename+"_"+dm_name+".txt");	
		}
		
		if(save_roi){
			// # save only the ROIs belonging to this feature
			roiManager("deselect");
			sel = newArray(max_idx - min_idx);
			for(k=0; k<sel.length; k++){ sel[k] = min_idx + k; }
			roiManager("select", sel);
			roiManager("save selected", outdir + outfile_basename + "_" + dm_name + "_ROIs.zip");
			roiManager("deselect");
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


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res, save_to_file, min_idx, max_idx){
	// Run measurement in existing ROIs
	selectWindow(window_name);
	//channel_to_measure=newArray(1,2,3);
	num_channel=channel_to_measure.length;
	
	n = roiManager("count");
	if(max_idx > min_idx) {
		// # i.e., there is at least one valid ROI in the requested range
		
		for ( ch=0; ch<num_channel; ch++ ) {
			// NB: use the channel NUMBERS supplied by the caller. This loop
			//     previously did setChannel(ch+1), i.e. it measured channels
			//     1..N and silently ignored which channels were requested.
			Stack.setChannel(channel_to_measure[ch]);
			
			for ( i=min_idx; i<max_idx; i++ ) { 
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
	// make sure the destination folder exists before saveAs()
	out_parent = File.getParent(outfile);
	if(!File.exists(out_parent)){ File.makeDirectory(out_parent); }

	run("Z Project...", "projection=["+method+"]");
	proj_title = getTitle();
	
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
	
	// reduce scale to half size, derived from the actual image dimensions.
	// NB: this previously read "width=500 height=500", which silently produced
	//     a wrong-sized output for anything that was not a 1000x1000 image.
	getDimensions(zw, zh, zc, zs, zf);
	run("Scale...", "x=0.5 y=0.5 width="+round(zw/2)+" height="+round(zh/2)+" interpolation=Bilinear average create");
	saveAs("tiff", outfile);
	scaled_title = getTitle(); // saveAs() renames the active window

	// close by title rather than positionally -- the old "close(); close();"
	// closed whatever happened to be frontmost, including the source image if
	// Scale... did not open a new window.
	close(scaled_title);
	if(isOpen(proj_title)){ close(proj_title); }
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
		// VERIFY: passing method= to "Convert to Mask" below may cause ImageJ to
		//   recompute the threshold and discard the manual values set here, which
		//   would defeat the point of the fixed-threshold branch. Left unchanged
		//   pending an empirical check: run one image with lower_thresh set very
		//   high and confirm the mask actually goes (nearly) empty. If it does
		//   not, drop the method= argument.
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

// Process images without drawing them to screen. Substantially faster, but it
// is a behavioural change: verify on a single image before trusting a batch,
// and before regenerating any reference/fixture output. Left off by default.
use_batch_mode=false;

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

// Force the measurement set so output columns do not depend on the operator's
// Fiji preferences. See `measurement_fields` above.
run("Set Measurements...", measurement_fields + " redirect=None decimal=" + measurement_decimal);

if(use_batch_mode){ setBatchMode(true); }

for (img = 0; img < n_img; img++) {
	// print("  img "+img+": "+img_list[img]);
	cur_img_name=img_list[img];
	selectWindow(cur_img_name);
	Stack.setDisplayMode("grayscale");
	img_id = resolve_image_id(position_pattern);
	print("Image id: "+img_id);
	out_basename = out_prefix + img_id;
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
			detect_outline_coord("nucleus_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi, 0, n);
		}
		selectWindow(start_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(start_window, channel_array(nuc_ch), outdir, out_basename+"_nucleus", auto_reset_results_window, save_measurement, 0, n);
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
			detect_outline_coord("PLA_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi, 0, n);
		}
		selectWindow(start_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(start_window, channel_array(PLA_ch), outdir, out_basename+"_PLA", auto_reset_results_window, save_measurement, 0, n);
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
	print(img_id+" - Done");
	print("");
	
}

if(use_batch_mode){ setBatchMode(false); }

print("*** All Analysis Done! ***");
selectWindow("Log");
exit();













