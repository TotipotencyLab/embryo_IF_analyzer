// version 0.1.4
// last update: 2026-09-14
//
// CHANGELOG (0.1.4)
//  - FIX: measure_roi() measured channels 1..N instead of the channels listed
//         in `channel_to_measure`. The array contents are now honoured.
//  - FIX: outline y-coordinates were scaled by pixelWidth instead of
//         pixelHeight (wrong on anisotropic pixels).
//  - FIX: the "Position" token lookup crashed on images whose slice label
//         carries no match. Now configurable, with a fallback to the title.
//  - FIX: the nucleolus pass re-prefixed every ROI in the manager, including
//         the nucleus ROIs kept by keep_nuc_roi, producing names like
//         "nucleolus_nucleus_0011-0001-0442". Only the newly added ROIs are
//         prefixed now.
//  - FIX: with keep_nuc_roi=true the nucleolus outline/measurement files also
//         contained every nucleus ROI, duplicating the nucleus outputs.
//         detect_outline_coord() and measure_roi() now take an ROI index range
//         so each feature writes only its own ROIs.
//  - Set Measurements is now issued explicitly. It is a PERSISTENT Fiji user
//         preference, so relying on it made output columns machine-dependent.
//  - Detection parameters (blur sigma, size, circularity) lifted out of the
//         analysis section into the configuration block, per feature.

// # ANALYSIS CONFIGURATION ======================================================
// ## Output specification -------------------------------------------------------
outdir="/Users/chad/Lab/dev/embryo_IF_analyzer/fixture/if_data/res_fiji/"
out_basename="FJ_"


// ## Image information --------------------------------------------------------
// Token used to locate the imaging position inside the slice label.
// Leica .lif series carry e.g. ".../Position003". Set to "" to always fall
// back to the image window title instead.
position_pattern = "Position";

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
threshold_method_nucleolus = "Triangle"

// ## Per-feature detection settings
// Nucleus
nucleus_blur_sigma    = 8;
nucleus_particle_size = "80-Infinity";
nucleus_fill_holes    = true;

// Nucleolus
// NB: nucleoli are far smaller than nuclei. A blur sigma tuned for nuclei
//     erodes them below the minimum particle size, so small nucleoli go
//     undetected and the rest measure smaller than they really are. Tune this
//     independently of the nucleus sigma -- ~5 has worked better historically.
nucleolus_blur_sigma           = 3;
nucleolus_particle_size        = "3-150";
nucleolus_particle_circularity = "0.50-1.00";
// Compute one threshold from the whole stack rather than per z-slice, as
// mask_nucleus() already does. Without it each slice gets its own threshold, so
// a slice where the nucleolus is out of focus or absent thresholds on
// essentially nucleolus-free data -- making nucleolus size drift across z and
// corrupting the downstream z-merge.
// Pooling the histogram across z made things worse in practice (no nucleoli
// detected with either Default or Triangle): most slices contain little or no
// nucleolus, so the pooled population is dominated by slices with nothing to
// find. Left off; this is likely why it was absent originally.
nucleolus_use_stack_histogram  = false;

// Compute the nucleolus threshold from NUCLEAR PIXELS ONLY, using the nucleus
// ROIs detected in the previous pass.
// Rationale: nucleoli are dark in DAPI, so the old path inverted the image and
// thresholded the whole frame -- which also made the large extranuclear
// background the brightest thing present, so the auto-threshold ended up
// separating background from nucleus rather than nucleolus from nucleoplasm.
// Restricting the histogram to inside the nuclei removes the background from
// the calculation entirely, and lets us drop the inversion (setAutoThreshold
// can target the dark end directly).
// KNOWN BROKEN -- SUPERSEDED. Leave this false.
//   This path computes ONE threshold, from ONE arbitrary slice, over the
//   z-FLATTENED union of all nucleus ROIs, then applies it to every slice. In
//   practice that detects noise in empty space (a nucleus at one z fences in
//   background at another) and whole nuclei as nucleoli (a threshold from one
//   slice is wrong where the nucleus is dimmer).
//   Doing it properly needs a per-nucleus, per-slice loop, which the IJ1 macro
//   language cannot express cleanly -- the ROI Manager is the only way to hold a
//   set of ROIs. That is why this moved to Groovy; see
//   scripts/groovy/NucleolusDetect.groovy, which is the supported version.
//   Kept here, disabled, as a record of why.
nucleolus_restrict_to_nucleus  = false;

// ## Measurements --------------------------------------------------------------
// Fiji's Set Measurements is a persistent USER PREFERENCE, not a per-macro
// setting. A macro that relies on whatever the operator happens to have ticked
// produces different columns on different machines, which silently breaks the
// downstream R scripts. Setting it explicitly makes the output schema
// deterministic. This list reproduces the column set the R scripts expect:
//   Label Area Mean StdDev Min Max X Y Circ. IntDen Median RawIntDen
//   Ch Slice AR Round Solidity
// NB: this overwrites the operator's Fiji preference, and it persists.
measurement_fields  = "area mean standard min centroid shape integrated median stack display";
measurement_decimal = 3;


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
// Derive a per-image identifier for the output filenames. Prefers a token
// matching `position_pattern` in the slice label; falls back to the image
// title so the macro also runs on data named differently (e.g. "...-S8-ch2.tif").
out_basename = out_basename + resolve_image_id(position_pattern);


// # Set measurement
// Force the measurement set so the output columns do not depend on the
// operator's Fiji preferences. See `measurement_fields` above.
run("Set Measurements...", measurement_fields + " redirect=None decimal=" + measurement_decimal);


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
// mask_nucleus(dna_mask_ch, sigma, method, particle_size, fill_hole, close_popup_window)
mask_nucleus(dna_mask_ch, nucleus_blur_sigma, threshold_method_nucleus, nucleus_particle_size, nucleus_fill_holes, close_outline_window);

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
		detect_outline_coord("nucleus_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi, 0, n);
	}
	selectWindow(start_window); // come back to the main image
	if(auto_measure_results){
		measure_roi(start_window, channel_to_measure, outdir, out_basename+"_nucleus", auto_reset_results_window, save_measurement, 0, n);
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
	// NB: capture the ROI count BEFORE detection. With keep_nuc_roi=true the
	//     nucleus ROIs are still in the manager, so renaming the full 0..n range
	//     re-prefixed them as "nucleolus_nucleus_...". Only prefix the new ones.
	n_before = roiManager("count");
	mask_nucloli(dna_mask_ch, nucleolus_blur_sigma, threshold_method_nucleolus, nucleolus_particle_size, nucleolus_particle_circularity, nucleolus_use_stack_histogram, nucleolus_restrict_to_nucleus, 0, n_before, close_outline_window);
	
	// # this chunk has the same structure as the nucleus one.
	n = roiManager("count");
	if(n > n_before){
		print("Detected ROI (nucleolus): " + (n - n_before));
		modify_roi_name_range("nucleolus_", "", n_before, n);
		if(extract_outline_coord){
			detect_outline_coord("nucleolus_outline", outdir, out_basename, close_outline_table_window, save_outline_coord, save_roi, n_before, n);
		}
		selectWindow(start_window); // come back to the main image
		if(auto_measure_results){
			measure_roi(start_window, channel_to_measure, outdir, out_basename+"_nucleolus", auto_reset_results_window, save_measurement, n_before, n);
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
// Naming ------------------------------------------------------------------------

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


function mask_nucloli(dna_mask_ch, sigma, method, particle_size, particle_circularity, use_stack_hist, restrict_to_nuc, nuc_min, nuc_max, close_popup_window){
	// recommended input value:
	//		sigma: 5
	//		particle_size: 3-300
	//		circularity: 0.50-1.00
	
	run("Duplicate...", "duplicate channels="+dna_mask_ch);
	tmpt=getTitle();
	selectWindow(tmpt);
	
	if(restrict_to_nuc && (nuc_max > nuc_min)){
		// --- threshold from nuclear pixels only, no inversion ----------------
		run("Gaussian Blur...", "sigma="+sigma+" stack");

		// union of the nucleus ROIs from the previous pass
		sel = newArray(nuc_max - nuc_min);
		for(k=0; k<sel.length; k++){ sel[k] = nuc_min + k; }
		roiManager("deselect");
		roiManager("select", sel);
		if(sel.length > 1){ roiManager("Combine"); }

		// NB: setAutoThreshold() is the BUILT-IN macro function, not the
		//     "Auto Threshold" plugin used below. It computes its histogram
		//     from the ACTIVE SELECTION, which is the entire point here.
		//     No "dark" modifier => it targets the dark end, i.e. the nucleoli.
		thresh_opt = method;
		if(use_stack_hist){ thresh_opt = thresh_opt + " stack"; }
		setAutoThreshold(thresh_opt);
		getThreshold(nucleolus_lo, nucleolus_hi);
		print("   nucleolus threshold (within nuclei): [" + nucleolus_lo + ", " + nucleolus_hi + "]");

		// apply over the frame, then discard anything outside the nuclei
		run("Select None");
		setThreshold(nucleolus_lo, nucleolus_hi);
		setOption("BlackBackground", true);
		run("Convert to Mask", "background=Light black");

		roiManager("select", sel);
		if(sel.length > 1){ roiManager("Combine"); }
		run("Clear Outside", "stack");
		run("Select None");

	}else{
		// --- original behaviour: invert, then threshold the whole frame ------
		run("Invert", "stack");
		run("Gaussian Blur...", "sigma="+sigma+" stack");
		// run("Auto Threshold", "method=Triangle white stack");
		thresh_opt = "method="+method+" white stack";
		if(use_stack_hist){ thresh_opt = thresh_opt + " use_stack_histogram"; }
		run("Auto Threshold", thresh_opt);
		run("Close-", "stack");
		//run("Dilate", "stack");
	}

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

function detect_outline_coord(dm_name, outdir, outfile_basename, close_popup_window, save_coord, save_roi, min_idx, max_idx){
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


function measure_roi(window_name, channel_to_measure, outdir, outfile_basename, reset_res, save_to_file, min_idx, max_idx) {
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


