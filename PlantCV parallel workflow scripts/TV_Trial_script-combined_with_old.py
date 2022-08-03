#!/usr/bin/env python
# coding: utf-8

# In[1]:


import sys, traceback
import cv2
import os
import re
import numpy as np
import argparse
import string
from plantcv import plantcv as pcv

def options():
    parser = argparse.ArgumentParser(description="Imaging processing with opencv")
    parser.add_argument("-i", "--image", help="Input image file.", required=True)
    parser.add_argument("-o", "--outdir", help="Output directory for image files.", required=False)
    parser.add_argument("-r","--result", help="result file.", required= False )
    parser.add_argument("-r2","--coresult", help="result file.", required= False )
    parser.add_argument("-w","--writeimg", help="write out images.", default=False, action="store_true")
    parser.add_argument("-D", "--debug", help="Turn on debug, prints intermediate images.", action="store_true")
    args = parser.parse_args()
    return args

def main():
	# Get options
	args = options()

	# Set debug to the global parameter
	pcv.params.debug = args.debug


	# In[4]:


	# ds = ['/home/mpope/internship_2021/paspalum_images/EK001_N_062618/snapshot66656/VIS_TV_z1_h0_g0_e100_166960_0.png']
	ds = ['/home/mpope/internship_2021/paspalum_images/EK001_N_062618/snapshot103200/VIS_TV_z1_h0_g0_e100_276934_0.png']
	# ds = ['./VIS_TV_z1_h0_g0_e100_166960_0_crop.png']


	# Visualize pixels in channels `h` and `b` (only available in PlantCV 4.x)

	# In[5]:


	# pcv.visualize.pixel_scatter_plot(ds, x_channel='h', y_channel='b')


	# In[6]:


	img, _, filename = pcv.readimage(args.image)


	# ### 1. Detect blue tray in order to segment the inside and outside separately

	# In[7]:


	img_b = pcv.rgb2gray_lab(img, channel='b')


	# In[8]:


	tray = pcv.threshold.binary(img_b, threshold=110, max_value=255, object_type='dark')


	# In[9]:


	tray_fill = pcv.fill(tray, size=500)


	# In[10]:


	tray_id_obj, tray_id_hierarchy = pcv.find_objects(img=img, mask=tray_fill)


	# ROI containing the blue tray and some of the plant in case it has disconnected pieces

	# In[11]:


	tray_roi, tray_roi_h = pcv.roi.rectangle(img=img, x=900, y=650, h=740, w=740)


	# In[12]:


	tray_obj, tray_h, tray_mask, _ = pcv.roi_objects(img,tray_roi,tray_roi_h,tray_id_obj,tray_id_hierarchy,'partial')


	# In[13]:


	tray_idx_row, tray_idx_col = np.where(tray_mask!=0)


	# In[14]:


	tray_p1x = np.amin(tray_idx_col)
	tray_p1y = np.amin(tray_idx_row)
	tray_p2x = np.amax(tray_idx_col)
	tray_p2y = np.amax(tray_idx_row)


	# In[15]:


	_,tray_mask,_,_ = pcv.rectangle_mask(img, p1=(tray_p1x,tray_p1y), p2=(tray_p2x,tray_p2y), color="white")


	# ### 2. Segment the inside of the tray

	# In[16]:


	img_h = pcv.rgb2gray_hsv(img, channel='h')


	# In[17]:


	# range of threshold in the h channel
	bin_img_h, _ = pcv.threshold.custom_range(img, lower_thresh=[18,0,0], upper_thresh=[75,255,255], channel='HSV')


	# In[18]:


	bin_inside_tray = pcv.logical_and(bin_img_h, tray_mask)


	# In[19]:


	bin_inside_tray_fill = pcv.fill(bin_inside_tray, size=500)


	# In[20]:


	overlay = pcv.visualize.overlay_two_imgs(img1=img, img2=bin_inside_tray_fill, alpha=0.6)


	# ### New3.try old method to segment outside tray

	# In[21]:


	############Threshold Background subtracted image##############
	#STEP 2: EXTRACT CHANNELS AND GENERATE MASKS
	l = pcv.rgb2gray_lab(rgb_img=img, channel='l')
	l_mblur = pcv.median_blur(gray_img =l, ksize =2)
	#pcv.visualize.histogram(gray_img=l_mblur, mask=None, bins=256, color='red', title=None)
	pcv.visualize.histogram(l_mblur, mask=None, bins=256, title=None)
	l_thresh = pcv.threshold.binary(gray_img =l_mblur, threshold =50, max_value =255, object_type ='light')


	# In[22]:


	############START WITH VIS##############
	#STEP 2: EXTRACT CHANNELS AND GENERATE MASKS
	#THRESHOLD 1: SATURATION
	# Convert RGB to HSV and extract the Saturation channel
	s = pcv.rgb2gray_hsv(rgb_img=img, channel='s')
	# Threshold the Saturation image
	s_thresh = pcv.threshold.binary(gray_img =s, threshold =80, max_value =190, object_type ='light')
	# Median Filter
	s_mblur = pcv.median_blur(gray_img =s_thresh, ksize =1)
	# s_erd = pcv.erode(gray_img=s_mblur, ksize=5, i=1)


	# In[23]:


	#THRESHOLD 2: BLUE-YELLOW
	# Convert RGB to LAB and extract the Blue-yellow channel
	b = pcv.rgb2gray_lab(rgb_img=img, channel='b')
	# Threshold the blue-yellow image
	b_thresh = pcv.threshold.binary(gray_img=b, threshold=140, max_value=180, object_type='light')


	# In[24]:


	#THRESHOLD 3: hue
	# Convert RGB to LAB and extract the Green-Magenta channel
	h = pcv.rgb2gray_hsv(rgb_img=img, channel='h')
	pcv.visualize.histogram(h, mask=None, bins=256, title=None)

	# Threshold the green-magenta
	h_thresh = pcv.threshold.binary(gray_img=h, threshold=40, max_value=300, object_type='light')


	# In[25]:


	#Combine masks
	# Join the thresholded saturation and blue-yellow masks
	bs = pcv.logical_and(bin_img1=s_mblur, bin_img2=b_thresh)
	# Join the combined thresholded saturation and blue-yellow mask with green-magenta
	#bsa = pcv.logical_and(bin_img1=bs, bin_img2=a_thresh)


	# In[26]:


	#Refine masks, apply to image
	# Fill small noise
	bs_fill1 = pcv.fill(bin_img=bs, size=2)
	# Dilate to join small objects with larger ones
	bs_fill2=pcv.dilate(gray_img=bs_fill1, ksize=3, i=2)
	# Fill dilated image mask
	bs_fill3=pcv.fill(bin_img=bs_fill2,size=150)
	# Apply Mask (for vis images, mask_color=white)
	masked = pcv.apply_mask(img=img, mask=bs_fill3, mask_color='white')


	# In[48]:


	not_tray_mask = pcv.invert(tray_mask)
	outside_mask = pcv.logical_and(bin_img1=bs_fill3, bin_img2=not_tray_mask)


	# In[49]:


	#STEP 3: SELECT OBJECTS
	# Identify objects: find objects in the image masked, while applying the bsa_fill3 mask.
	id_objects,obj_hierarchy = pcv.find_objects(img=img, mask=outside_mask)


	# In[50]:


	roi_objects, hierarchy3, kept_mask, obj_area = pcv.roi_objects(img,tray_roi,tray_roi_h,id_objects,obj_hierarchy,'partial')


	# In[51]:


	#composed_mask = pcv.logical_or(bin_img1=bin_inside_tray_fill, bin_img2=kept_mask)


	# # ### 3. Segment the outside of the tray

	# # In[ ]:





	# # In[ ]:


	# img_a = pcv.rgb2gray_lab(rgb_img=img, channel='a')


	# # In[ ]:


	# img_a_mblur = pcv.median_blur(gray_img=img_a, ksize=5)


	# # In[ ]:


	# img_a_bin = pcv.threshold.binary(gray_img=img_a_mblur, threshold=120, max_value=255, object_type='dark')


	# # In[ ]:


	# img_a_bin_fill = pcv.fill(bin_img=img_a_bin, size=100)


	# # In[ ]:


	# id_objects, obj_hierarchy = pcv.find_objects(img=img, mask=img_a_bin_fill)


	# # In[ ]:


	# roi_objects, hierarchy3, kept_mask, obj_area = pcv.roi_objects(img,tray_roi,tray_roi_h,id_objects,obj_hierarchy,'partial')


	# ### 4. Combine masks

	# In[52]:


	composed_mask = pcv.logical_or(bin_img1=bin_inside_tray_fill, bin_img2=kept_mask)


	# In[53]:


	composed_mask_obj, composed_mask_h = pcv.find_objects(img, composed_mask)


	# In[54]:


	#STEP 4: ANALYZE IMAGE
	############## VIS Analysis: size height color etc ################

	#this is just an option to make the debug pic easier to see
	pcv.params.line_thickness = 5

	#outfile=False
	if args.writeimg==True:
		outfile=args.outdir+"/"+filename

	#if pot is empty (masked image has no objects), return everything as 0 instead of crashing
	if np.sum(composed_mask) == 0:
		pcv.outputs.add_observation(variable='height', trait='height',
															method='plantcv.plantcv.analyze_object', scale='pixels', datatype=int,
															value=0, label='pixels')
	else:

	# If pot not empty: object combine kept objects
	#apply object (roi_objects, hierarchy3) to the raw image (img), outputting an object-based mask
		obj, mask = pcv.object_composition(img=img, contours=composed_mask_obj, hierarchy=composed_mask_h)

		# Find shape properties, output shape image (optional)
		#analyze object (obj) in raw image (img) with final object-based mask applied (mask)
		shape_img = pcv.analyze_object(img=img, obj=obj, mask=mask)

		# Determine color properties: Histograms, Color Slices and Pseudocolored Images, output color analyzed images (optional)
		color_img= pcv.analyze_color(rgb_img=img, mask=mask, hist_plot_type=None)


	# In[ ]:


	# write out

	#outimg = pcv.visualize.overlay_two_imgs(img, composed_mask, alpha=0.3)
	#pcv.print_image(outimg,args.outdir+"/"+filename)
	pcv.outputs.save_results(args.result,outformat="json")

if __name__ == '__main__':
    main()
