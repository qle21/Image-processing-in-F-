# 1 PPM files are an uncommon file format, so if you double-click on a “.ppm” image file you will be unable to view it.Viewing PPM files, which you’ll need to do for testing purposes…Your assignment is to implement the five image processing  functions found in the F# file “Library.fs”: Grayscale, Threshold, FlipHorizontal, EdgeDetect, and  RightRotate90. Here’s the contents of that file. Do not change the API in any way: do not add parameters, do not change their types, etc. I will be grading your F# library against our own test suite, and so your API must match what is given on the next page:

namespace ImageLibrary
module Operations =
 //
 // all functions must be indented
 //
 //
 // Grayscale:
 //
 // Converts the image into grayscale and returns the 
 // resulting image as a list of lists. Pixels in grayscale
 // have the same value for each of the Red, Green and Blue
 // values in the RGB value. Conversion to grayscale is done
 // by using a WEIGHTED AVERAGE calculation. A normal average
 // (adding the three values and dividing by 3) is NOT the best,
 // since the human eye does not perceive the brightness of 
 // red, green and blue the same. The human eye perceives 
 // green as brighter than red and it perceived red as brighter
 // than blue. Research has shown that the following weighted
 // values should be used when calculating grayscale.
 // - the green value should account for 58.7% of the grayscale amount.
 // - the red value should account for 29.9% of the grayscale amount.
 // - the blue value should account for 11.4% of the grayscale amount.
 //
 // So if the RGB values were (25, 75, 250), the grayscale amount 
 // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
 // and then all three RGB values would become 80 or (80, 80, 80).
 // We will use truncation to cast from the floating point result 
 // to the integer grayscale value.
 //
 // Returns: updated image.
 //
let rec Grayscale (width:int) (height:int) (depth:int) (image:(int*int*int) list list) =
        let calculateGray (r, g, b) = 
            let gray = int (0.299 * float r + 0.587 * float g + 0.114 * float b)
            (gray, gray, gray)
        
        List.map (List.map calculateGray) image

 //
 // Threshold
 //
 // Thresholding increases image separation --- dark values 
 // become darker and light values become lighter. Given a 
 // threshold value in the range 0 < threshold < color depth,
 // each RGB value is compared to see if it's > threshold.
 // If so, that RGB value is replaced by the color depth;
 // if not, that RGB value is replaced with 0. 
 //
 // Example: if threshold is 100 and depth is 255, then given 
 // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
 //
 // Returns: updated image.
 //
 let rec Threshold (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (threshold:int) =
        let applyClamp value = if value > threshold then depth else 0
        List.map (List.map (fun (r, g, b) -> (applyClamp r, applyClamp g, applyClamp b))) image
 //
 // FlipHorizontal:
 //
 // Flips an image so that what’s on the left is now on 
 // the right, and what’s on the right is now on the left. 
 // That is, the pixel that is on the far left end of the
 // row ends up on the far right of the row, and the pixel
 // on the far right ends up on the far left. This is 
 // repeated as you move inwards toward the row's center.
 //
 // Returns: updated image.
 //
 let rec FlipHorizontal (width:int) (height:int) (depth:int) (image:(int*int*int) list list) =
        List.map List.rev image

 //
 //
 // Edge Detection:
 //
 // Edge detection is an algorithm used in computer vision to help
 // distinguish different objects in a picture or to distinguish an
 // object in the foreground of the picture from the background.
 //
 // Edge Detection replaces each pixel in the original image with
 // a black pixel, (0, 0, 0), if the original pixel contains an 
 // "edge" in the original image. If the original pixel does not
 // contain an edge, the pixel is replaced with a white pixel 
 // (255, 255, 255).
 //
 // An edge occurs when the color of pixel is "signigicantly different"
 // when compared to the color of two of its neighboring pixels. 
 // We only compares each pixel in the image with the 
 // pixel immediately to the right of it and with the pixel
 // immediately below it. If either pixel has a color difference
 // greater than a given threshold, then it is "significantly
 // different" and an edge occurs. Note that the right-most column
 // of pixels and the bottom-most column of pixels can not perform
 // this calculation so the final image contain one less column
 // and one less row than the original image.
 //
 // To calculate the "color difference" between two pixels, we
 // treat the each pixel as a point on a 3-dimensional grid and
 // we calculate the distance between the two points using the
 // 3-dimensional extension to the Pythagorean Theorem.
 // Distance between (x1, y1, z1) and (x2, y2, z2) is
 // sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
 //
 // The threshold amount will need to be given, which is an 
 // integer 0 < threshold < 255. If the color distance between
 // the original pixel either of the two neighboring pixels 
 // is greater than the threshold amount, an edge occurs and 
 // a black pixel is put in the resulting image at the location
 // of the original pixel. 
 //
 // Returns: updated image.
 //
 let rec EdgeDetect (width:int)
 (height:int)
 (depth:int)
 (image:(int*int*int) list list)
 (threshold:int) = 
 // for now, just return the image back, i.e. do nothing:
 image
 //
 // RotateRight90:
 //
 // Rotates the image to the right 90 degrees.
 //
 // Returns: updated image.
 //
 let rec RotateRight90 (width:int)
 (height:int)
(depth:int)
(image:(int*int*int) list list) = 
 // for now, just return the image back, i.e. do nothing:
 image




++Detail 
Grayscale (width:int) (height:int) (depth:int) (image:(int*int*int) list list): 
This function converts the image into grayscale and returns the resulting image as a list of lists. 
Conversion to grayscale is done by calculating a weighted average of the RGB values for a pixel, and 
then replacing them all by that average. The weights used for this calculation are 29.9% for red, 58.7% 
for green and 11.4% for blue. This is because the human eye perceives green as brighter than red and 
it perceives red as brighter than blue. So if the RGB values were (25, 75, 250), the average would be 
80. Since (25 * 0.299) + (75 * 0.587) + (250 * 0.114) is 80. Then all three RGB values would become 80 
— i.e. (80, 80, 80). 

# 2. Threshold (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (threshold:int): 
Thresholding increases image separation --- dark values become darker and light values become 
lighter. Given a threshold value in the range 0 < threshold < color depth, any RGB value > threshold is 
set to the color depth (e.g. 255), while any RGB value <= threshold is set to 0. Example: assuming a 
threshold of 100 and a depth of 255, the pixel (80, 120, 160) is transformed to the pixel (0, 255, 255). 
Given a grayscale image, after thresholding the image becomes black and white. Another example: 
given the cake image, the left is the result with a threshold value of 50, and the right with a threshold 
value of 200:

# 3. FlipHorizontal (width:int) (height:int) (depth:int) (image:(int*int*int) list list): 
This function flips an image so that what’s on 
the left is now on the right, and what’s on the 
right is now on the left. That is, the pixel that is 
on the far left end of the row ends up on the far 
right of the row, and the pixel on the far right 
ends up on the far left. This is repeated as you 
move inwards toward the center of the row; 
remember to preserve RGB order for each pixel 
as you flip ― you flip pixels, not individual RGB 
colors. the cake flipped horizontally:

# 4. EdgeDetect (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (threshold:int): 
Edge detection is an algorithm used in computer vision to help distinguish different objects in a picture 
or to distinguish an object in the foreground of the picture from the background. Edge Detection 
replaces each pixel in the original image with a black pixel, (0, 0, 0), if the original pixel contains an 
"edge" in the original image. If the original pixel does not contain an edge, the pixel is replaced with a 
white pixel (255, 255, 255). 
An edge occurs when the color of pixel is "significantly different" when compared to the color of two of 
its neighboring pixels. We only compare each pixel in the image with the pixel immediately to the right 
of it and with the pixel immediately below it. If either pixel has a color difference greater than a given 
threshold, then it is "significantly different" and an edge occurs. Note that the right-most column of 
pixels and the bottom-most row of pixels cannot perform this calculation so the final image contains
one less column and one less row than the original image. 
To calculate the "color difference" between two pixels, we treat each pixel as a point on a 3-
dimensional grid and we calculate the distance between the two points using the 3-dimensional 
extension to the Pythagorean Theorem. The distance between (x1, y1, z1) and (x2, y2, z2) is:
√((𝑥1 − 𝑥2)^2 + (𝑦1 − 𝑦2)^2 + (𝑧1 − 𝑧2)^2)
The threshold amount will need to be given, which is an 
integer 0 < threshold < 255. If the color distance between 
the original pixel either of the two neighboring pixels is 
greater than the threshold amount, an edge occurs, and a 
black pixel is put in the resulting image at the location of 
the original pixel. 

# 5. RotateRight90 (width:int) (height:int) (depth:int) (image:(int*int*int) list list): 
This function rotates the image to the right 90 
degrees. The image returned by this function will have 
different dimensions than the original image passed 
in. Here’s the cake rotated right 90 degrees:
Hints
● FlipHorizontal is probably the easiest and thus the first function to attempt. The built-in List.rev function 
that reverses a list makes this one pretty simple. So, use that one to understand working with a list of lists. 
No need to modify the tuple for the color values at each pixel. 
● RotateRight90 also does not have you modify the tuple for the color values. A built-in function makes this 
one easy to complete as well. However, determining your own solution will help for some of the other 
functions. 
● Both Grayscale and Threshold require you to access and change the color values in the tuple for each pixel.
● EdgeDetect is the one that is the hardest. The resulting color values at each pixel relies on accessing the 
color values at two other pixels.


