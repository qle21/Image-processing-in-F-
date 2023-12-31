//
// Project Name Image processing in F#
// Project Description : a F# program to perform various operations on images stored in PPM format
// Name Quang Le
// Netid 660670451
// Date 10/28/2023
//

// Work on your solution here
// Do not change the API (module/namespace/function signatures)
// Refer to the PDF for more instructions on how to implement
// each function
// As of now, each function simply takes an image as input
// and returns it as it is

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
    let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
        let total (r, g, b) = 
            let scaleValue = int (0.299 * float r + 0.587 * float g + 0.114 * float b)
            (scaleValue, scaleValue, scaleValue)
        List.map (List.map total) image
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
    let rec Threshold (width:int) 
                      (height:int)
                      (depth:int)
                      (image:(int*int*int) list list)
                      (threshold:int) = 
        let clampValue value = if value > threshold then depth else 0
        List.map (List.map (fun (r, g, b) -> (clampValue r, clampValue g, clampValue b))) image
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
    let rec FlipHorizontal (width:int)
                           (height:int)
                           (depth:int)
                           (image:(int*int*int) list list) = 
        let flipValue = image |> List.map (fun i -> List.rev i)
        flipValue
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
    let rec distance img0 img1 threshold =
        match img0, img1 with
        | h::[],_          -> []
        | h1::t1,h2::t2 -> 
            let (R1, G1, B1) = h1
            let (R2, G2, B2) = List.head t1
            let (R3, G3, B3) = h2
            let sum1 = sqrt(float (R1 - R2)**2.0 + float (G1 - G2)**2.0 + float (B1 - B2)**2.0)
            let sum2 = sqrt(float (R1 - R3)**2.0 + float (G1 - G3)**2.0 + float (B1 - B3)**2.0)
            if (sum1 > float threshold || sum2 > float threshold) then
                (0, 0, 0) :: (distance t1 t2 threshold)
            else 
                (255, 255, 255) :: (distance t1 t2 threshold)

    let rec EdgeDetect (width:int)
                       (height:int)
                       (depth:int)
                       (image:(int*int*int) list list)
                       (threshold:int) = 
        match image with 
        | h::[] -> []
        | h::t -> (distance h (List.head t) threshold) :: (EdgeDetect width height depth t threshold)
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
        let rec customTranspose customImage = 
            match List.head customImage with
            | h :: t -> 
                let firstColumn = customImage |> List.map (fun x -> x.[0])
                let remainingColumns = customImage |> List.map (fun f -> List.tail f)
                (List.rev firstColumn) :: customTranspose remainingColumns
            | _ -> [] 

        customTranspose image