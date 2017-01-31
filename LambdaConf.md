# Title. What is the title of your proposal?

Image Processing in Haskell

# Introduction. What is this session about?

This session will mostly be structured around Haskell Image Processing library
(HIP). Consequently, it will include introductory concepts from image processing,
examples of basic image transformations using Haskell, guidance on how to write
efficient code using the library and demonstration of some more advanced
techniques like edge detection, binary morphology and/or watermarking.

# Relevancy. Why is this session relevant to a professional software developer?

Computer vision is so common, it is literally used all around us, from
smartphones to self-driving cars. This means, that there are a whole lot of
professional software developers that must be experts in image processing. At
the same time, Haskell is very efficient pure functional language, which allows
for easy parallelization, thus making it a natural match for digital image
processing. Despite this fact, Haskell is rarely used in industry for the
purpose image processing and there are very few success stories in that area.
This was the reason why I developed Haskell Image Processing library and now would like to make sure developers are aware of it's capabilities with hope of it becoming an invaluable
tool in the industry.

# Benefits. How will this session help developers to better accomplish their job?

Every developer at some point has to perform at least some basic image
manipulations, be it resizing an image, adjusting brightness or composing two
images. Almost all technically savvy people, including most of programmers, in
situations like this resort to popular tools like PhotoShop, GIMP, imagemagick,
etc. It doesn't have to be this way, because with the right library at hand,
those tasks can be very straightforward and easy to implement in the language a
programmer is accustomed to, which in the end does save programmer's time and makes the
process reproducible. Therefore, this session will not only be helpful to
professional developers that specialize in image processing, but also to every
programmer that knows how to write Haskell or wants to learn this amazing language.


# Concepts. What concepts will developers learn from the session?

* Basic image transformations: image scaling, rotation, composition, etc.
* Introduction to some common Image Processing algorithms (convolution,
  interpolation, Fourier transformation, etc.)
* Peculiarities of image processing in a pure functional language vs imperative.
* Image processing in parallel.

# Skills. What concrete skills will developers acquire from the session?

After this session developers will be able to use HIP library effectively:

* Read/write images in common formats
* Perform simple image transformations
* Write efficient code

# Outline. Please create a brief outline how you intend to structure the session.

* Brief introduction to what Image Processing is.
* High level description of types utilized in HIP library for color spaces,
  pixels and images.
* Overview of common higher order functions (map, zipWith, fold) specialized to
  images and their utilization in creating ability to treat images as numbers to perform addition,
  multiplication, etc.
* Describe underlying image representation types and how Repa and Vector
  packages are utilized to achieve computation fusion, parallel processing and
  mutation capabilities.
* A few simple examples: read/write, resize, compose images, etc.
* Some common pitfalls and performance penalties if used incorrectly. A real life example.
* Demonstration of edge detection
* Comparison to other available solutions
* Conclusion


# Pitch. What is the main reason developers should come to your session instead of other ones?

Every developer knows how to do a simple image transformation of an image, like
properly scaling it for instance, but not all of them know how to do it
programmatically, and even fewer know how to do it in Haskell. This session will
benefit every developer, regardless of their knowledge of Haskell or Image
Processing.

# Background Requirements.

* Basic knowledge of Haskell
* Understanding of second order functions, function composition.
* Familiarity with type classes, do syntax and simple IO.
* Knowledge of basic concepts from linear algebra.

[LOFP](http://lambdaconf.us/downloads/documents/lambdaconf_slfp.pdf)

